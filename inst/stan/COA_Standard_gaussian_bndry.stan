// Declare data
data {
  int<lower = 0> nind;               // number of individuals
  int<lower = 0> nrec;               // number of receivers
  int<lower = 0> ntime;              // number of time steps
  int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
  array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
  vector[nrec] recX;                // receiver locations in east-west direction
  vector[nrec] recY;              // receiver locations in north-south direction
  // vector[2] xlim;                    // area bounds east-west
  // vector[2] ylim;                    // area boundes north-south
  int<lower = 1>  n_pixels;           // number of valid in-lake pixels
  vector[n_pixels] pix_x;              // pixel centroid E-W coordinates (vector not array
  vector[n_pixels] pix_y;              // pixel centroid N-S coordinates  for vectorization)
}

// Declare parameters
parameters {
  // fixed effects
  real<lower = -7, upper = 7> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  // matrix<lower = xlim[1], upper = xlim[2]> [nind, ntime] sx; // E-W center of activity coordinate - bounds reflect spatial extent
  // matrix<lower = ylim[1], upper = ylim[2]> [nind, ntime] sy; // N-S center of activity coordinate - bounds reflect spatial extent
}

// Pre-compute log detection probabilities for every pixel x receiver combination
// once here rather than recomputing them inside the model loop. Because these
// only depend on parameters (alpha0, alpha1) and data (pix_x, pix_y, recX, recY)
// Stan only recomputes them once per leapfrog step rather than nind x ntime times.
transformed parameters {
  // log p(detect | at pixel k, seen by receiver j)
  array[n_pixels, nrec] real log_det_prob;
  // log(1 - p) — the complement, needed for the binomial miss probability
  array[n_pixels, nrec] real log1m_det_prob;

  // Loop over receivers (small, typically ~10-30) in the outer loop.
  // The distance calculation is then vectorized over all pixels at once
  // using Stan's vector arithmetic — this is the key performance gain
  // over the original pixel x receiver nested loop.
  for (j in 1:nrec) {
    // Squared Euclidean distance from every pixel centroid to receiver j
    // square() and + operate element-wise on vectors — no explicit pixel loop needed
    vector[n_pixels] d2 = square(pix_x - recX[j]) + square(pix_y - recY[j]);

    // Half-normal detection probability at each pixel for this receiver
    // This mirrors the original: p = p0 * exp(-alpha1 * d^2)
    vector[n_pixels] p = inv_logit(alpha0) * exp(-alpha1 * d2);

    // Store log probs pixel by pixel — fmax/fmin guard against log(0) or log1m(1)
    // which would produce -Inf and break the sampler
    for (k in 1:n_pixels) {
      real pk = fmax(1e-10, fmin(1 - 1e-10, p[k]));
      log_det_prob[k, j] = log(pk);
      log1m_det_prob[k, j] = log1m(pk);
    }
  }
}

model {
  // priors
  alpha0 ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

 // CHANGED: likelihood now marginalises over pixel locations rather than
  // conditioning on continuous sx[i,t], sy[i,t].
  //
  // For each individual x time step we compute the log-likelihood of the
  // observed detections at every pixel, then use log_sum_exp to integrate
  // over all pixels. This is mathematically equivalent to:
  //   p(y | params) = sum_k [ p(y | location = k, params) * prior(k) ]
  // where prior(k) = 1/npixels (uniform over valid lake pixels).
  // Taking logs: log p(y) = log_sum_exp(lp) - log(npixels)

  real log_npix = log(n_pixels);  // log uniform prior weight — constant so computed once

  for (i in 1:nind) {
    for (t in 1:ntime) {
      // Log-likelihood contribution of the observed detection array
      // at each candidate pixel location
      vector[n_pixels] lp;

      for (k in 1:n_pixels) {
        real ll = 0.0;
        for (j in 1:nrec) {
          // Manual binomial log-pmf using pre-computed log probs.
          // The binomial coefficient log C(ntrans, y) is the same for all pixels
          // so it cancels in the log_sum_exp and is safely dropped here.
          ll += y[i, t, j] * log_det_prob[k, j]
              + (ntrans - y[i, t, j]) * log1m_det_prob[k, j];
        }
        lp[k] = ll;
      }

      // Accumulate marginal log-likelihood into the joint target
      target += log_sum_exp(lp) - log_npix;
    }
  }
}


generated quantities {
  // Detection probability at a distance of 0
  // Inverse logit of alpha0 - constrains probability b/tw 0 and 1
  real p0 = inv_logit(alpha0); 

  // Standard deviation of the distance-decay function
  // Derived from coefficient specifying distance-related decay in detection prob. 
  // (this is 1 / 2 * sigma^2 = a1) solved to equal sigma - this then is used 
  // in the full model
  real sigma = sqrt(1.0 / (2.0 * alpha1));
  // NEW: posterior pixel weights — the spatial analogue of the old sx/sy posteriors.
  // For each individual x time step, w[k] is the posterior probability that the
  // animal's activity centre was in pixel k, given the observed detections.
  // These can be mapped back to lake coordinates in R to produce utilisation
  // distributions or MAP location estimates.
  array[nind, ntime] int    pixel_mode;             // index of most probable pixel (MAP)
  array[nind, ntime, n_pixels] real pixel_weight;    // full posterior over all pixels

  for (i in 1:nind) {
    for (t in 1:ntime) {
      vector[n_pixels] lp;

      for (k in 1:n_pixels) {
        real ll = 0.0;
        for (j in 1:nrec) {
          ll += y[i, t, j] * log_det_prob[k, j]
              + (ntrans - y[i, t, j]) * log1m_det_prob[k, j];
        }
        lp[k] = ll;
      }

      // Normalise in log space then exponentiate — numerically stable
      vector[n_pixels] w = exp(lp - log_sum_exp(lp));
      pixel_mode[i, t] = sort_indices_desc(w)[1]; // MAP pixel index
      for (k in 1:n_pixels)
        pixel_weight[i, t, k] = w[k];
    }
  }
}
