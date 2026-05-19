// fixed effect
// detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

// latent variables
// E-W center of activity coordinate - bounds reflect spatial extent
matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
// N-S center of activity coordinate - bounds reflect spatial extent
matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;

