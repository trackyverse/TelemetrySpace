  int<lower = 0> nind;               // number of individuals
  int<lower = 0> nrec;               // number of receivers
  int<lower = 0> ntime;              // number of time steps
  int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
  array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
  vector[nrec] recX;                // receiver locations in east-west direction
  vector[nrec] recY;              // receiver locations in north-south direction
  vector[2] xlim;                    // area bounds east-west
  vector[2] ylim;                    // area boundes north-south
  
  