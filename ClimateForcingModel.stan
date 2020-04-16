data {
  int<lower=0> P; // number of features
  int<lower=0> L; // number of locations
  int<lower=0> T; // number of times
  real<lower=0> w[T]; // serial interval distribution
  int<lower=0> i[T,L]; // number of infections
  real x[T,P,L]; // climate information
}
parameters {
  real alpha;                 // constant log(R0)
  row_vector[P] beta;         // linear approximation for log(R) - log(RO)
  matrix[L,T] epsilon;        // residual in data
  real<lower=0> importations; // number of importations into each province on each day
}
model {
  // prior for intercept (very weakly informative), R0 usually estimated around 2.5
  alpha ~ normal(log(2.5), 1);

  // prior for beta (very weak) [note on unit scale, variation of 1 is high]
  for (p in 1:P) {
    beta[p] ~ normal(0, 1);
  }

  // prior for epsilon (also very weak)
  for (l in 1:L) {
    for (t in 1:T) {
      epsilon[l,t] ~ normal(0, 1);
    }
  }

  // likelihood
  for (l in 1:L) {
    for (t in 1:T) {
      real R = exp(alpha + beta * to_vector(x[t, :, l]) + epsilon[l, t]);
      real serials = 0.0;
      for (s in 1:(t-1)) {
        serials += w[s] * i[t-s, l];
      }
      i[t,l] ~ poisson(R * serials + importations);
    }
  }
}
