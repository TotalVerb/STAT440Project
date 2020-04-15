data {
  int<lower=0> P; // number of features
  int<lower=0> L; // number of locations
  int<lower=0> T; // number of times
  real<lower=0> w[T]; // serial interval distribution
  int<lower=0> i[L,T];
}
parameters {
  row_vector[P] beta;
  matrix[L,P] x;
  matrix[L,T] epsilon;  // residual
}
model {
  for (l in 1:L) {
    for (t in 1:T) {
      real lambda = beta * x[l, :]' + epsilon[l, t];
      real serials = 0.0;
      for (s in 1:(t-1)) {
        serials += w[s] * i[l, t-s];
      }
      i[l,t] ~ poisson(lambda * serials);
    }
  }
}
