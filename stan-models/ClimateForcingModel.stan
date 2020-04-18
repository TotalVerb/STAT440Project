data {
  int<lower=0> P;      // number of features
  int<lower=0> L;      // number of locations
  int<lower=0> T;      // number of times
  real<lower=0> w[T];  // serial interval distribution
  int<lower=0> i[T,L]; // number of infections
  real x[T,P,L];       // climate information
  real pop[L];         // population information
}
parameters {
  // Country-wide time-specific log(R) residual, which is autoregressive.
  // Note that theta[1] plays the role of log(R0), i.e. basic reproduction number
  // in average climate forcings and before interventions.
  vector[T] theta;

  // Standard deviation of daily drift in theta.
  real<lower=0> theta_drift;

  // Effect of climate forcings, universal across time and space.
  row_vector[P] beta;

  // Effect of long-run local policies and nature not explained by forcings modeled
  // in beta, universal across time but not space.
  vector[L] lambda;

  // Daily noise of log(R) which is not modeled by above parameters.
  matrix[L,T] epsilon;

  // Per capita number of daily importations into each province.
  real<lower=0> importations;
}
model {
  // prior for base R (very weakly informative), R0 usually estimated around 2.5
  theta_drift ~ gamma(2, 1e-2);
  theta[1] ~ normal(log(2.5), 1);
  theta[2:T] ~ normal(theta[1:(T-1)], theta_drift);

  // prior for beta (weak) [note on unit scale, variation of 1 is high]
  for (p in 1:P) {
    beta[p] ~ normal(0, 0.5);
  }

  // prior for epsilon, theta, lambda (weak)
  for (l in 1:L) {
    lambda[l] ~ normal(0, 0.5);
    for (t in 1:T) {
      epsilon[l,t] ~ normal(0, 0.5);
    }
  }

  // prior for importations per capita [populations are not normalized]
  importations ~ gamma(2, 1e-5);

  // likelihood
  for (l in 1:L) {
    for (t in 1:T) {
      real R = exp(theta[t] + beta * to_vector(x[t, :, l]) + lambda[l] + epsilon[l, t]);
      real serials = 0.0;
      for (s in 1:(t-1)) {
        serials += w[s] * i[t-s, l];
      }
      i[t,l] ~ poisson(R * serials + importations * pop[l]);
    }
  }
}
