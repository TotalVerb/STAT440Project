library(bayesplot)
library(ggplot2)

load("data/cf_fit.rds")

descr = c("temp", "RH", "GDP per capita", "density")
params <- rstan::extract(cf_fit)

mcmc_areas(
  cf_fit,
  regex_pars = c("beta.+"),
  point_est = "mean"
)

mcmc_areas(
  cf_fit,
  regex_pars = c("theta\\[.+"),
  point_est = "mean",
  transformations = exp
)

# graph 1: histogram of beta
par(mfrow=c(3, 2))
for (i in 1:4) {
  hist(params$beta[,i], main = descr[i], xlab = paste("coef for GDP per capita", descr[i]))
}

# graph 2: evolution of climate-independent R over time, nationally
# Code copied from https://stackoverflow.com/a/51993331/3575047 written by @www.
mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE) {
  dat <- data.frame(y = mean(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)
}
thetadf <- data.frame(params$theta)
thetadf <- pivot_longer(thetadf, cols = colnames(thetadf), names_to = "time", values_to = "logR")
ggplot(thetadf, aes(time, logR)) + geom_smooth(stat = 'summary', fun.data = mean_cl_quantile)
