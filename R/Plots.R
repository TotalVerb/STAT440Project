library(bayesplot)
library(ggplot2)
library(gtable)

#' Given the Bayesian posterior stanfit object, plot the MCMC confidence for
#' beta.
#'
#' @param cf_fit The stanfit object containing the Bayesian posterior samples.
#' @return A plot of location of standardized beta parameters, with 50%
#'   confidence interval highlighted.
plot_beta_confidence <- function(cf_fit) {
  betadescr <- paste(descr, sep = ", ", collapse = ", ")
  (
    mcmc_areas(
      cf_fit,
      regex_pars = c("beta.+"),
      point_est = "mean"
    ) + ggplot2::labs(title = "Posterior of β, magnitude of climate forcings",
                      subtitle = paste("Each standardized; in order:", betadescr)
  )
}

#' Given the Bayesian posterior stanfit object, plot the MCMC confidence for
#' lambda
#'
#' @param cf_fit The stanfit object containing the Bayesian posterior samples.
#' @return A plot of location of standardized beta parameters, with 50%
#'   confidence interval highlighted.
plot_lambda_confidence <- function(cf_fit, start, end) {
  (
    mcmc_intervals(
      cf_fit,
      pars = paste("lambda[", start:end, "]", sep = ""),
      point_est = "mean"
    )
    + ggplot2::labs(
      title = "Posterior of λ, location-dependent adjustment to log(R)",
      subtitle = paste(
        "Mean, 50% and 90% confidence intervals for locations",
        start,
        "through",
        end
      ),
      x = "Local adjustment to log(R)"
    )
    + ggplot2::theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())
    + xlim(-2, 2)
  )
}

#' Given a series, return the mean and two specified quantiles.
#' Code copied from https://stackoverflow.com/a/51993331/3575047 written by @www.
#'
#' @param x The series to process.
#' @param q The limits of confidence (e.g. 0.1 to 0.9 for 80%)
#' @param na.rm Whether to ignore NA values.
#' @return A gg-plot geom_smooth-usable summary of the mean and quantiles.
mean_cl_quantile <- function(x,
                             q = c(0.1, 0.9),
                             na.rm = TRUE) {
  dat <- data.frame(
    y = mean(x, na.rm = na.rm),
    ymin = quantile(x, probs = q[1], na.rm = na.rm),
    ymax = quantile(x, probs = q[2], na.rm = na.rm)
  )
  return(dat)
}

#' Given the Bayesian posterior samples and initial data, return the modeled
#' time-varying R independent of climate forcings for the entire country.
#'
#' @param params Samples of posterior distribution of parameters.
#' @param cf_data Observed data.
#' @return A ggplot2 time series of estimated independent R with 80% confidence.
plot_independentR <- function(params, cf_data) {
  thetadf <- data.frame(params$theta)
  colnames(thetadf) <- dates
  thetadf <-
    pivot_longer(
      thetadf,
      cols = colnames(thetadf),
      names_to = "date",
      values_to = "logR"
    )
  (
    ggplot(thetadf, aes(date, exp(logR), group = 1))
    + geom_smooth(stat = 'summary', fun.data = mean_cl_quantile)
    + ylab("R")
    + ggplot2::labs(
      title = "Climate-independent basic R, by date",
      subtitle = "Mean and 80% confidence interval"
    )
    + theme(axis.text.x = element_text(angle = 90))
  )
}

#' Given the Bayesian posterior samples and initial data, return the modeled
#' time-varying R for location with the given index.
#'
#' @param params Samples of posterior distribution of parameters.
#' @param cf_data Observed data.
#' @param loc Index of the location to examine.
#' @param locname Name of the location, to label axis.
#' @return A ggplot2 time series of estimated R with 80% confidence.
plot_locationR <- function(params, cf_data, loc, locname) {
  thetadf <- data.frame(params$theta +
                          params$lambda[loc] +
                          params$epsilon[, loc, ] +
                          params$beta %*% t(cf_data$x[, , loc]))
  colnames(thetadf) <- dates
  thetadf <-
    pivot_longer(
      thetadf,
      cols = colnames(thetadf),
      names_to = "date",
      values_to = "logR"
    )
  (
    ggplot(thetadf, aes(date, exp(logR), group = 1))
    + geom_smooth(stat = 'summary', fun.data = mean_cl_quantile)
    + ylab("R")
    + ggplot2::labs(
      title = paste("Estimated R in", locname, "by date"),
      subtitle = "Mean and 80% confidence interval"
    )
    + theme(axis.text.x = element_text(angle = 90))
  )
}

#' Plot a histogram of all samples of epsilon across all times and locations.
#'
#' @param params Samples of posterior distribution of parameters.
#' @return A histogram of all samples of epsilon.
plot_epsilon_dispersion <- function(params) {
  df <- data.frame(adjustment = as.vector(params$epsilon))
  ggplot(df, aes(adjustment)) +
    geom_histogram() +
    ggplot2::labs(title = "ε, idiosyncratic adjustment to log(R)",
                  subtitle = "Aggregated across all locations and times",
                  xlab = "difference in log(R)")
}

# TODO: move these to the Rmarkdown file

plot_beta_confidence(cf_fit)
plot_lambda_confidence(cf_fit, 1, 34)
plot_lambda_confidence(cf_fit, 35, 68)
plot_lambda_confidence(cf_fit, 69, 102)
plot_independentR(params, cf_data)
plot_locationR(params, cf_data, 13, "Bergamo")
plot_locationR(params, cf_data, 55, "Milano")
plot_locationR(params, cf_data, 79, "Rome")
plot_locationR(params, cf_data, 96, "Venezia")
plot_locationR(params, cf_data, 99, "Verona")
plot_epsilon_dispersion(params)
