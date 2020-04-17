library(bayesplot)
library(ggplot2)
library(gtable)

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

#' Create labels for dates appropriate for use in a chart, by removing the year.
#' @param range The date range.
#' @return A vector of strings to use as labels for a chart.
removeyear <- function(range) {
  substr(lapply(as.Date(range), toString), 6, 10)
}

#' Given the Bayesian posterior stanfit object, plot the MCMC confidence for
#' beta.
#'
#' @param res Output of `results()` containing results of the analysis.
#' @return A plot of location of standardized beta parameters, with 50%
#'   confidence interval highlighted.
plot_beta_confidence <- function(res) {
  betadescr <- paste(res$descr, sep = ", ", collapse = ", ")
  (
    mcmc_areas(
      res$stanfit,
      regex_pars = c("beta.+"),
      point_est = "mean"
    ) + ggplot2::labs(title = "Posterior of β, magnitude of climate forcings",
                      subtitle = paste("Each standardized; in order:", betadescr))
  )
}

#' Given the Bayesian posterior stanfit object, plot the MCMC confidence for
#' lambda
#'
#' @param res Output of `results()` containing results of the analysis.
#' @return A plot of location of standardized beta parameters, with 50%
#'   confidence interval highlighted.
plot_lambda_confidence <- function(res, start, end) {
  (
    mcmc_intervals(
      res$stanfit,
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

#' Given the Bayesian posterior samples and initial data, return the modeled
#' time-varying R independent of climate forcings for the entire country.
#'
#' @param res Samples of posterior distribution of parameters.
#' @return A ggplot2 time series of estimated independent R with 80% confidence.
plot_independentR <- function(res) {
  thetadf <- data.frame(res$params$theta)
  colnames(thetadf) <- removeyear(res$dates)
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
#' @param res Samples of posterior distribution of parameters.
#' @param loc Index of the location to examine.
#' @return A ggplot2 time series of estimated R with 80% confidence.
plot_locationR <- function(params, loc) {
  thetadf <- data.frame(res$params$theta +
                          res$params$lambda[loc] +
                          res$params$epsilon[, loc, ] +
                          res$params$beta %*% t(res$standata$x[, , loc]))
  colnames(thetadf) <- removeyear(res$dates)
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
      title = paste("Estimated R in", res$locations[loc], "by date"),
      subtitle = "Mean and 80% confidence interval"
    )
    + theme(axis.text.x = element_text(angle = 90))
  )
}

#' Plot a histogram of all samples of epsilon across all times and locations.
#'
#' @param res Samples of posterior distribution of parameters.
#' @return A histogram of all samples of epsilon.
plot_epsilon_dispersion <- function(res) {
  df <- data.frame(adjustment = as.vector(res$epsilon))
  ggplot(df, aes(adjustment)) +
    geom_histogram() +
    ggplot2::labs(title = "ε, idiosyncratic adjustment to log(R)",
                  subtitle = "Aggregated across all locations and times",
                  xlab = "difference in log(R)")
}

# TODO: move these to the Rmarkdown file
generate_plots <- function() {
  res <- results()
  plot_beta_confidence(res)
  plot_lambda_confidence(res, 1, 34)
  plot_lambda_confidence(res, 35, 68)
  plot_lambda_confidence(res, 69, 102)
  plot_independentR(res)
  plot_locationR(res, 13)
  plot_locationR(res, 55)
  plot_locationR(res, 79)
  plot_locationR(res, 96)
  plot_locationR(res, 99)
  plot_epsilon_dispersion(res)
}
