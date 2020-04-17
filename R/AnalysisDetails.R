#' Description of the seasonal forcing features used in the analysis.
descr = c("temp", "RH", "GDP per capita", "density")

#' Create labels for dates appropriate for use in a chart, by removing the year.
#' @param range The date range.
#' @return A vector of strings to use as labels for a chart.
removeyear <- function(range) {
  substr(toString(as.Date(range)), 5, 10)
}

#' Return a list containing results of the analysis, sufficient for generating plots.
#' Must be called after `collectData()` and `runMCMC()`.
#' @return The saved results of the analysis.
results <- function() {
  if (!file.exists("data/dpc-augmented.csv")) {
    throw.error("Could not find augmented DPC data. Did you `collectData()`?")
  } else if (!file.exists("data/cf_fit.rds") | !file.exists("data/cf_data.rds")) {
    throw.error("Could not find MCMC fit results. Did you `runMCMC()`?")
  }
  dpc <- read.csv("data/dpc-augmented.csv")
  load("data/cf_fit.rds")
  load("data/cf_data.rds")
  list(
    dpc = dpc,
    stanfit = cf_fit,
    standata = cf_data,
  )
}

