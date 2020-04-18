#' Return a list containing results of the analysis, sufficient for generating plots.
#' Must be called after `collectData()` and `runMCMC()` and `runThompson()`.
#' @return The saved results of the analysis.
results <- function() {
  if (!file.exists("data/dpc-augmented.csv")) {
    throw.error("Could not find augmented DPC data. Did you `collectData()`?")
  } else if (!file.exists("data/cf_fit.rds") | !file.exists("data/cf_data.rds") | !file.exists("data/cf_dates.rds")) {
    throw.error("Could not find MCMC fit results. Did you `runMCMC()`?")
  } else if (!file.exists("data/thompson_fit.rds")) {
    throw.error("Could not find Thompson fit results. Did you `runThompson()`?")
  }
  dpc <- read.csv("data/dpc-augmented.csv")
  load("data/cf_fit.rds")
  load("data/cf_data.rds")
  load("data/cf_dates.rds")
  load("data/thompson_fit.rds")
  list(
    descr = c("temp", "RH", "GDP per capita", "density"),
    dpc = dpc,
    stanfit = cf_fit,
    standata = cf_data,
    params = rstan::extract(cf_fit),
    dates = dates,
    locations = colnames(cf_data$i),
    thompsonfit = fit
  )
}

