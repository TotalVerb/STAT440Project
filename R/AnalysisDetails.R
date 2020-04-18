#' Return a list containing results of the analysis, sufficient for generating plots.
#' Must be called after `collectData()` and `runMCMC()` and `runThompson()`.
#' @return The saved results of the analysis.
#' @export
results <- function() {
  if (!file.exists("inst/extdata/dpc-augmented.csv")) {
    throw.error("Could not find augmented DPC data. Did you `collectData()`?")
  } else if (!file.exists("inst/extdata/cf_fit.rds") | !file.exists("inst/extdata/cf_data.rds") | !file.exists("inst/extdata/cf_dates.rds")) {
    throw.error("Could not find MCMC fit results. Did you `runMCMC()`?")
  } else if (!file.exists("inst/extdata/thompson_fit.rds")) {
    throw.error("Could not find Thompson fit results. Did you `runThompson()`?")
  }
  dpc <- read.csv("inst/extdata/dpc-augmented.csv")
  load("inst/extdata/cf_fit.rds")
  load("inst/extdata/cf_data.rds")
  load("inst/extdata/cf_dates.rds")
  load("inst/extdata/thompson_fit.rds")
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
