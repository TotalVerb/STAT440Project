#' Return a list containing results of the analysis, sufficient for generating plots.
#' Must be called after `collectData()` and `runMCMC()` and `runThompson()`.
#' @return The saved results of the analysis.
#' @export
results <- function() {
  dpc <- read.csv("inst/extdata/dpc-augmented.csv")
  load("inst/extdata/cf_fit.rds")
  load("inst/extdata/cf_data.rds")
  load("inst/extdata/cf_dates.rds")
  list(
    descr = c("temp", "RH", "GDP per capita", "density"),
    dpc = dpc,
    stanfit = cf_fit,
    standata = cf_data,
    params = rstan::extract(cf_fit),
    dates = dates,
    locations = colnames(cf_data$i)
  )
}
