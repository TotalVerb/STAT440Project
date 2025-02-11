library(rstan)
library(dplyr)
library(tidyr)
library(abind)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#' Compiled STAN model of the seasonal forcing model described in accompanying
#' report.
cf_mod <- stan_model("stan-models/ClimateForcingModel.stan")

#' Discretize the serial interval distribution of 4.7 std. dev 2.9, used in the
#' CMMID paper https://epiforecasts.io/covid/methods.html
#' @param n The number of discrete dates in the future to compute a serial
#'   interval density for.
#' @param mean The estimated mean of the serial interval.
#' @param sd The estimated standard distribution of the serial interval.
#' @return A vector whose index corresponds to the number of days in the future,
#'   and whose value corresponds to the density of the serial interval
#'   distribution.
getSIdist <- function(n, mean = 4.7, sd = 2.9) {
  w <- dnorm(1:n, mean, sd)
  w / sum(w)
}

#' Compile the stan model, run MCMC, and save results.
#' @param cutoff The cutoff date (default: Mar 14).
#' @export
runMCMC <- function(cutoff = "2020-03-14") {
  df <- read.csv("inst/extdata/dpc-augmented.csv", stringsAsFactors = FALSE)

  # Stop the analysis on Mar 13
  df <- filter(df, date < cutoff)

  # Standardize each of the columns we will use.
  df <- (
    df
    %>% mutate(std_air_temp = scale(air_temp),
               std_RH = scale(RH),
               std_gdppercapita = scale(gdppercapita),
               std_density = scale(density))
  )

  # Generate data for climate model.
  X <- abind(split(
    select(df, c('std_air_temp', 'std_RH', 'std_gdppercapita', 'std_density')),
    df$province
  ), along=3)

  I <- abind(split(df$new_cases, df$province), along=2)

  pop <- abind(df %>% group_by(province) %>% group_map(~ .x$population[1]), along=1)

  w <- getSIdist(nrow(I))

  # stan data
  cf_data <- list(
    P = ncol(X),
    L = ncol(I),
    T = nrow(I),
    w = w,
    i = I,
    x = as.array(X),
    pop = pop
  )

  # fit stan model
  cf_fit <- sampling(cf_mod, data = cf_data, verbose = TRUE)

  dates <- unique(df$date)
  save(dates, file="inst/extdata/cf_dates.rds")
  save(cf_fit, file="inst/extdata/cf_fit.rds")
  save(cf_data, file="inst/extdata/cf_data.rds")

  print("Execution completed. Read a summary object of results using `results()`.")
}
