library(rstan)
library(dplyr)
library(tidyr)
library(abind)

# Compile stan model
cf_mod <- stan_model("ClimateForcingModel.stan")
save(cf_mod, file="data/cf_mod.rds")

df <- read.csv("data/dpc-augmented.csv", stringsAsFactors = FALSE)

# TODO:
# This is probably duplicated work from the Thompson model. Try to merge the
# codepaths that generate this df.
df <- mutate(
  df,
  infections = coalesce(pmax(0, totale_casi - lag(totale_casi)), 0)
)

# Drop the locations with some NA in data (there should be none, TODO: delete
# this code).
df <- (
  df
  %>% group_by(denominazione_provincia)
  %>% filter(!any(is.na(air_temp)) & !any(is.na(RH)))
  %>% ungroup
)

# Stop the analysis on Mar 13
df <- filter(df, date < "2020-03-14")

#' Standardize a single feature.
#' @param series The series to standardize.
#' @return A series with mean 0 and standard deviation 1.
standardize <- function(series) {
  (series - mean(series)) / sd(series)
}

df <- (
  df
  %>% mutate(std_air_temp = standardize(air_temp),
             std_RH = standardize(RH),
             std_gdppercapita = standardize(gdppercapita),
             std_density = standardize(density))
)

# Generate data for climate model.
X <- abind(split(
  select(df, c('std_air_temp', 'std_RH', 'std_gdppercapita', 'std_density')),
  df$denominazione_provincia
), along=3)

I <- abind(split(df$infections, df$denominazione_provincia), along=2)

pop <- abind(df %>% group_by(denominazione_provincia) %>% group_map(~ .x$population[1]), along=1)

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

save(cf_fit, file="data/cf_fit.rds")
save(cf_data, file="data/cf_data.rds")
