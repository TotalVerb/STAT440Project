#' Follows the Framework of the Thompson et al paper.
#' Using the EpiEstim library to estimate
#'

library(EpiEstim)

#' Using EpiEstim.
#'
#'
#'

confirmed_df <- read.csv("data/time_series_covid19_confirmed_global.csv")
deaths_df <- read.csv("data/time_series_covid19_deaths_global.csv")
recovered_df <- read.csv("data/time_series_covid19_recovered_global.csv")

confirmed_df
