#' @importFrom stats density dnorm filter lm na.omit quantile
#' @importFrom utils download.file read.csv write.csv
#' @importFrom dplyr %>% coalesce filter first full_join group_by group_map
#'   group_modify left_join mutate rename ungroup
NULL

#' dpc-covid19-ita-province
#'
#' Raw data per Italian province published by the Italy DPC.
#' @docType data
#' @name dpc-covid19-ita-province
NULL

#' time_series_covid19_confirmed_global
#'
#' Raw data for confirmed case counts various countries published by the Johns
#' Hopkins school. Not used in the analysis by default.
#' @docType data
#' @name time_series_covid19_confirmed_global
NULL

#' time_series_covid19_deaths_global
#'
#' Raw data for death counts from various countries published by the Johns
#' Hopkins school. Not used in the analysis by default.
#' @docType data
#' @name time_series_covid19_deaths_global
NULL

#' time_series_covid19_recovered_global
#'
#' Raw data for recovery counts from various countries published by the Johns
#' Hopkins school. Not used in the analysis by default.
#' @docType data
#' @name time_series_covid19_recovered_global
NULL
