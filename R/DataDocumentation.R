#' @importFrom stats density dnorm lm na.omit quantile
#' @importFrom utils download.file read.csv write.csv
#' @importFrom dplyr %>% coalesce filter first full_join group_by group_map
#'   group_modify left_join mutate rename select summarize ungroup
#' @importFrom memoise has_cache
#' @importFrom rstan sampling
#' @importFrom abind abind
#' @importFrom bayesplot mcmc_areas mcmc_intervals
#' @importFrom ggplot2 aes element_blank element_text geom_histogram geom_smooth ggplot xlim ylab
#' @importFrom worldmet getMeta
#' @importFrom restatapi get_eurostat_data get_eurostat_dsd
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom R.utils withTimeout
#' @importFrom tidyr pivot_longer
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

# dplyr "globals" and cf_data, cf_fit which come from `load`, throw.error from R.oo
utils::globalVariables(c("RH", "adjustment", "age", "air_temp", "cf_data", "cf_fit", "code", "concept",
                         "dates", "fit", "gdppercapita", "lat", "logR", "province", "region", "sex",
                         "theme", "throw.error", "total_cases", "unit", "values", "values.x", "values.y"))
