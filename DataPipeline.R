library(lubridate)

#' Fetches a raw file from url.
#'
fetch_file <- function(url, filename) {
  tryCatch({
    code <- download.file(url, filename)
    if (code != 0) {
      stop("Error downloading file")
    }
  },
  error = function(e) {
    stop(sprintf("Error downloading file '%s': %s",
                 url, e$message))
  })
}

#' Fetches the latest COVID-19 cases data from ECDC.
#'
fetch_latest_ecdc <- function() {
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  file_name <- "data/ecdc-COVID-19-up-to-date.csv"
  fetch_file(url, filename)
  
  #' Quick cleanup on the column titles.
  #'
  d <- read.csv(filename, stringsAsFactors = FALSE)
  d$t <- lubridate::decimal_date(as.Date(d$dateRep, format = "%d/%m/%Y"))
  d <- d[order(d$'countriesAndTerritories', d$t, decreasing = FALSE), ]
  names(d)[names(d) == "countriesAndTerritories"] <- "Countries.and.territories"
  names(d)[names(d) == "deaths"] <- "Deaths"
  names(d)[names(d) == "cases"] <- "Cases"
  names(d)[names(d) == "dateRep"] <- "DateRep"
  saveRDS(d, "data/ecdc-COVID-19-up-to-date.rds")
}
fetch_latest_ecdc()

#' Fetches latest data from John Hopkins University CSSE.
#'
fetch_latest_csse <- function() {
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  filename <- "data/time_series_covid19_confirmed_global.csv"
  fetch_file(url, filename)
  
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  filename <- "data/time_series_covid19_deaths_global.csv"
  fetch_file(url, filename)
  
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  filename <- "data/time_series_covid19_recovered_global.csv"
  fetch_file(url, filename)
}
fetch_latest_csse()

#' Fetch Italian provinces data.
#'
fetch_latest_dpc <- function() {
  url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
  filename <- "data/dpc-covid19-ita-province.csv"
  fetch_file(url, filename)
}
fetch_latest_dpc()
