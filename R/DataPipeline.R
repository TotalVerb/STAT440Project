library(restatapi)
library(dplyr)
library(worldmet)
library(memoise)
library(lubridate)
library(R.utils)

#' Fetches a raw file from url, and saves it to a filename.
#' For proper style, filename string should begin with "data/".
#' Throws exception on failure.
#'
#' @param url The URL of the file to download.
#' @param filename The filename that the file is saved to.
#'
#' @return None
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

#' Fetches latest data from John Hopkins University CSSE.
#'
#' @return None
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


#' Fetch Italian provinces data from DPC. This file is never written as it contains non-ASCII characters.
#'
#' @return None
#'
fetch_latest_dpc <- function() {
  url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
  filename <- tempfile()
  fetch_file(url, filename)

  d <- read.csv(filename)
  #' Rename Italian column names
  names(d)[names(d) == "data"] <- "date"
  names(d)[names(d) == "denominazione_provincia"] <- "province"
  names(d)[names(d) == "denominazione_regione"] <- "region"
  names(d)[names(d) == "codice_provincia"] <- "province_code"
  names(d)[names(d) == "codice_regione"] <- "region_code"
  names(d)[names(d) == "totale_casi"] <- "total_cases"
  d
}

#' Obtain mapping of health ministry province names to eurostat codes
#'
#' @return Dataframe of italian provinces with columns "name", "code".
getprovincelist <- function() {
  # The island of Sardinia was reorganized between collection of the demographic data and
  # the health ministry's collection of COVID-19 data, so demographic data does not map
  # cleanly. Because Sardinia is an island and the situation may be unique, we have just
  # excluded it from the analysis.
  sardiniaprefix <- "ITG2"

  dsd <- get_eurostat_dsd("nama_10r_3gdp")
  itprovinces <- select(
    filter(
      filter(dsd, concept == "GEO"),
      startsWith(code, "IT") &
        nchar(code) == 5 &
        code != "ITZZZ" &
        !startsWith(code, sardiniaprefix)
    ),
    c("code", "name")
  )
  # 3 provinces have names which do not match the ones the health ministry uses;
  # so we rename them to match
  statname <- c("Valle d'Aosta/Vall\u00E9e d'Aoste", "Bolzano-Bozen", "Massa-Carrara")
  healthname <- c("Aosta", "Bolzano", "Massa Carrara")
  itprovinces <- left_join(
    itprovinces,
    data.frame(statname, healthname, stringsAsFactors = FALSE),
    by = c("name" = "statname")
  )
  itprovinces$name <- coalesce(itprovinces$healthname, itprovinces$name)
  select(itprovinces, c("code", "name"))
}

#' Fetch GDP per capita data and population density data for Italian provinces
#' Source: Eurostat
#'
#' @return Eurostat dataframe with columns "name", "gdppercapita", "density", "population".
getdemodata <- function() {
  itprovinces <- getprovincelist()

  #' Get GDP/capita data from Eurostat.
  #'
  gdppercapita <- filter(get_eurostat_data(
    "nama_10r_3gdp", # Specific column code for GDP per capital from Eurostat documentation.
    date_filter="2017",
    filters=itprovinces$code
  ), unit == "EUR_HAB")
  gdppercapita <- select(gdppercapita, c("geo", "values"))
  gdppercapita$geo <- as.character(gdppercapita$geo)

  #' Get population density data from Eurostat.
  density <- get_eurostat_data(
    "demo_r_d3dens", # Specific column code for density from Eurostat documentation.
    date_filter="2017",
    filters=itprovinces$code
  )
  density <- select(density, c("geo", "values"))
  density$geo <- as.character(density$geo)

  #' Get population data from Eurostat.
  population <- filter(get_eurostat_data(
    "demo_r_pjangrp3", # Specific column code for population from Eurostat documentation.
    date_filter="2017",
    filters=itprovinces$code
  ), sex == "T" & age == "TOTAL")
  population <- select(population, c("geo", "values"))
  population$geo <- as.character(population$geo)

  df <- rename(
    rename(
      gdppercapita %>%
        full_join(density, by = 'geo'),
      gdppercapita = values.x,
      density = values.y
    ) %>%
      full_join(population, by = 'geo'),
    population = values
  )

  full_join(df, itprovinces, by = c("geo" = "code"))
}

#' Augment Italy DPC data with demographics, and also removes Sardinia provinces
#'
#' @param dpc Italian per province timeseries data.
#' @param demodata Dataframe sourced from Eurostat that contains GDP per Capita and Density information on Italian regions.
#'
#' @return Augmented dataframe that contains both COVID-19 cases timeseries and eurostats.
augmentDPCdemo <- function(dpc, demodata) {
  dpc <- filter(dpc, lat != 0)  # not regional data
  dpc <- filter(dpc, region != "Sardegna")

  df <- left_join(dpc, demodata, by = c("province" = "name"))
  df <- select(df, c("date", "province", "lat", "long", "gdppercapita", "density", "total_cases", "new_cases", "population"))
  df
}

#' Get the closest weather stations to a latitude longitude point.
#'
#' @param lat Latitude
#' @param long Longitude
#' @param n Number of weather stations to get.
#'
#' @return The codes of the closest weather stations in a list format.
closestweatherstations <- function(lat, long, n) {
  print(paste("Querying for", lat, long))
  getMeta(lat = lat, lon = long, n = n)$code
}

#' Memoized equivalent of functions, such that repeated function calls are efficient.
#'
#' @param lat Latitude
#' @param long Longitude
#' @param n Number of weather stations to get.
#'
#' @return The codes of the closest weather stations in a list format.
closestweatherstationsM <- memoise(closestweatherstations)

#' ImportNOAA is a function exposed in "worldmet" library. Gets data from weather stations.
#'
#' @param code Station codes to get data from.
#' @param year Years to get data for.
#' @param hourly If TRUE, aggregate hourly.
#' @param precip If TRUE, include precipitation.
#' @param PWC If TRUE, include text description.
#' @param parallel If TRUE, use multiple cores.
#' @param quiet If TRUE, do not produce output.
#' @param path If provided, store results in a file.
#'
#' @return The codes of the closest weather stations in a list format.
importNOAAM <- memoise(importNOAA)

#' Augment a single group of data points with weather, using a list of station codes.
#' Query each station code for air temperature, dewpoint, and relative humidity, taking the closest result that is not NA.
#'
#' @param group A dataframe groupby object on a particular province, which we are pulling weather data for.
#' @param station A comma separated string of station codes, which are used by importNOAA to query for weather data from those stations.
#'
#' @return Augmented dataframe groupby object which now contains "air_temp", "RH", "dewpoint" as columns.
augmentsingleweather <- function(group, station) {
  fallbacks <- strsplit(station$station[1], ",")[[1]]
  if (has_cache(importNOAAM)(code = fallbacks, year = 2020)) {
    cat('.')
  } else {
    print(paste("Looking up weather for", paste(fallbacks, sep = ', ', collapse = ', ')))
  }
  noaa <- importNOAAM(code = fallbacks, year = 2020)
  noaa <- noaa %>% group_by(date) %>% summarize(
    air_temp = first(na.omit(air_temp)), RH = first(na.omit(RH))
  )
  left_join(group, noaa, by = "date")
}


#' Augment a table with date, lat, and long columns with weather data collected
#' closest to the given time. **Warning**: This function will be very slow,
#' since it queries the NOAA database often. Also, it may hang indefinitely due
#' to a presumed bug in the worldmet package. Multiple attempts may be
#' necessary. Use the `robust` utility function to automate the re-running of
#' this function.
#'
#' @param dpc Dataframe of DPC per province time series case data. Required columns: "date", "lat", "long".
#'
#' @return Transformed dataframe with new columns added: "air_temp", "dewpoint", "RH".
augmentDPCweather <- function(dpc) {
  # 1. Find the appropriate station code for each latitude and longitude present in data.
  stations = mapply(
    closestweatherstationsM,
    lat = dpc$lat,
    long = dpc$long,
    MoreArgs = list(n = 5)
  )
  dpc <- mutate(
    dpc,
    station = apply(stations, 2, function (sts) { paste(sts, collapse=",", sep=",") })
  )
  dpc <- mutate(dpc, date = as.POSIXct(date, format="%Y-%m-%dT%H:%M:%S", tz = "Europe/Rome"))
  group_by(dpc, station) %>% group_modify(augmentsingleweather) %>% ungroup
}

#' Convert single-argument function f to a robust version. The robust version of
#' the function will be run over and over on the same input until successful.
#' Each individual run is not allowed to take longer than 60s (or a different
#' specified timeout).
#' @param f The function to make robust (must accept one argument).
#' @param timeout The number of seconds each individual run is allowed to use.
#' @return A robust version of `f`.
robust <- function(f, timeout = 60) {
  # Code modified from https://stackoverflow.com/a/20770711/3575047 by Vincent
  # Zoonekynd.
  function(arg) {
    attempt <- 1
    result <- NULL
    while (is.null(result) && attempt <= 1000) {
      print(paste("Attempt", attempt))
      attempt <- attempt + 1
      try(
        result <- withTimeout(f(arg), timeout = timeout)
      )
    }
    result
  }
}

#' Repair total case data to be monotonically increasing, through taking a rolling maximum.
#' Drop the last 4 dates as it is likely to have no weather data for each location.
#' Add new_cases column which contains the number of new cases each day.
#'
#' @param dpc Dataframe of DPC per province timeseries. Requires the columns: "date", "province", "total_cases".
#'
#' @return Transformed dataframe with modified "total_cases" column, where the column is now monotonically increasing.
transform_total_cases <- function(dpc) {
  (dpc
   %>% group_by(province)
   %>% group_modify(~ arrange(.x, by=date) %>% mutate(total_cases = cummax(total_cases)))
   %>% mutate(new_cases = c(0, diff(total_cases)))
   %>% ungroup
   %>% filter(date != max(as.character(date)))
   %>% filter(date != max(as.character(date)))
   %>% filter(date != max(as.character(date)))
   %>% filter(date != max(as.character(date)))
  )
}

#' Augments the Italian per province data with GDP, weather, density data.
#' Produces a file data/dpc-augmented.csv which contains the required data for
#' this analysis.
#' @param rewriteall Attempt to refresh all files even if already exist. If
#'   TRUE, this function may need to be called multiple times due to `worldmet`
#'   issues.
collectData <- function(rewriteall = FALSE) {
  fetch_latest_csse()

  if (rewriteall | !file.exists("data/dpc-augmented.csv")) {
    demodata <- getdemodata()
    dpc <- fetch_latest_dpc()
    df <- transform_total_cases(dpc)
    df <- augmentDPCdemo(df, demodata)
    df <- robust(augmentDPCweather, timeout=120)(df)
    write.csv(df, "data/dpc-augmented.csv")

    # test: check that no provinces weren't mapped to demographic data
    stopifnot(nrow(filter(df, is.na(density))) == 0)

    # test: check no temperature NAs
    stopifnot(nrow(filter(df, is.na(air_temp))) == 0)
  }
}
