library(restatapi)
library(dplyr)
library(worldmet)
library(memoise)
library(lubridate)

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

#' Fetches the latest COVID-19 cases data from European Center for Disease Control (ECDC).
#' 
#' @returns None
#' 
fetch_latest_ecdc <- function() {
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  filename <- "data/ecdc-COVID-19-up-to-date.csv"
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
  write.csv(d, filename)
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


#' Fetch Italian provinces data from DPC.
#' 
#' @return None
#'
fetch_latest_dpc <- function() {
  url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
  filename <- "data/dpc-covid19-ita-province.csv"
  fetch_file(url, filename)
  
  d <- read.csv(filename)
  #' Rename Italian column names
  names(d)[names(d) == "data"] <- "date"
  names(d)[names(d) == "denominazione_provincia"] <- "province"
  names(d)[names(d) == "denominazione_regione"] <- "region"
  names(d)[names(d) == "codice_provincia"] <- "province_code"
  names(d)[names(d) == "codice_regione"] <- "region_code"
  names(d)[names(d) == "totale_casi"] <- "total_cases"
  write.csv(d, filename)
}

fetch_latest_ecdc()
fetch_latest_dpc()
fetch_latest_csse()

#' Obtain mapping of health ministry province names to eurostat codes
#'
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
  statname <- c("Valle d'Aosta/Vallée d'Aoste", "Bolzano-Bozen", "Massa-Carrara")
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
getdemodata <- function() {
  itprovinces <- getprovincelist()
  
  gdppercapita <- filter(get_eurostat_data(
    "nama_10r_3gdp",
    date_filter="2017",
    filters=itprovinces$code
  ), unit == "EUR_HAB")
  gdppercapita <- select(gdppercapita, c("geo", "values"))
  gdppercapita$geo <- as.character(gdppercapita$geo)
  
  density <- get_eurostat_data(
    "demo_r_d3dens",
    date_filter="2017",
    filters=itprovinces$code
  )
  density <- select(density, c("geo", "values"))
  density$geo <- as.character(density$geo)
  
  population <- filter(get_eurostat_data(
    "demo_r_pjangrp3",
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

# Augment Italy DPC data with demographics
# Also remove Sardinia provinces
augmentDPCdemo <- function(dpc, demodata) {
  dpc <- filter(dpc, lat != 0)  # not regional data
  dpc <- filter(dpc, region != "Sardegna")
  
  df <- left_join(dpc, demodata, by = c("province" = "name"))
  df <- select(df, c("data", "province", "lat", "long", "gdppercapita", "density", "cases", "population"))
  df
}

#' Get the closest weather station to a latitude longitude point.
#' 
#' @param lat Latitude
#' @param long Longitude
#' @param n 
#'
closestweatherstations <- function(lat, long, n) {
  f(lat = lat, lon = long, n = n)$code
}
closestweatherstationsM <- memoise(closestweatherstations)

importNOAAM <- memoise(importNOAA)

# Augment a single group of data points with weather, using a list of station codes.
augmentsingleweather <- function(group, station) {
  fallbacks <- strsplit(station$station[1], ",")[[1]]
  print(paste("Looking up weather for", fallbacks))
  noaa <- importNOAAM(code = fallbacks, year = 2020)
  noaa <- noaa %>% group_by(date) %>% summarize(
    air_temp = first(na.omit(air_temp)), RH = first(na.omit(RH))
  )
  left_join(group, noaa, by = "date")
}

# Augment a table with date, lat, and long columns with weather data collected closest to the given time.
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
  dpc <- mutate(dpc, date = as.POSIXct(data, format="%Y-%m-%dT%H:%M:%S", tz = "Europe/Rome"))
  dpc <- select(dpc, -c("data"))
  group_by(dpc, station) %>% group_modify(augmentsingleweather) %>% ungroup
}

# Repair total case data to be monotonically increasing, through taking a rolling maximum.
# Also drop the last date as it is likely to have no weather data for each location.
repairtotalcases <- function(dpc) {
  (dpc
   %>% group_by(province)
   %>% group_modify(~ arrange(.x, by = data) %>% mutate(total_cases = cummax(total_cases)))
   %>% ungroup
   %>% filter(data != max(as.character(data)))
  )
}

# test code
demodata <- getdemodata()
dpc <- read.csv("data/dpc-covid19-ita-province.csv")
df <- repairtotalcases(dpc)
df <- augmentDPCdemo(df, demodata)
df <- augmentDPCweather(df)
write.csv(demodata, "data/demodata.csv")
write.csv(df, "data/dpc-augmented.csv")

# test: check that no provinces weren't mapped to demographic data
stopifnot(nrow(filter(df, is.na(density))) == 0)

# test: check no temperature NAs
stopifnot(nrow(filter(df, is.na(air_temp))) == 0)
