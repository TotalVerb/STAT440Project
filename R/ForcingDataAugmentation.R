library(restatapi)
library(dplyr)
library(worldmet)
library(memoise)

# Obtain mapping of health ministry province names to eurostat codes

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

# Fetch GDP per capita data and population density data for Italian provinces
# Source: Eurostat
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
  density <- droplevels(select(density, c("geo", "values")))
  density$geo <- as.character(density$geo)

  df <- rename(
    full_join(gdppercapita, density, by = 'geo'),
    gdppercapita = values.x,
    density = values.y
  )

  full_join(df, itprovinces, by = c("geo" = "code"))
}

# Augment Italy DPC data with demographics
# Also remove Sardinia provinces
augmentDPCdemo <- function(dpc, demodata) {
  dpc <- filter(dpc, lat != 0)  # not regional data
  dpc <- filter(dpc, denominazione_regione != "Sardegna")

  df <- left_join(dpc, demodata, by = c("denominazione_provincia" = "name"))
  df <- select(df, c("data", "denominazione_provincia", "lat", "long", "gdppercapita", "density", "totale_casi"))
  df
}

closestweatherstation <- function(lat, long) {
  getMeta(lat = lat, lon = long, n = 1)$code[1]
}
closestweatherstationM <- memoise(closestweatherstation)

importNOAAM <- memoise(importNOAA)

# Augment a single group of data points with weather, using just one station code.
augmentsingleweather <- function(group, station) {
  print(station$station[1])
  noaa <- importNOAAM(code = station$station[1], year = 2020)
  print(group)
  print(noaa)
  result <- left_join(group, noaa, by = "date")
  print(result)
  select(result, -c("station"))
}

# Augment a table with date, lat, and long columns with weather data collected closest to the given time.
augmentDPCweather <- function(dpc) {
  # 1. Find the appropriate station code for each latitude and longitude present in data.
  dpc <- mutate(dpc, station = mapply(closestweatherstationM, lat = lat, long = long))
  dpc <- mutate(dpc, date = as.POSIXct(data, format="%Y-%m-%dT%H:%M:%S", tz = "Europe/Rome"))
  dpc <- select(dpc, -c("data"))
  group_by(dpc, station) %>% group_modify(augmentsingleweather) %>% ungroup
}

# Repair total case data to be monotonically increasing, through taking a rolling maximum.
repairtotalcases <- function(dpc) {
  (dpc
   %>% group_by(denominazione_provincia)
   %>% group_modify(~ arrange(.x, by = data) %>% mutate(totale_casi = cummax(totale_casi)))
   %>% ungroup)
}

# test code
demodata <- getdemodata()
dpc <- read.csv("data/dpc-covid19-ita-province.csv")
df <- repairtotalcases(dpc)
df <- augmentDPCdemo(dpc, demodata)
df <- augmentDPCweather(df)
write.csv(df, "data/dpc-augmented.csv")

# test: check that no provinces weren't mapped to demographic data
stopifnot(nrow(filter(df, is.na(density))) == 0)

# test: check no temperature NAs (fails! TODO)
stopifnot(nrow(filter(df, is.na(air_temp))) == 0)
