library(restatapi)
library(dplyr)
library(worldmet)  # TODO : not done yet

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

# Augment Italy DPC data with weather (TODO) and demographics
# Also remove Sardinia provinces
augmentDPC <- function(dpc, demodata) {
  dpc <- filter(dpc, lat != 0)  # not regional data
  dpc <- filter(dpc, denominazione_regione != "Sardegna")

  df <- left_join(dpc, demodata, by = c("denominazione_provincia" = "name"))
  df <- select(df, c("data", "denominazione_provincia", "lat", "long", "gdppercapita", "density"))
  df
}


# test code
demodata <- getdemodata()
dpc <- read.csv("data/dpc-covid19-ita-province.csv")
df <- augmentDPC(dpc, demodata)

# test: check that no provinces weren't mapped to demographic data
stopifnot(nrow(filter(df, is.na(density))) == 0)
