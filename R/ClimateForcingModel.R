library(rstan)
library(dplyr)
library(abind)

# compile stan model
cf_mod <- stan_model("ClimateForcingModel.stan")

save(cf_mod, file="data/cf_mod.rds")

df <- read.csv("data/dpc-augmented.csv", stringsAsFactors = FALSE)
# TODO: this is probably duplicated work from somewhere
df <- mutate(
  df,
  infections = coalesce(pmax(0, totale_casi - lag(totale_casi)), 0)
)
# TODO: this is a hack, do this a better way...
# Drop the last date as it is NA for all locations.
# Then drop the locations with some NA in data.
df <- filter(df, date != max(as.character(date)))
df <- (
  df
  %>% group_by(denominazione_provincia)
  %>% filter(!any(is.na(air_temp)) & !any(is.na(RH)))
  %>% ungroup
)

X <- abind(split(
  select(df, c('date', 'air_temp', 'RH', 'gdppercapita', 'density')),
  df$denominazione_provincia
), along=3)

I <- abind(split(df$infections, df$denominazione_provincia), along=2)

# TODO: use a correct discretized serial interval distribution
w <- rep(0, nrow(I))
w[1] <- 0.1
w[2] <- 0.2
w[3] <- 0.3
w[4] <- 0.2
w[5] <- 0.1
w[6] <- 0.1

# stan data
cf_data <- list(
  P = ncol(X),
  L = ncol(I),
  T = nrow(I),
  w = w,
  i = I,
  x = X
)

# fit stan model
cf_fit <- sampling(cf_mod, data = cf_data, iter = 400,
                   verbose = TRUE, chains = 4)
