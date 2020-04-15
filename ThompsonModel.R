#' Follows the Framework of the Thompson et al paper.
#'

library(EpiEstim)
library(ggplot2)

#' Let's use the Italian Provincial datasets.
#'
dpc_df <- read.csv("data/dpc-covid19-ita-province.csv")
region_names <- unique(dpc_df$denominazione_provincia)


#' Let's just try Rome first.
#'
region <- "Roma"
region_df <- subset(dpc_df, denominazione_provincia == region, select = c("data", "totale_casi"))
names(region_df)[1] <- "date"
names(region_df)[2] <- "cases"

#' Add a "new cases" column for per-day data. This might already be true.
region_df <- within(region_df, new_cases <- c(0, diff(cases)))
region_df

#' Analysis Step 1: Estimate the serial interval
#'
#' Let's just put these placeholders in here.

est_mean_si = 2.5
est_std_si = 1

#' Analysis Step 2: Estimate the reproduction rate.
#'
#'
res_parametric_si <- estimate_R(region_df$new_cases,
      method="parametric_si",
      config = make_config(list(
        mean_si = est_mean_si, 
        std_si = est_std_si)))

plot(res_parametric_si, legend = FALSE)

