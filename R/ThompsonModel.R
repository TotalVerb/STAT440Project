#' Follows the Framework of the Thompson et al paper.
#'

library(EpiEstim)
library(ggplot2)

#' Let's use the Italian Provincial datasets.
#'
dpc_df <- read.csv("data/dpc-covid19-ita-province.csv")
province_names <- unique(dpc_df$province)

all_provinces_df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(all_provinces_df) <- c("date", "province", "mean_R", "gdppercapita", "density", "air_temp","RH")

augmented_df <- read.csv("data/dpc-augmented.csv")
variables_df <- subset(augmented_df, select=c("denominazione_provincia", "gdppercapita", "density", "date", "air_temp", "RH"))
names(variables_df)[names(variables_df) == "denominazione_provincia"] <- "province"
variables_df$date <- as.Date(variables_df$date, format = "%Y-%m-%d")

for (province_name in province_names) {
  region_df <- subset(dpc_df, province == province_name, select = c("date", "total_cases"))
  
  #' Add a "new cases" column for per-day data.
  region_df <- within(region_df, new_cases <- c(0, diff(total_cases)))
  region_df$new_cases[region_df$new_cases<0] <- 0
  region_df$date <- as.Date(region_df$date, format = "%Y-%m-%d")
  
  #' Analysis Step 1: Estimate the serial interval
  #'
  #' We have determined that it is difficult to find the line-list data for COVID-19, and thus will be using
  #' the mean and standard deviation 
  est_mean_si = 4.7
  est_std_si = 2.9
  
  #' Analysis Step 2: Estimate the reproduction rate.
  #'
  res_parametric_si <- estimate_R(region_df$new_cases,
        method="parametric_si",
        config = make_config(list(
          mean_si = est_mean_si, 
          std_si = est_std_si)))
  
  #' res_parametric_si dataframe R that is based on aggregating on a weekly sliding window.
  #' Therefore, the length of the date list and the mean list is off by 7.
  l = length(region_df$date)
  daily_R <- data.frame(date=region_df$date[1:(l-7)], mean_R=res_parametric_si$R$`Mean(R)`, province=province_name)
  
  #' Analysis Step 3: Linear Regression Against Climate (and other confounding variables).
  #'
  merged_df <- merge(daily_R, variables_df, by=c("date", "province"))
  all_provinces_df <- rbind(all_provinces_df, merged_df)
}

#' Cutoff on 2020-03-13, since intervention completely changes regime.
all_provinces_df <- subset(all_provinces_df, date <= "2020-03-13")
fit <- lm(mean_R ~ gdppercapita + density + air_temp + RH, data=all_provinces_df)
summary(fit)

# Produce good plots for the 
#