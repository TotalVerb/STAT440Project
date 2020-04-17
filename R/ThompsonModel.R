#' Follows the Framework of the Thompson et al paper, while using OLS to predict R from climate similar to Wang et al.
#' Primarily leverages the EpiEstim R package to perform MCMC and estimate R from time series.
#' Requires some estimate of serial interval, which we simply use values from pervious papers for.
#' 

library(EpiEstim)
library(ggplot2)

#' Using Italian Provincial COVID-19 dataset augmented with climate and population statistics.
dpc_df <- read.csv("data/dpc-augmented.csv")
dpc_df$date <- as.Date(dpc_df$date, format = "%Y-%m-%d")
province_names <- unique(dpc_df$province)

#' Prepare an empty dataframe to collect each province's daily R estimates.
all_provinces_df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(all_provinces_df) <- c("date", "province", "mean_R", "gdppercapita", "density", "air_temp","RH")

#' Compute the reproductive rate estimates for each province in our dataset.
#'
for (province_name in province_names) {
  region_df <- subset(dpc_df, province == province_name)
  
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
  merged_df <- merge(daily_R, region_df, by=c("date", "province"))
  all_provinces_df <- rbind(all_provinces_df, subset(merged_df, select=c("date", "province", "mean_R", "gdppercapita", "density", "air_temp","RH")))
}

#' Analysis Step 3: Linear Regression Against Climate (and other confounding variables).
#'
#' Cutoff on 2020-03-13, since intervention completely changes regime.
all_provinces_df <- subset(all_provinces_df, date <= "2020-03-13")

#' Perform linear regression on expected R against: GDP per capita, Density, Air Temperature, Relative Humidity.
fit <- lm(mean_R ~ gdppercapita + density + air_temp + RH, data=all_provinces_df)
summary(fit)
