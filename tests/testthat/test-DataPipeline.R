library(dplyr)

#' Create a mock DPC-like dataframe for testing data pipeline.
createmockdpc <- function() {
  province <- c(rep("A", 9), rep("B", 9))
  region <- rep("C", 18)
  lat <- c(rep(1.0, 9), rep(2.0, 9))
  long <- c(rep(1.0, 9), rep(2.0, 9))
  dateraw <- c(paste("2020-01-0", 1:9, sep=""))
  date <- c(dateraw, dateraw)
  total_cases <- c(0, 2, 3, 5, 9, 12, 15, 19, 20,
                   3, 5, 3, 9, 2, 5, 100, 120, 140)
  df <- data.frame(province, region, date, total_cases, lat, long)
}

test_that("transform total cases works", {
  df <- createmockdpc()
  df <- transform_total_cases(df)
  expect_equal(nrow(df), 10)
  expect_equal(df$total_cases, c(0, 2, 3, 5, 9, 3, 5, 5, 9, 9))
  expect_equal(df$new_cases, c(0, 2, 1, 2, 4, 0, 2, 0, 4, 0))
})

test_that("augment demographic data works", {
  df <- createmockdpc()
  df <- transform_total_cases(df)
  density <- gdppercapita <- c(1.0, 2.0)
  population <- c(100, 50)
  name <- c("A", "B")
  demodata <- data.frame(density, gdppercapita, name, population)
  df <- augmentDPCdemo(df, demodata)
  expect_equal(nrow(df), 10)
  expect_equal(nrow(filter(df, population == 100)), 5)
})
