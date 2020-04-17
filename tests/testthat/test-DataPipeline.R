test_that("transform total cases works", {
  province <- c(rep("A", 9), rep("B", 9))
  dateraw <- c(paste("2020-01-0", 1:9, sep=""))
  date <- c(dateraw, dateraw)
  total_cases <- c(0, 2, 3, 5, 9, 12, 15, 19, 20,
                   3, 5, 3, 9, 2, 5, 100, 120, 140)
  df <- data.frame(province, date, total_cases)
  df <- transform_total_cases(df)
  expect_equal(as.character(df$date), as.character(c(dateraw[1:5], dateraw[1:5])))
  expect_equal(df$total_cases, c(0, 2, 3, 5, 9, 3, 5, 5, 9, 9))
  expect_equal(df$new_cases, c(0, 2, 1, 2, 4, 0, 2, 0, 4, 0))
})
