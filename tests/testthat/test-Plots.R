test_that("80% confidence interval works", {
  data <- 1:100
  results <- mean_cl_quantile(data, na.rm = TRUE)
  expect_equal(results$y, 50.5)
  expect_equal(results$ymin, 10.9)
  expect_equal(results$ymax, 90.1)
})

test_that("remove year of date range works on various types", {
  range <- c("2019-07-02", "2019-07-03", "2030-01-01")
  ry1 <- removeyear(range)
  ry2 <- removeyear(as.POSIXct(range))
  expect_equal(ry1, c("07-02", "07-03", "01-01"))
  expect_equal(ry2, c("07-02", "07-03", "01-01"))
})

