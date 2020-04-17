test_that("80% confidence interval works", {
  data <- 1:100
  results <- mean_cl_quantile(data, na.rm = TRUE)
  expect_equal(results$y, 50.5)
  expect_equal(results$ymin, 10.9)
  expect_equal(results$ymax, 90.1)
})
