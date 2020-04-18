test_that("discretized SI distribution", {
  sidist <- getSIdist(5, mean = 2.0, sd = 1.0)
  expect_true(abs(sum(sidist) - 1) < 0.01)
  expect_true(sidist[2] == max(sidist))
})
