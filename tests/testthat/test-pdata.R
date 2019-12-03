test_that("predict_data", {
  d <- data.frame(
    y = c(2010, 2013, 2014),
    total = c(30800000.00, 32285714.00, 41500000.00)
  )
  i <- seq(from = 2015, to = 2020, by = 1)
  for (m in c("lm", "glm", "gam")) {
    dp <- predict_data(.data = d, .interval = i, method = m)
    expect_equal(nrow(dp), length(i) + nrow(d))
    expect_length(setdiff(names(d), names(dp)), 0)
  }
})
