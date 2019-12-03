test_that("aggregate_data", {
  d <- data.frame(id = c("i1", "i1", "i2", "i3"), y = c(10, 20, 10, 10), a = c(4,2,3, 1), r = c(4,5,6,7))
  dr <- agregate_data(d, f = "i1+i2+i3", id_cols = ~y, value_cols = c("a", "r"), na.rm = TRUE)
  expect_equal(subset(dr, y == 10)$a, 8)
  expect_equal(subset(dr, y == 10)$r, 17)
  expect_equal(subset(dr, y == 20)$a, 2)
  expect_equal(subset(dr, y == 20)$r, 5)

  d <- data.frame(id = c("i1", "i2", "i3"), a = c(4, 3, 1), r = c(4, 6,7))
  dr <- agregate_data(d, f = "i1+i2+ifelse(i3==1,0,-1)", value_cols = c("a", "r"))
  expect_equal(dr$a, 7)
  expect_equal(dr$r, 9)
})
