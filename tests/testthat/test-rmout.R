test_that("remove_outliers", {
  d <- data.frame(id = c("1","2","3","4","5"), a = c(1,2,10,1000,100), b = c(19999, 30000000, 1, 5, 426798))

  # One variable
  da <- remove_outliers(d, vars = ~a)
  expect_equal(nrow(da), 4)
  expect_equal(length(setdiff(names(d), names(da))), 0)
  expect_equal(nrow(subset(da, a == 1000)), 0)

  # All variables at once
  dr <- remove_outliers(d)
  expect_equal(nrow(dr), 5)
  expect_equal(length(setdiff(names(d), names(dr))), 0)
  expect_equal(nrow(subset(dr, a == 1000)), 0)
  expect_equal(nrow(subset(dr, b == 30000000)), 0)
  expect_equal(as.character(subset(dr, is.na(a))$id), "4")
  expect_equal(as.character(subset(dr, is.na(b))$id), "2")
})
