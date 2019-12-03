test_that("remove_outliers", {
  d <- data.frame(id = c("1","2","3","4","5"), a = c(1,2,10,1000,100), b = c(19999, 30000000, 10000, 5000000, 426798))

  # One variable, no approximation
  da <- remove_outliers(d, vars = ~a)
  expect_equal(nrow(da), 4)
  expect_length(setdiff(names(d), names(da)), 0)
  expect_equal(nrow(subset(da, a == 1000)), 0)

  # All variables, no approximation
  dr <- remove_outliers(d)
  expect_equal(nrow(dr), 5)
  expect_length(setdiff(names(d), names(dr)), 0)
  expect_equal(nrow(subset(dr, a == 1000)), 0)
  expect_equal(nrow(subset(dr, b == 30000000)), 0)
  expect_equal(as.character(subset(dr, is.na(a))$id), "4")
  expect_equal(as.character(subset(dr, is.na(b))$id), "2")

  # One variable, with approximation
  da <- remove_outliers(d, vars = ~a, approx = TRUE)
  expect_equal(nrow(da), 5)
  expect_length(setdiff(names(d), names(da)), 0)
  expect_equal(nrow(subset(da, a == 1000)), 0)
  expect_lte(subset(da, id == "4")$a, 50)

  # All variables, no approximation
  dr <- remove_outliers(d, approx = TRUE)
  expect_equal(nrow(dr), 5)
  expect_length(setdiff(names(d), names(dr)), 0)
  expect_equal(nrow(subset(dr, a == 1000)), 0)
  expect_equal(nrow(subset(dr, b == 30000000)), 0)
  expect_lte(subset(dr, id == "4")$a, 50)
  expect_lte(subset(dr, id == "2")$b, 1000000)
})
