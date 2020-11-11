# TEST OBJECTS -----------------------------------------------------------------

my_vec <- c(1:4)
my_mixed <- c(1:3, "a")
my_matrix <- matrix(rep(my_vec, 4), ncol = 4)
my_array <- array(my_vec, dim = c(4, 4, 2))
my_df <- data.frame(my_vec, my_vec)
my_list <- list(my_vec, my_vec)
my_task <- mlr3::tsk("iris")

# TESTS ------------------------------------------------------------------------

test_that("lag requires n to be integer value", {
  expect_silent(compute_lag(my_vec))
  expect_silent(compute_lag(my_vec, 2))
  expect_error(compute_lag(my_vec, 2.5))
  expect_error(compute_lag(my_vec, "2"))
  expect_error(compute_lag(my_vec, c(1:2)))
})

test_that("lag rejects n > length(x)", {
  expect_error(compute_lag(my_vec, length(my_vec) + 1))
})

test_that("lag can handle matrices, arrays and dataframes but throws warning", {
  expect_warning(compute_lag(my_matrix))
  expect_warning(compute_lag(my_array))
  expect_warning(compute_lag(my_df))
})

test_that("lag rejects objects of other classes", {
  expect_error(compute_lag(my_task))
  expect_error(compute_lag(my_list))
})