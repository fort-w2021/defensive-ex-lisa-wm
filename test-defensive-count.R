context("checking count_them")

test_that("does the right thing for sensible inputs", {
  expect_equivalent(count_them(1), 1L)
  expect_equivalent(count_them(0.0), 0L)
  expect_identical(count_them(1.0), 1L)
  expect_warning(count_them(1.4), "rounding")
  expect_identical(suppressWarnings(count_them(2.6)), 3L)
})

test_that("does the right thing for problematic inputs", {
  expect_error(count_them(-1.1), "negative")
  expect_error(count_them(c(1.1, 2)), "length")
  expect_error(count_them(NA_real_), "NA")
  expect_error(count_them(NA), "NA")
  expect_error(count_them(Inf), "finite")
  expect_error(count_them("a"), "numeric")
  expect_error(count_them(list("a","b", 20)), "length")
})
