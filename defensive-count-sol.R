# QUESTIONS

# Is '::' really preferable to library statements if a package is extensively
# used in a script?

# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(checkmate)
library(testthat)

# COUNT FUNCTION ---------------------------------------------------------------

#' Rounds numeric input to count variable
#'
#' @param supposedly_a_count Numeric value
#' @return supposedly_a_count rounded to nearest integer

count_them <- function(supposedly_a_count) {
  
  # Lazy version
  
  # if (!test_number(supposedly_a_count, finite = TRUE, lower = 0)) {
  #   stop("supposedly_a_count must be a single, finite, positive number")
  # }
  
  # Informative version
  
  if (length(supposedly_a_count) != 1) {
    stop("supposedly_a_count must be of length 1")
  }
  
  if (is.na(supposedly_a_count)) {
    stop("supposedly_a_count may not be NA")
  }
  
  if (!test_numeric(supposedly_a_count)) {
    stop("supposedly_a_count must be numeric")
  }
  
  if (supposedly_a_count < 0) {
    stop("supposedly_a_count may not be negative")
  }
  
  if (!test_number(supposedly_a_count, finite = TRUE)) {
    stop("supposedly_a_count must be finite")
  }
  
  if(!test_count(supposedly_a_count)) {
    warning(
      "rounding ", supposedly_a_count,
      " to the nearest integer."
    )
    supposedly_a_count <- round(supposedly_a_count)
  }

  as.integer(supposedly_a_count)
  
}

# TESTING ----------------------------------------------------------------------

test_file(here("test-defensive-count.R"))
