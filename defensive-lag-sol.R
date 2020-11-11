# QUESTIONS --------------------------------------------------------------------

# Is that a case of speculative generality? Should I restrict to vector input
# and let people use apply & friends?
   
# Is there any more elegant way to subsume objects that can be created 
# cbinding vectors?

# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(checkmate)
library(testthat)

# LAG FUNCTION -----------------------------------------------------------------

#' Returns version of input that begins at position n + 1, filling first n
#' positions with NA
#'
#' @param x Vector object (array and dataframe objects are also supported)
#' @param n Number of positions x is to be lagged by (default is 1L)
#' @return Version of x with lagged entries

compute_lag <- function(x, n = 1L) {
  
  # Check if n is of the required data type
  
  if (!test_count(n)) stop("n must be a single, non-negative integer")
  
  # Check if x is a vector or at least a matrix/array/dataframe whose columns 
  # are vectors, such that lagging can be reasonably applied
  
  if (is.null(nrow(x)) && !is.atomic(x)) stop("x cannot be coerced to a vector")

  # Base function; throws error if n is > length(x)

  compute_lag <- function(x, n) {
    xlen <- length(x)
    if(n > xlen) stop("n must not be greater than length(x)")
    return(c(rep(NA, n), x[seq_len(xlen - n)]))
  }
  
  # Vectors

  if (is.null(nrow(x))) return(compute_lag(x = x, n = n))
  
  # Matrices/arrays/dataframes

  warning("x is not one-dimensional, lagging is applied column-wise")

  if (test_matrix(x)) return(apply(x, 2, compute_lag, n = n))
  
  if (test_array(x) & length(dim(x)) > 2) {
    return(array(apply(x, c(2, 3), compute_lag, n = n), dim = dim(x)))
  }
  
  if(test_data_frame(x)) return(data.frame(apply(x, 2, compute_lag, n = n)))

  NA
  
}

# TESTS ------------------------------------------------------------------------

test_file(here("test-defensive-lag-sol.R"))

