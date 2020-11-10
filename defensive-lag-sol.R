# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(checkmate)

# LAG FUNCTION -----------------------------------------------------------------

compute_lag <- function(x, n = 1L) {
  
  # Check for n to be a single integer value (use round instead of
  # test_integer bc the latter will fail for, e.g., n = 1 instead of 1L
  
  if(!test_number(n)) stop("n must be a single number")
  if(n != round(n)) stop("n must be an integer value")
  
  base_lag <- function(x, n) {
    xlen <- length(x)
    return(c(rep(NA, n), x[seq_len(xlen - n)]))
  }

  if(test_vector(x, strict = TRUE) & !(is.list(x))) {
    return(base_lag(x = x, n = n))
    }
  
  warning("x is not a vector. Lagging is applied column-wise")
  
  if(test_matrix(x)) return(apply(x, 2, base_lag, n = n))
  
  if(test_array(x) & length(dim(x)) > 2) {
    return(array(apply(x, c(2, 3), base_lag, n = n), dim = dim(x)))
  }
  
  if(test_data_frame(x)) return(data.frame(apply(x, 2, base_lag, n = n)))
  
  if(test_list(x)) return(lapply(x, base_lag, n = n))
  
  stop("x does not match any supported class")
  
}

# TESTS ------------------------------------------------------------------------

my_vec <- c(1:4)
my_matrix <- matrix(rep(my_vec, 4), ncol = 4)
my_array <- array(my_vec, dim = c(4, 4, 2))
my_df <- data.frame(my_vec, my_vec)
my_list <- list(my_vec, my_vec)
my_task <- mlr3::tsk("iris")

compute_lag(my_vec)
compute_lag(my_vec, 2)
compute_lag(my_vec, 2.5)
compute_lag(my_vec, "2")
compute_lag(my_vec, c(1:2))

compute_lag(my_matrix)
compute_lag(my_array)
compute_lag(my_df)
compute_lag(my_list)
compute_lag(my_task)

