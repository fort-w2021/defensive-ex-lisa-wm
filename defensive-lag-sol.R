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
  
  base_lag <- function() {
    xlen <- length(x)
    return(c(rep(NA, n), x[seq_len(xlen - n)]))
  }

  if(test_vector(x, strict = TRUE)) return(base_lag())
  
  warning("x is not a vector. Lagging will be applied column-wise")
  
  if(test_matrix(x)) apply(x, 2, base_lag)
  
  if(test_array(x)) {
    array(apply(x, c(2, 3), base_lag), dim = dim(x))
  }
  
  if(test_data_frame(x)) data.frame(apply(x, 2, base_lag))
  
  if(test_list(x)) lapply(x, base_lag)
  
  return(NA)
  
}

compute_lag(my_vec)
compute_lag(my_df)

compute_lag <- function(object) UseMethod("compute_lag")
compute_lag.default <- function(object) stop("lag mi doch")

compute_lag.vector <- function(x, n = 1L) {
  
  xlen <- length(x)
  return(c(rep(NA, n), x[seq_len(xlen - n)]))
  
}

compute_lag.matrix <- function(x, n = 1L) {
  
  warning("lagging will be applied to all column vectors")
  apply(x, 2, compute_lag.vector)

}

compute_lag.array <- function(x, n = 1L) {
  
  warning("lagging will be applied to all column vectors within the enclosed 
          matrices")
  array(apply(x, c(2, 3), compute_lag.vector), dim = c(4, 4, 2))
  
}

compute_lag.data.frame <- function(x, n = 1L) {
  
  warning("lagging will be applied to all column vectors")
  data.frame(apply(x, 2, compute_lag.vector))
  
}

compute_lag.list <- function(x, n = 1L) {
  
  warning("lagging will be applied to all list entries")
  lapply(x, compute_lag.vector)
  
}

my_vec <- c(1:4)
my_list <- list(my_vec, my_vec)
my_df <- data.frame(my_vec, my_vec)
my_matrix <- matrix(rep(my_vec, 4), ncol = 4)
my_array <- array(my_vec, dim = c(4, 4, 2))

lag_orig <- function(x, n = 1L) {
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}

lag_orig(my_matrix)

apply(my_matrix, 2, cumsum)

z <- array(1:24, dim = 2:4)
zseq <- apply(z, 1:2, function(x) seq_len(max(x)))
zseq         ## a 2 x 3 matrix
typeof(zseq) ## list
dim(zseq) ## 2 3
zseq[1,]
apply(z, 3, function(x) seq_len(max(x)))

# TESTS ------------------------------------------------------------------------

my_vec <- c(1:4)
my_list <- list(my_vec, my_vec)
my_df <- data.frame(my_vec, my_vec)
my_matrix <- matrix(rep(my_vec, 4), ncol = 4)
my_array <- array(my_vec, dim = c(4, 4, 2))