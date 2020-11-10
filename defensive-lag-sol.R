# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(testthat)

# LAG FUNCTION -----------------------------------------------------------------

lag_improved <- function(x, n = 1L) {
  
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
  
}

dplyr::lag(c(1, 2, 3))

df = data.frame(
  a = c(1:3),
  b = c(1:3),
  d = c(1:3)
)

l = list(
  a = c(1:3),
  b = c(1:3)
)

lag(df, 2.5)

lag(c("a", "b"))
