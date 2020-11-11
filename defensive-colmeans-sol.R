# QUESTIONS

# Can I suppress warnings in calling expect_<datatype> that are intended to
# show in non-test environment (here: inserting df w/ either 0 rows or cols - 
# )

# PREREQUISITES ----------------------------------------------------------------

library(here)
library(tidyverse)
library(checkmate)
library(testthat)

# COUNT FUNCTION ---------------------------------------------------------------

#' Computes column means for numeric columns of a dataframe
#'
#' @param df Dataframe input
#' @return Dataframe containing column means of df's numeric columns

col_means <- function(df, na.rm = FALSE) {
  
  df <- data.frame(df) %>% 
    select_if(is.numeric)
  
  if (any(dim(df) == 0)) {
    warning("df must have at least one row and one column")
    df <- data.frame()
  }

  data.frame(lapply(df, mean, na.rm = na.rm))
}

# TESTING ----------------------------------------------------------------------

test_file(here("test-defensive-colmeans.R"))
