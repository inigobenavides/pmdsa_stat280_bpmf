# utils.R
# List of functions for utilities

library(purrr)

matrix_to_tidydf <- function(matrix) {
  #' Converts a matrix datatype into a tidyformat dataframe
  #' with first column
  
  dims <- dim(matrix)
  map_df(
    1:dims[1],
    function(rows) {
      map_df(
        1:dims[2],
        function(cols) {
          data.frame(
            row = rows,
            col = cols,
            value = matrix[rows,cols]
          )
        }
      )
    }
  )
}

generate_synthetic_matrix <- function(n, df) {
  #' Generates a matrix of samples from a normal-wishart 
  #' distribution with an identity covariance matrix and mean 0.
  #' @param n number of samples
  #' @param df degrees of freedom for Wishart distribution
  
  1:n %>% Map(function(x) {
    sampled_covariance <- rWishart(1, df, diag(df))[,,1]
    rmvnorm(1, mean = rep(0, df), sigma = sampled_covariance)
  }, .) %>% Reduce(function(x, y){
    rbind(x, y)
  }, .)
}