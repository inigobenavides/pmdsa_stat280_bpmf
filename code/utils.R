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

transform_score_to_rating <- function(score) {
  #' Transforms score to rating using logit and scaling
  
  sigmoid <- 1 /  (1 + exp(-score))
  
  case_when(
    sigmoid <= 0.2 ~ 1,
    sigmoid <= 0.4 ~ 2,
    sigmoid <= 0.6 ~ 3,
    sigmoid <= 0.8 ~ 4,
    sigmoid <= 1 ~ 5
  )
}

transform_rating_to_score <- function(rating) {
  #' Transforms ratings to score 
  
  case_when(
    rating == 1 ~ 0.2,
    rating == 2 ~ 0.4,
    rating == 3 ~ 0.6,
    rating == 4 ~ 0.8,
    rating == 5 ~ 1
  )
}
