# utils.R
# List of functions for utilities

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

transform_rating_to_score <- function(rating, 
                                      min = 0.5,
                                      max = 5,
                                      by = 0.5) {
  #' Transforms ratings to score 
  
  rescaling <- case_when(
    rating == min ~ 0.000000000001,
    rating == max ~ 0.999999999999,
    TRUE ~ (rating - by)/(max - by)
  )
  
  log(rescaling / (1 - rescaling))
}

transform_score_to_rating <- function(score, 
                                      min = 0.5,
                                      max = 5,
                                      by = 0.5) {
  #' Transforms score to rating using logit and scaling
  
  sigmoid <- 1 / (1 + exp(-score))
  
  case_when(
    sigmoid <= transform_rating_to_score(min, min = min, max = max,
                              by = by) ~ min,
    sigmoid >= transform_rating_to_score(max, min = min, max = max,
                                       by = by) ~ max,
    TRUE ~ sigmoid * (max - by) + by
  )
}

compute_matrix_difference <- function(subtrahend, minuend,
                                      subtrahend_row_col,
                                      subtrahend_column_col,
                                      subtrahend_value_col,
                                      minuend_row_col,
                                      minuend_column_col,
                                      minuend_value_col
                                      ) {
  #' Given two matrices in tidy format, compute for the 
  #' difference among common indeces. Deletes rows
  #' where indeces are not common to both
  
  subtrahend_col_names <- colnames(subtrahend)
  minuend_col_names <- colnames(minuend)
  
  # Rename Row
  colnames(subtrahend)[
    which(subtrahend_col_names == subtrahend_row_col)
  ] <- 'row'
  
  colnames(minuend)[
    which(minuend_col_names == minuend_row_col)
    ] <- 'row'
  
  # Rename Col
  colnames(subtrahend)[
    which(subtrahend_col_names == subtrahend_column_col)
    ] <- 'col'
  
  colnames(minuend)[
    which(minuend_col_names == minuend_column_col)
    ] <- 'col'
  
  subtrahend %>% 
    inner_join(minuend, by = c('row', 'col')) %>% 
    mutate(difference = !!parse_expr(subtrahend_value_col) - !!parse_expr(minuend_value_col)) %>% 
    select(row, col, difference)
}
