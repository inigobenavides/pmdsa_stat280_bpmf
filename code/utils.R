# utils.R
# List of functions for utilities

library(purrr)

sample_ratings <- function(R, approx_n_users, n_movies) {
  # Stratify samples a ratings matrix in tidy format
  # first by rating distribution per user
  # then by rating distribution per movie
  #' @param R: Ratings matrix in tidy format 
  #' @param approx_n_users: number of users to sample per decile
  #' @param n_movies: number of movies to sample per decile
  #' @return R_subset: Subset ratings matrix with n distinct users <= approx_n_users and n_distinct movies 
  
  users_to_sample_per_decile <- floor(approx_n_users / 10)
  movies_to_sample_per_decile <- floor(n_movies / 10)
  
  # Stratify sample ratings by count distribution per user
  sampled_users <- R %>%
    group_by(userId) %>%
    summarise(n=n()) %>%
    mutate(decile_user=ntile(n, 10)) %>% 
    group_by(decile_user) %>% 
    sample_n(users_to_sample_per_decile)
  
  ratings_user_subset <- R %>%
    inner_join(sampled_users, by="userId") %>%
    select(userId, movieId, rating, timestamp, decile_user)
  
  # Stratify sample ratings by count distribution per movie
  sampled_movies <- ratings_user_subset %>%
    group_by(movieId) %>%
    summarise(n=n()) %>%
    mutate(decile_movie=ntile(n, 10)) %>% 
    group_by(decile_movie) %>% 
    sample_n(movies_to_sample_per_decile)
  
  ratings_user_movie_subset <- ratings_user_subset %>%
    inner_join(sampled_movies, by="movieId") %>%
    select(userId, movieId, rating, timestamp)
  
  return(ratings_user_movie_subset)
  
}

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
  
  rescaling <- case_when(
    rating == 1 ~ 0.2,
    rating == 2 ~ 0.4,
    rating == 3 ~ 0.6,
    rating == 4 ~ 0.8,
    rating == 5 ~ 0.999999 # 1 returns Inf
  )
  
  log(rescaling / (1 - rescaling))
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
