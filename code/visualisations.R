# visualisations.R
# Compilation of functions that generates visuals

vis_matrix <- function(data, row_col, column_col, 
                       value_col, label = TRUE) {
  #' Produces geom tile for matrix
  #' @param data: dataframe or matrix in tidy format [row, column, value]
  #' @param row_col: string of column name depicting row index
  #' @param column_col: string of column name depicting column index
  #' @param value_col: string of column name depicting value
  #' @param label: if true, puts label in place
  
  data %>%
    as.data.frame() %>% 
    ggplot() +
    geom_tile(
      mapping = aes_string(y = row_col, 
                           x = column_col, 
                           fill = value_col)
    ) +
    theme_minimal() +
    scale_y_reverse(breaks = 1:max(data[[row_col]])) +
    scale_x_continuous(breaks = 1:max(data[[column_col]]), position = "top") +
    scale_fill_gradient2(
      low = "#70ff75",
      high = "#91c5ff",
      midpoint = 0,
      mid = "#ffffff"
    ) -> plot
  
  if (label) {
    plot +
      geom_text(
        mapping = aes_string(y = row_col, 
                             x = column_col, 
                             label = glue("round({value_col},2)")
                  )
      ) -> plot
  }
  
  return(plot)
}

viz_posterior_rating <- function(user_index,
                                 movie_index,
                                 sampled_ratings_mapped,
                                 gibbs_results,
                                 burn_in = 500,
                                 transformed = TRUE) {
  # Visualizes the posterior rating distribution
  # for a given user and movie
  #' @param user_index: index of user to visualize
  #' @param movie_index: index of movie to visualize
  #' @param sampled_ratings_mapped: matrix of mapped ratings in tidy format
  #' @param gibbs_results: result object from BPMF_Gibbs_Sampler()
  #' @param burn_in: (optional) number of replications to burn in, default 500
  #' @param transformed: (optional) boolean of whether to plot transformed posterior, default TRUE
  #' @return viz: ggplot visualization of posterior density

  n_replications <- netflix_bpmf_gibbs_results$Rs %>% length
  xs <- burn_in:n_replications
  true_rating <- sampled_ratings_mapped %>%
    filter(user_key == user_index) %>% 
    filter(movie_key == movie_index) %>% 
    select(rating) %>% 
    as.numeric()
  
  if (transformed) {
    true_rating <- true_rating %>% transform_score_to_rating
  }
  
  x <- xs %>%
    Map(function(y) {
      if (transformed) {
        gibbs_results$Rs[[y]][user_index, movie_index] %>% 
          transform_score_to_rating
      } else {
        gibbs_results$Rs[[y]][user_index, movie_index]
      }
    }, .) %>% unlist
  
  data.frame(x=x) %>%
    ggplot(aes(x=x)) +
    geom_density() +
    theme_minimal() +
    geom_vline(xintercept=true_rating, color='red') -> plot
  
  return(plot)
}
