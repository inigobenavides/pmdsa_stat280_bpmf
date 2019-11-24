UV <- initialize_UV(k_estimate, n_users, n_movies)
U_init <- UV[[1]]
V_init <- UV[[2]]

pmf_solver(
  R = sparse_observations,
  U = U_init,
  V = V_init
)

.Last.value -> x

x %>% 
  matrix_to_tidydf()


compute_matrix_difference(
  subtrahend = x %>% 
    matrix_to_tidydf() %>% 
    mutate(value_pmf = sapply(value, transform_score_to_rating)) %>% 
    select(-value), 
  minuend = sparse_observations %>% 
    mutate(value = sapply(value, transform_score_to_rating)),
  
  subtrahend_row_col = 'row',
  subtrahend_column_col = 'col',
  subtrahend_value_col = 'value_pmf',
  minuend_row_col = 'row',
  minuend_column_col = 'col',
  minuend_value_col = 'value'
) %>% 
  vis_matrix(row_col = 'row',
             column_col = 'col',
             value_col = 'difference') +
  labs(y = 'User', x = 'Movie')


# RMSE

compute_matrix_difference(
  subtrahend = x %>% 
    matrix_to_tidydf() %>% 
    mutate(value_pmf = sapply(value, transform_score_to_rating)) %>% 
    select(-value), 
  minuend = sparse_observations %>% 
    mutate(value = sapply(value, transform_score_to_rating)),
  
  subtrahend_row_col = 'row',
  subtrahend_column_col = 'col',
  subtrahend_value_col = 'value_pmf',
  minuend_row_col = 'row',
  minuend_column_col = 'col',
  minuend_value_col = 'value'
) %>% summarise(
  rmse = sum(difference^2)
)

compute_matrix_difference(
  subtrahend = simulations.dt %>% 
    group_by(movie_index, user_index) %>% 
    summarise(mean = mean(simulated_rating)) %>% 
    ungroup() %>% 
    mutate(mean = sapply(mean, transform_score_to_rating)), 
  minuend = sparse_observations %>% 
    mutate(value = sapply(value, transform_score_to_rating)),
  
  subtrahend_row_col = 'user_index',
  subtrahend_column_col = 'movie_index',
  subtrahend_value_col = 'mean',
  minuend_row_col = 'row',
  minuend_column_col = 'col',
  minuend_value_col = 'value'
) %>% summarise(
  rmse = sum(difference^2)
)
