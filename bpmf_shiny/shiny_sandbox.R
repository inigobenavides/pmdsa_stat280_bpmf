ratings <- read_feather("../rating.feather")

source('global.R')

sample_ratings_top_users(ratings, n_users = 10,
                         n_movies = 10, 
                         max_num_users_per_movie = 7,
                         min_num_users_per_movie = 3) %>% 
  saveRDS("dummy_data/netflix_sample.rds")
