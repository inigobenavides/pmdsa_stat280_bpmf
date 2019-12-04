# Utilities concerning sampling

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
    select(userId, movieId, rating, timestamp) %>% 
    as.data.frame()
  
  return(ratings_user_movie_subset)
  
}


sample_ratings_top_users <- function(R, n_users, n_movies,
                                     max_num_users_per_movie, 
                                     min_num_users_per_movie) {
  #' Samples from ratings feather a curated list of users and movies
  #' @param R: ratings feather data frame
  #' @param n_users: number of top users 
  #' @param n_movies: number of movies
  #' @param max_num_users_per_movie: maximum number of users that rated a movie
  #' @param min_num_users_per_movie: minimum number of users that rated a movie
  
  top_users <- R %>%
    group_by(userId) %>% 
      summarise(
        num_movies = n()
      ) %>% 
      ungroup() %>% 
      arrange(desc(num_movies)) %$%
      userId %>% 
      {.[1:n_users]}
  
  # Sample movies with more than 4 user ratings
  sample_movies <- R %>% 
    filter(userId %in% top_users) %>% 
    group_by(movieId) %>% 
    summarise(
      num_users = n()
    ) %>% ungroup() %>% 
    arrange(desc(num_users)) %>% 
    filter(num_users >= min_num_users_per_movie) %>% 
    filter(num_users <= max_num_users_per_movie) %>% 
    sample_n(n_movies) %$% 
    movieId
  
  R %>% 
    filter(userId %in% top_users) %>% 
    filter(movieId %in% sample_movies) %>% 
    # mutate(userId = as.numeric(factor(userId, labels = seq(1:n_users)))) %>% 
    # mutate(movieId = as.numeric(factor(movieId, labels = seq(1:n_movies)))) %>% 
    rename(
      row = userId,
      col = movieId,
      value = rating
    ) %>% 
    # mutate(value = sapply(value, transform_rating_to_score)) %>% 
    as.data.frame()
}

