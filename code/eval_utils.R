# eval_utils.R
# Functions to evaluate recommender system models
# Implementations of RMSE, precision and recall curves

train_test_split <- function(R, train_size=0.8) {
  # Temporally splits ratings data by user
  # where training set corresponds to first train_size per user
  # and test set corresponds to last 1 - train_size per user
  #' @param R: Ratings matrix in tidy format with timestamp; IMPORTANT: must have at least four ratings per user
  #' @param train_size: Double of the proportion to split, value must be between 0 and 1, default is 0.8
  #' @return result[$R_train, $R_test]: object of train and test ratings matrices in tidy format
  
  R_rn <- R %>%
    arrange(userId, timestamp) %>%
    group_by(userId) %>%
    mutate(rn=ntile(row_number(), 10)/10)
  
  R_train <- R_rn %>% filter(rn < train_size) %>% select(userId, movieId, rating, timestamp) %>% as.data.frame()
  R_test <- R_rn %>% filter(rn >= train_size) %>% select(userId, movieId, rating, timestamp) %>% as.data.frame()
  
  result <- c()
  result$train <- R_train
  result$test <- R_test
  return(result)
}



set.seed(1)
# Generate sampled ratings
sampled_ratings <- sample_ratings_top_users(ratings, 15, 15, max_num_users_per_movie = 10, min_num_users_per_movie = 6) %>%
  mutate(value=sapply(value, transform_score_to_rating)) %>%
  rename(userId=row,
         movieId=col,
         rating=value)


# Split sampled ratings
split_sampled_ratings <- train_test_split(sampled_ratings)

sampled_ratings_train <- split_sampled_ratings$train
sampled_ratings_test <- split_sampled_ratings$test

# Map sampled ratings train
sampled_ratings_user_key_train <- sampled_ratings_train %>%
  select(userId) %>%
  distinct %>%
  mutate(user_key=row_number())

sampled_ratings_movie_key_train <- sampled_ratings_train %>%
  select(movieId) %>%
  distinct %>%
  mutate(movie_key=row_number())

sampled_ratings_mapped_train <- sampled_ratings_train %>%
  left_join(sampled_ratings_user_key_train, by="userId") %>% 
  left_join(sampled_ratings_movie_key_train, by="movieId") %>% 
  select(user_key, movie_key, rating)

# Run training set through BPMF
# Implement Gibbs sampler
n_replications <- 1000

# Define initial parameters
k_estimate <- 5
alpha <- 2
mu_0 <- rep(0, k_estimate)
beta_0 <- 1
nu_0 <- 1
W_0 <- diag(k_estimate)

sampled_ratings_matrix_train <- sampled_ratings_mapped_train %>%
  mutate(rating=sapply(rating, transform_rating_to_score)) %>% 
  select(user_key, movie_key, rating) %>%
  as.matrix

n_users <- sampled_ratings_train %>%
  summarise(n_users=n_distinct(userId)) %>%
  as.numeric()

n_movies <- sampled_ratings_train %>%
  summarise(n_movies=n_distinct(movieId)) %>%
  as.numeric()

# Run replications and models
netflix_bpmf_gibbs_results_train <- BPMF_Gibbs_Sampler(
  sampled_ratings_matrix_train, 
  k_estimate, 
  n_replications, 
  n_users, 
  n_movies, 
  mu_0, beta_0, nu_0, W_0, alpha
)

# PMF model
netflix_pmf_gibbs_results_train <- PMF_Gibbs_Sampler(
  sampled_ratings_matrix_train,
  k_estimate,
  n_replications,
  n_users,
  n_movies,
  sigma_U = 1,
  sigma_V = 1,
  sigma = 1
)

# SVD model
svd_train <- svd_solver(
  sampled_ratings_train,
  k_estimate,
  n_users,
  n_movies) %>%
  matrix_to_tidydf() %>% 
  mutate(value=sapply(value, transform_score_to_rating)) %>% 
  left_join(sampled_ratings_user_key_train, by=c("row" = "user_key")) %>% 
  left_join(sampled_ratings_movie_key_train, by=c("col" = "movie_key")) %>% 
  select(userId, movieId, value) %>% 
  rename(rating=value)

# Compute posterior mean of replications Rs and convert to tidy format
compute_mean_posterior_ratings_matrix <- function(gibbs_result, burn_in, n_replications) {
  # Computes posterior mean of replications from gibbs_result
  #' @param gibbs_result: return object from Gibbs Sampler
  #' @param burn_in: number of replications to burn in
  #' @param n_replications: number of replications
  #' @return posterior_means_matrix: matrix of posterior mean ratings
  
  posterior_means_matrix <- burn_in:n_replications %>%
    Map(function(x) {
      df <- gibbs_result$Rs[[x]] %>%
        matrix_to_tidydf() %>%
        mutate(replication=x)
    }, .) %>% Reduce(function(x, y) {
      rbind(x, y)
    }, .) %>% 
    group_by(row, col) %>% 
    summarise(value=mean(value))
  
  return(posterior_means_matrix)
}

netflix_bpmf_gibbs_posterior_ratings <- compute_mean_posterior_ratings_matrix(
  netflix_bpmf_gibbs_results_train,
  burn_in = 500,
  n_replications = n_replications
) %>% mutate(value=sapply(value, transform_score_to_rating)) %>%
  as.data.frame %>% 
  left_join(sampled_ratings_user_key_train, by=c("row" = "user_key")) %>% 
  left_join(sampled_ratings_movie_key_train, by=c("col" = "movie_key")) %>% 
  select(userId, movieId, value) %>% 
  rename(rating=value)
  

netflix_pmf_gibbs_posterior_ratings <- compute_mean_posterior_ratings_matrix(
  netflix_pmf_gibbs_results_train,
  burn_in = 500,
  n_replications = n_replications
) %>% mutate(value=sapply(value, transform_score_to_rating)) %>%
  as.data.frame %>% 
  left_join(sampled_ratings_user_key_train, by=c("row" = "user_key")) %>% 
  left_join(sampled_ratings_movie_key_train, by=c("col" = "movie_key")) %>% 
  select(userId, movieId, value) %>% 
  rename(rating=value)

# Compute RMSE
compute_RMSE <- function(R1, R2) {
  # Computes RMSE for two ratings matrices in tidy format
  #' @param R1: first ratings matrix
  #' @param R2: second ratings matrix
  #' @return RMSE: scalar of root mean square error
  R1 %>% inner_join(R2, by=c("userId", "movieId")) %>%
    mutate(square_error=(rating.x - rating.y)^2) %>%
    summarise(rmse=sqrt(mean(square_error))) %>% 
    as.numeric
}

compute_RMSE(svd_train, sampled_ratings_test)
compute_RMSE(netflix_pmf_gibbs_posterior_ratings, sampled_ratings_test)
compute_RMSE(netflix_bpmf_gibbs_posterior_ratings, sampled_ratings_test)
