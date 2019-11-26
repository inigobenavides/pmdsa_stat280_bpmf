# eval_utils.R
# Functions to evaluate recommender system models
# Implementations of RMSE, train_test_split

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

model_evaluation_pipeline <- function(R, k) {
  # Pipeline to run model evaluation and comparisons
  # given sampled ratings and given k
  #' @param R: sampled ratings matrix in tidy format
  #' @param k: number of latent factors to pass to BPMF, PMF, and SVD
  #' @param model_eval: dataframe of RMSE by model
  
  logger::log_info(paste("Running model evaluation pipeline, k =", k))
  
  # Split sampled ratings
  split_sampled_ratings <- train_test_split(R)
  
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
  
  # Define initial parameters
  n_replications <- 1000
  alpha <- 2
  mu_0 <- rep(0, k)
  beta_0 <- 1
  nu_0 <- 1
  W_0 <- diag(k)
  
  # Run BPMF
  logger::log_info("Running BPMF...")
  netflix_bpmf_gibbs_results_train <- BPMF_Gibbs_Sampler(
    sampled_ratings_matrix_train, 
    k, 
    n_replications, 
    n_users, 
    n_movies, 
    mu_0, beta_0, nu_0, W_0, alpha
  )
  
  # Run PMF
  logger::log_info("Running PMF...")
  netflix_pmf_gibbs_results_train <- PMF_Gibbs_Sampler(
    sampled_ratings_matrix_train,
    k,
    n_replications,
    n_users,
    n_movies,
    sigma_U = 1,
    sigma_V = 1,
    sigma = 1
  )
  
  # SVD model
  logger::log_info("Running SVD...")
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
  
  # Compute posterior means for BPMF replications
  logger::log_info("Computing BPMF posterior means...")
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
  
  # Compute posterior means for PMF replications
  logger::log_info("Computing PMF posterior means...")
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
  
  
  rmse_svd_train <- compute_RMSE(svd_train, sampled_ratings_train)
  rmse_pmf_train <- compute_RMSE(netflix_pmf_gibbs_posterior_ratings, sampled_ratings_train)
  rmse_bpmf_train <- compute_RMSE(netflix_bpmf_gibbs_posterior_ratings, sampled_ratings_train)
  rmse_svd_test <- compute_RMSE(svd_train, sampled_ratings_test)
  rmse_pmf_test <- compute_RMSE(netflix_pmf_gibbs_posterior_ratings, sampled_ratings_test)
  rmse_bpmf_test <- compute_RMSE(netflix_bpmf_gibbs_posterior_ratings, sampled_ratings_test)
  
  model_results <- data.frame(model=c("SVD", "PMF", "BPMF"),
                              rmse_train=c(rmse_svd_train, rmse_pmf_train, rmse_bpmf_train),
                              rmse_test=c(rmse_svd_test, rmse_pmf_test, rmse_bpmf_test))
  logger::log_info("Done")
  return(model_results)
}



