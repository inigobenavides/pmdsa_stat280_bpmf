# Bayesian Probabilistic Matrix Factorization
source("code/bpmf_utils.R")


initialize_UV <- function(k_estimate, n_users, n_movies) {
  # Initializes U and V latent feature matrices
  U_init <- 1:n_users %>% Map(function(x) {
    sampled_covariance <- rWishart(1, k_estimate, diag(k_estimate))[,,1]
    rmvnorm(1, mean=rep(0, k_estimate), sigma=sampled_covariance)
  }, .) %>% Reduce(function(x, y){
    rbind(x, y)
  }, .)
  
  V_init <- 1:n_movies %>% Map(function(x) {
    sampled_covariance <- rWishart(1, k_estimate, diag(k_estimate))[,,1]
    rmvnorm(1, mean=rep(0, k_estimate), sigma=sampled_covariance)
  }, .) %>% Reduce(function(x, y){
    rbind(x, y)
  }, .)
  
  return(list(U_init, V_init))
}


BPMF_Gibbs_Sampler <- function(R, k, n_replications, n_users, n_movies, mu_0, beta_0, nu_0, W_0, alpha) {
  # Implements Bayesian Probabilistic Matrix Factorization via Gibbs Sampling
  # @param R: Matrix of [user_index, movie_index, rating]
  # @param k: estimated number of latent dimensions for U and V
  # @param n_replications: number of replications to run for Gibbs Sampler
  # @param n_users: number of users in data set
  # @param n_movies: number of movies in data set
  # @param mu_0: hyperparameter Matrix[k x 1]
  # @param beta_0: hyperparameter Scalar
  # @param nu_0: hyperparameter Scalar
  # @param W_0: hyperparameter Matrix[k x k]
  # @param alpha: hyperparameter Scalar
  # @return results[List[Rs, Us, Vs, mu_Us, mu_Vs, Lambda_Us, Lambda_Vs]]
  
  # Initialize U and V
  UV_init <- initialize_UV(k, n_users, n_movies)
  U_current <- UV_init[[1]]
  V_current <- UV_init[[2]]
  
  # Initialize collections
  mu_Us <- list()
  mu_Vs <- list()
  Lambda_Us <- list()
  Lambda_Vs <- list()
  Rs <- list()
  Us <- list()
  Vs <- list()
  results <- list()
  
  # Run Gibbs Sampler
  for (i in 1:n_replications) {
    # Update U and V
    U_prev <- U_current
    V_prev <- V_current
    
    # Sample theta U
    sampled_theta_U <- sample_theta_U(U_prev, mu_0, beta_0, nu_0, W_0, n_users)
    sampled_mean_U <- sampled_theta_U[[1]]
    sampled_Lambda_U <- sampled_theta_U[[2]]
    
    # Sample theta V
    sampled_theta_V <- sample_theta_V(V_prev, mu_0, beta_0, nu_0, W_0, n_movies)
    sampled_mean_V <- sampled_theta_V[[1]]
    sampled_Lambda_V <- sampled_theta_V[[2]]
    
    # Sample U
    U_current <- sample_U(sparse_observations, sampled_mean_U, sampled_Lambda_U, V_prev, alpha, k, n_users)
    
    # Sample V
    V_current <- sample_V(sparse_observations, sampled_mean_V, sampled_Lambda_V, U_current, alpha, k, n_movies)
    
    # Compute mean ratings
    R_current_means <- U_current %*% t(V_current)
    
    # Draw from normal with precision 1/alpha
    R_current <- structure(vapply(R_current_means,
                                  function(x) {rnorm(1, mean=x, sd=1/alpha)},
                                  numeric(1)),
                           dim=dim(R_current_means))
    
    # Collect replication samples
    mu_Us[[i]] <- sampled_mean_U
    mu_Vs[[i]] <- sampled_mean_V
    Lambda_Us[[i]] <- sampled_Lambda_U
    Lambda_Vs[[i]] <- sampled_Lambda_V
    
    Us[[i]] <- U_current
    Vs[[i]] <- V_current
    Rs[[i]] <- R_current
  }
  
  # Collect in results
  results$Rs <- Rs
  results$Us <- Us
  results$Vs <- Vs
  results$mu_Us <- mu_Us
  results$mu_Vs <- mu_Vs
  results$Lambda_Us <- Lambda_Us
  results$Lambda_Vs <- Lambda_Vs
  
  return(results)
  
}

