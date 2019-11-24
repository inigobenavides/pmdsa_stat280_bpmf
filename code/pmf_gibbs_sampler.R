# Probabilistic Matrix Factorization


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


PMF_Gibbs_Sampler <- function(R, k, n_replications, n_users, n_movies, sigma_U, sigma_V, sigma) {
  # Implements Probabilistic Matrix Factorization via Gibbs Sampling
  #' @param R: Matrix of [user_index, movie_index, rating]
  #' @param k: estimated number of latent dimensions for U and V
  #' @param n_replications: number of replications to run for Gibbs Sampler
  #' @param n_users: number of users in data set
  #' @param n_movies: number of movies in data set
  #' @param sigma_U: prior precision on latent user vectors
  #' @param sigma_V: prior precision on latent movie vectors
  #' @param sigma: precision for ratings
  #' @return results[List[Rs, Us, Vs, mu_Us, mu_Vs, Lambda_Us, Lambda_Vs]]
  
  # Initialize U and V
  UV_init <- initialize_UV(k, n_users, n_movies)
  U_current <- UV_init[[1]]
  V_current <- UV_init[[2]]
  
  # Initialize collections
  Rs <- list()
  Us <- list()
  Vs <- list()
  results <- list()
  
  # Run Gibbs Sampler
  for (i in 1:n_replications) {
    # Update U and V
    U_prev <- U_current
    V_prev <- V_current
    
    # Sample U
    U_current <- sample_U_pmf(R, V_prev, sigma_U, sigma, n_users)
    
    # Sample V
    V_current <- sample_V_pmf(R, U_current, sigma_V, sigma, n_movies)
    
    # Compute mean ratings
    R_current_means <- U_current %*% t(V_current)
    
    # Draw from normal with precision 1/alpha
    R_current <- structure(vapply(R_current_means,
                                  function(x) {rnorm(1, mean=x, sd=1/alpha)},
                                  numeric(1)),
                           dim=dim(R_current_means))
    
    # Collect replication samples
    Us[[i]] <- U_current
    Vs[[i]] <- V_current
    Rs[[i]] <- R_current
  }
  
  # Collect in results
  results$Rs <- Rs
  results$Us <- Us
  results$Vs <- Vs
  
  return(results)
  
}

# pmf_simulation <- PMF_Gibbs_Sampler(
#   R = sparse_observations,
#   k = k,
#   n_replications = 1000,
#   n_users = n_users,
#   n_movies = n_movies,
#   sigma_U = 10,
#   sigma_V = 10,
#   sigma = 10
# )

