# Functions for BPMF Gibbs Sampler

# Define helper functions to re-use for U and V
compute_U_bar <- function(latent_matrix, N) {
  U_bar <- colSums(latent_matrix) / N
  return(U_bar)
}

compute_S_bar <- function(latent_matrix, N) {
  S_bar <- (t(latent_matrix) %*% latent_matrix) / N
  return(S_bar)
}

compute_mu_0_star <- function(beta_0_prev, mu_0_prev, latent_matrix, N) {
  return((beta_0_prev * mu_0_prev + N * compute_U_bar(latent_matrix, N)) / (beta_0_prev + N))
}

compute_beta_0_star <- function(beta_0_prev, N) {
  return(beta_0_prev + N)
}

compute_nu_0_star <- function(nu_0_prev, N) {
  return(nu_0_prev + N)
} 

compute_W_0_star <- function(W_0, latent_matrix, N, mu_0_prev, beta_0_prev) {
  W_0_inv <- solve(W_0)
  S_bar <- compute_S_bar(latent_matrix, N)
  U_bar <- compute_U_bar(latent_matrix, N)
  W_0_star_inv <- (W_0_inv + N * S_bar + ((beta_0 * N) / (beta_0 + N)) * ((mu_0_prev - U_bar) %*% t(mu_0_prev - U_bar)))
  W_0_star <- solve(W_0_star_inv)
  return(W_0_star)
}

# Define theta_U sampler
sample_theta_U <- function(U_prev, mu_0_prev, beta_0_prev, nu_0_prev, W_0_prev, n_users) {
  # @param U_prev: Matrix[n_users: Integer, k: Integer]
  # @param mu_0_prev: Matrix[n_users: Integer, 1]
  # @param beta_0_prev: Scalar: Integer
  # @param nu_0_prev: Scalar: Integer
  # @param W_0_prev: Matrix[k: Integer, k: Integer]
  # @param n_users: Scalar: Integer
  # @return tuple(mu_U, Lambda_U)
  
  nu_0_star_U <- compute_nu_0_star(nu_0_prev, n_users)
  W_star_U <- compute_W_0_star(W_0_prev, U_prev, n_users, mu_0_prev, beta_0_prev)
  mu_star_U <- compute_mu_0_star(beta_0_prev, mu_0_prev, U_prev, n_users)
  
  Lambda_U <- rWishart(1, nu_0_star_U, W_star_U)[,,1]
  mu_U <- rmvnorm(1, mu_star_U, Lambda_U)
  return(list(mu_U, Lambda_U))
}

# Define theta_V sampler
sample_theta_V <- function(V_prev, mu_0_prev, beta_0_prev, nu_0_prev, W_0_prev, n_movies) {
  # @param V_prev: Matrix[n_movies: Integer, k: Integer]
  # @param mu_0_prev: Matrix[n_users: Integer, 1]
  # @param beta_0_prev: Scalar: Integer
  # @param nu_0_prev: Scalar: Integer
  # @param W_0_prev: Matrix[k: Integer, k: Integer]
  # @param n_movies: Scalar: Integer
  # @return tuple(mu_V, Lambda_V)
  
  nu_0_star_V <- compute_nu_0_star(nu_0_prev, n_movies)
  W_star_V <- compute_W_0_star(W_0_prev, V_prev, n_movies, mu_0_prev, beta_0_prev)
  mu_star_V <- compute_mu_0_star(beta_0_prev, mu_0_prev, V_prev, n_movies)
  
  Lambda_V <- rWishart(1, nu_0_star_V, W_star_V)[,,1]
  mu_V <- rmvnorm(1, mu_star_V, Lambda_V)
  return(list(mu_V, Lambda_V))
}

# Define helper function to retrieve movie observation mask
extract_user_observation_index <- function(R, user_index) {
  # @param R: Sparse Matrix
  # @param user_index: Integer
  # @return tuple(observations)
  return(R[R[,1] == user_index, 2])
}

extract_user_observation_ratings <- function(R, user_index) {
  # @param R: Sparse Matrix
  # @param user_index: Integer
  # @return tuple(observations)
  return(R[R[,1] == user_index, 3])
}


# Define function to sample from U_i
sample_U_i <- function(R, mu_U, Lambda_U, V, alpha, user_index, k) {
  # @param R: Sparse Matrix[user_index, movie_index, rating]
  # @param mu_U: Scalar
  # @param Lambda_U: Matrix[k, k]
  # @return U_i: Matrix[1, k]
  user_observation_mask <- extract_user_observation_index(R, user_index)
  user_ratings <- extract_user_observation_ratings(R, user_index)
  num_unobserved <- nrow(V) - length(user_observation_mask)
  V_observed <- V[user_observation_mask,,drop = FALSE]
  Lambda_i_star <- (Lambda_U + alpha * ((t(V_observed) %*% (V_observed)) + num_unobserved * diag(k)))
  
  mu_i_star <- (solve(Lambda_i_star) %*% (alpha * (t(V_observed) %*% as.matrix(user_ratings) + num_unobserved * as.matrix(rep(1, k))) + t(mu_U %*% Lambda_U)))
  
  U_i_sampled <- rmvnorm(1, mu_i_star, solve(Lambda_i_star))
  return(U_i_sampled)
}

# Define function to construct U from sampled U_i
sample_U <- function(R, mu_U, Lambda_U, V, alpha, k, n_users) {
  # @param R: Sparse Matrix[user_index, movie_index, rating]
  # @param mu_U: Scalar
  # @param Lambda_U: Matrix[k x k]
  # @param V: Matrix[n_users, k]
  # @param alpha: Scalar
  # @param k: Scalar
  # @param n_users: Scalar
  sampled_U <- c()
  for (i in 1:n_users) {
    u_i <- sample_U_i(R, mu_U, Lambda_U, V, alpha, i, k)
    sampled_U <- rbind(sampled_U, u_i)
  }
  return(sampled_U)
}

# Define helper function to retrieve user observation mask
extract_movie_observation_index <- function(R, movie_index) {
  # @param R: Sparse Matrix
  # @param movie_index: Integer
  # @return tuple(observations)
  return(R[R[,2] == movie_index, 1])
}

extract_movie_observation_ratings <- function(R, movie_index) {
  # @param R: Sparse Matrix
  # @param user_index: Integer
  # @return tuple(observations)
  return(R[R[,2] == movie_index, 3])
}


# Define function to sample from U_i
sample_V_i <- function(R, mu_V, Lambda_V, U, alpha, movie_index, k) {
  # @param R: Sparse Matrix[user_index, movie_index, rating]
  # @param mu_V: Scalar
  # @param Lambda_V: Matrix[k, k]
  # @return V_i: Matrix[1, k]
  movie_observation_mask <- extract_movie_observation_index(R, movie_index)
  movie_ratings <- extract_movie_observation_ratings(R, movie_index)
  num_unobserved <- nrow(U) - length(movie_observation_mask)
  U_observed <- U[movie_observation_mask, ,drop = FALSE]
  Lambda_i_star <- (Lambda_V + alpha * ((t(U_observed) %*% U_observed) + num_unobserved * diag(k)))
  
  mu_i_star <- (solve(Lambda_i_star) %*% (alpha * (t(U_observed) %*% as.matrix(movie_ratings) + num_unobserved * as.matrix(rep(1, k))) + t(mu_V %*% Lambda_V)))
  
  V_i_sampled <- rmvnorm(1, mu_i_star, solve(Lambda_i_star))
  return(V_i_sampled)
}

# Define function to construct U from sampled U_i
sample_V <- function(R, mu_V, Lambda_V, U, alpha, k, n_movies) {
  # @param R: Sparse Matrix[user_index, movie_index, rating]
  # @param mu_V: Scalar
  # @param Lambda_V: Matrix[k x k]
  # @param U: Matrix[n_movies, k]
  # @param alpha: Scalar
  # @param k: Scalar
  # @param n_movies: Scalar
  sampled_V <- c()
  for (i in 1:n_movies) {
    v_i <- sample_V_i(R, mu_V, Lambda_V, U, alpha, i, k)
    sampled_V <- rbind(sampled_V, v_i)
  }
  return(sampled_V)
}

extract_simulated_ratings <- function(simulation_results,
                                      user_index,
                                      movie_index) {
  #' From the BPMF_Gibbs_Sampler(), extract simulations 
  #' of the ratings given user and movie index
  #' @param simulation_results: object returned from BPMF_Gibbs_Sampler()
  #' @param user_index: integer
  #' @param movie_index: integer
  
  xs <- 500:n_replications
  sampled_rs <- xs %>% Map(function(x) {
    simulation_results$Rs[[x]][user_index, movie_index]
    }, .) %>% unlist
  posterior_rs_mean <- mean(sampled_rs)
  data.frame(
    replication_number = xs,
    simulated_rating = sampled_rs
  )
}

count_user_ratings <- function(sparse_matrix, user_index) {
  #' Given a tidy matrix with first column user_index,
  #' second column movie_index, and last column
  #' rating, return how many ratings user has given
  
  sparse_matrix %>% 
    as.data.frame() %>% 
    filter(.[[1]] == user_index) %>% 
    count() %>% 
    as.integer()
}
