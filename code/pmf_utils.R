# pmf_utils.R
# Defines helper functions to implement in PMF Gibbs Sampler
source("code/bpmf_utils.R")

sample_U_i_pmf <- function(R, V, sigma_U, sigma, user_index) {
  # Samples U_i for PMF Gibbs sampler
  #' @param R: ratings matrix in tidy format
  #' @param V: latent movie matrix
  #' @param sigma_U: prior precision on latent user vectors
  #' @param sigma: precision for ratings
  #' @param user_index: index of user to compute
  #' @return U_i_sampled
  
  user_observation_mask <- extract_user_observation_index(R, user_index)
  user_ratings <- extract_user_observation_ratings(R, user_index)
  num_unobserved <- nrow(V) - length(user_observation_mask)
  V_observed <- V[user_observation_mask,,drop = FALSE]
  k <- ncol(V)
  
  Lambda_i_star <- (sigma_U * diag(k) +
                      sigma * ((t(V_observed) %*% (V_observed)) + num_unobserved * diag(k)))
  
  mu_i_star <- (solve(Lambda_i_star) %*% (sigma * (t(V_observed) %*% as.matrix(user_ratings) + 
                                                     num_unobserved * as.matrix(rep(1, k)))))
  
  U_i_sampled <- rmvnorm(1, mu_i_star, solve(Lambda_i_star))
  return(U_i_sampled)
}

sample_U_pmf <- function(R, V, sigma_U, sigma, n_users) {
  #' @param R: ratings matrix in tidy format
  #' @param V: latent movie matrix
  #' @param sigma_U: prior precision on latent user vectors
  #' @param sigma: precision for ratings
  #' @param n_users: number of users 
  #' @return sampled_U

  sampled_U <- c()
  for (i in 1:n_users) {
    u_i <- sample_U_i_pmf(R, V, sigma_U, sigma, i)
    sampled_U <- rbind(sampled_U, u_i)
  }
  return(sampled_U)
}

sample_V_i_pmf <- function(R, U, sigma_V, sigma, movie_index) {
  # Samples V_i for PMF Gibbs sampler
  #' @param R: ratings matrix in tidy format
  #' @param U: latent user matrix
  #' @param sigma_V: prior precision on latent movie vectors
  #' @param sigma: precision for ratings
  #' @param movie_index: index of user to compute
  #' @return V_i_sampled
  movie_observation_mask <- extract_movie_observation_index(R, movie_index)
  movie_ratings <- extract_movie_observation_ratings(R, movie_index)
  num_unobserved <- nrow(U) - length(movie_observation_mask)
  U_observed <- U[movie_observation_mask, ,drop = FALSE]
  k <- ncol(U)
  
  Lambda_i_star <- (sigma_V * diag(k) +
                      sigma * ((t(U_observed) %*% U_observed) + num_unobserved * diag(k)))
  
  mu_i_star <- (solve(Lambda_i_star) %*% (sigma * (t(U_observed) %*% as.matrix(movie_ratings) +
                                                     num_unobserved * as.matrix(rep(1, k)))))
  
  V_i_sampled <- rmvnorm(1, mu_i_star, solve(Lambda_i_star))
  return(V_i_sampled)
}

sample_V_pmf <- function(R, U, sigma_V, sigma, n_movies) {
  #' @param R: ratings matrix in tidy format
  #' @param U: latent movie matrix
  #' @param sigma_V: prior precision on latent movie vectors
  #' @param sigma: precision for ratings
  #' @param n_movies: number of movies 
  #' @return sampled_V
  
  sampled_V <- c()
  for (i in 1:n_movies) {
    v_i <- sample_V_i_pmf(R, U, sigma_V, sigma, i)
    sampled_V <- rbind(sampled_V, v_i)
  }
  return(sampled_V)
}
