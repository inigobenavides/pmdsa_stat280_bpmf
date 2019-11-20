# pmf.R
# Functions that implement probabilistic matrix factorization
# source("code/utils.R")
# source("code/bpmf_gibbs_sampler.R")

# Example
# true_matrix <- matrix(c(9, 10, 11, 4, 5, 10, 1, 3, 5, 7, 5, 4), nrow=3)
# 
# # Sample observations
# sample_entries <- true_matrix %>% matrix_to_tidydf %>% group_by(row) %>% sample_n(3)
# 
# # Initialize U and V
# k_estimate <- 2
# n_users <- dim(true_matrix)[1]
# n_movies <- dim(true_matrix)[2]
# UV <- initialize_UV(k_estimate, n_users, n_movies)
# U_init <- UV[[1]]
# V_init <- UV[[2]]

pmf_solver <- function(R, U, V) {
  # Gradient descent on error function
  # with respect to U and V
  #' @param R: Rating matrix in tidy format
  #' @param U: Matrix[n_users x k]
  #' @param V: Matrix[n_movies x k]
  #' @return R_estimate: Rating estimate Matrix in tidy format

  # Flatten U_init and V_init to UV_vec_flat
  UV_vec_flat <- c((U %>% as.vector()), (V %>% as.vector()))
  n_users <- nrow(U)
  n_movies <- nrow(V)
  
  # Define error function
  pmf_error <- function(x) {
    # Reconstruct U_init and V_init from UV_vec_flat
    U_init <- matrix(x[1:(n_users*k_estimate)], nrow=n_users)
    V_init <- matrix(x[(n_users*k_estimate+1):(n_users*k_estimate + k_estimate*n_movies)], nrow=n_movies)
    
    # Compute observed square error
    R_estimate <- (U %*% t(V)) %>% matrix_to_tidydf()
    R_error <- sample_entries %>%
      inner_join(R_estimate, by = c("row", "col")) %>%
      mutate(square_error=(value.x - value.y)^2)
    
    sum_square_error <- R_error$square_error %>% sum
    return(sum_square_error)
  }
  
  # Optimize error function parameters
  optimizer <- optim(UV_vec_flat, pmf_error)
  
  # Reconstruct from optimized parameters
  U_estimate <- matrix(optimizer$par[1:(n_users*k_estimate)], nrow=n_users)
  V_estimate <- matrix(optimizer$par[(n_users*k_estimate+1):(n_users*k_estimate + k_estimate*n_movies)], nrow=n_movies)
  R_estimate <- (U_estimate %*% t(V_estimate))
  return(R_estimate)
  
}

# pmf_solver(sample_entries, U_init, V_init)

