# svd.R
# Functions that implements SVD

svd_solver <- function(R, k, n_users, n_movies) {
  # Gradient descent on error function
  # with respect to U and V
  #' @param R: Rating matrix in tidy format (mapped)
  #' @param k: number of latent variables
  #' @param n_users: number of users 
  #' @param n_movies: number of movies
  #' @return R_estimate: Rating estimate Matrix in tidy format
 
  # Initialize U and V
  UV_init <- initialize_UV(k, n_users, n_movies)
  U <- UV_init[[1]]
  V <- UV_init[[2]]
  
  # Flatten U_init and V_init to UV_vec_flat
  UV_vec_flat <- c((U %>% as.vector()), (V %>% as.vector()))
  n_users <- nrow(U)
  n_movies <- nrow(V)
  k_estimate <- ncol(U)
  
  # Define error function
  svd_error <- function(x) {
    # Reconstruct U_init and V_init from UV_vec_flat
    U_init <- matrix(x[1:(n_users*k_estimate)], nrow=n_users)
    V_init <- matrix(x[(n_users*k_estimate+1):(n_users*k_estimate + k_estimate*n_movies)], nrow=n_movies)
    
    # Compute observed square error
    R_estimate <- (U_init %*% t(V_init)) %>% matrix_to_tidydf() %>% rename(userId=row, movieId=col, rating=value)
    R_error <- R %>%
      inner_join(R_estimate, by = c("userId", "movieId")) %>%
      mutate(square_error=(rating.x - rating.y)^2)
    
    sum_square_error <- R_error$square_error %>% sum
    return(sum_square_error)
  }
  
  # Optimize error function parameters
  optimizer <- optim(UV_vec_flat, svd_error)
  
  # Reconstruct from optimized parameters
  U_estimate <- matrix(optimizer$par[1:(n_users*k_estimate)], nrow=n_users)
  V_estimate <- matrix(optimizer$par[(n_users*k_estimate+1):(n_users*k_estimate + k_estimate*n_movies)], nrow=n_movies)
  R_estimate <- (U_estimate %*% t(V_estimate))
  return(R_estimate)
  
}


