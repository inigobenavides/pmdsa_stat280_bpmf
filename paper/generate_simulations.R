library(tidyverse)
library(ggplot2)
library(glue)
library(mvtnorm)
library(rlang)

source("code/visualisations.R")
source("code/utils.R")
source("code/bpmf_gibbs_sampler.R")
source("code/bpmf_utils.R")

sparse.dt <- readRDS("sparse_seed_42.rds")
non_sparse.dt <- readRDS("non_sparse_seed_42.rds")

# Generate BPMF -----------------------------------------------------------

set.seed(42)

observed.dt <- sparse.dt %>% 
  mutate(row = as.numeric(factor(row, labels = 1:30))) %>% 
  mutate(col = as.numeric(factor(col, labels = 1:30))) %>% 
  mutate(value = sapply(value, transform_rating_to_score))

# Implement Gibbs sampler
n_replications <- 1500

# Define initial parameters
k_estimate <- 10
alpha <- 10
mu_0 <- rep(0, k_estimate)
beta_0 <- 1
nu_0 <- 1
W_0 <- diag(k_estimate)
n_users <- length(unique(observed.dt$row))
n_movies <- length(unique(observed.dt$col))

# Run
simulation_results <- BPMF_Gibbs_Sampler(
  observed.dt, 
  k_estimate, 
  n_replications, 
  n_users, 
  n_movies, 
  mu_0, beta_0, nu_0, W_0, alpha
)

# Take simulated ratings for each user 
predictions.dt <- map2_df(
  .x = expand.grid(1:n_users, 1:n_movies)$Var1,
  .y = expand.grid(1:n_users, 1:n_movies)$Var2,
  .f = function(x,y) {
    extract_simulated_ratings(simulation_results, x,y) %>% 
      mutate(user_index = x) %>% 
      mutate(movie_index = y)
  }
)

# Simulated Mean Ratings --------------------------------------------------

prediction_mean_ratings.dt <- predictions.dt %>% 
  group_by(user_index, movie_index) %>% 
  summarise(mean_rating = mean(simulated_rating)) %>% 
  ungroup() %>% 
  mutate(mean_rating = sapply(mean_rating, transform_score_to_rating))

# Visual For Simulated Mean Ratings ---------------------------------------

prediction_mean_ratings.dt %>% 
  vis_matrix(
    row_col = "user_index",
    column_col = "movie_index",
    value_col = "mean_rating"
  ) + labs(x = "Movie", y = "User") +
    theme(legend.position = "none")


# Difference Matrix -------------------------------------------------------

difference.dt <- compute_matrix_difference(
  subtrahend = observed.dt %>% 
    mutate(value = sapply(value, transform_score_to_rating)),
  minuend =  predictions.dt %>% 
    group_by(user_index, movie_index) %>% 
    summarise(mean_rating = mean(simulated_rating)) %>% 
    ungroup() %>% 
    mutate(mean_rating = sapply(mean_rating, transform_score_to_rating)),
  subtrahend_row_col = "row",
  subtrahend_column_col = "col",
  subtrahend_value_col = "value",
  minuend_row_col = "user_index",
  minuend_column_col = "movie_index",
  minuend_value_col = "mean_rating"
)


# Visual for Difference ---------------------------------------------------

difference.dt %>% 
  vis_matrix(
    row_col = "row",
    column_col = "col",
    value_col = "difference"
  ) + labs(x = "Movie", y = "User") +
  theme(legend.position = "none")


# Sample Distribution -----------------------------------------------------

predictions.dt %>% 
  filter(user_index == 14 & movie_index == 3) %>% 
  mutate(simulated_rating = sapply(simulated_rating, transform_score_to_rating)) %>% 
  ggplot() + geom_density(mapping = aes(x = simulated_rating), fill = "green", alpha = 0.3) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5), limits = c(0, 5)) +
  theme_minimal() + 
  geom_vline(
    data = function(data) {data %>% summarise(mean = mean(simulated_rating))},
    mapping = aes(xintercept = mean), color = "blue"
  ) +
  geom_vline(
    data = observed.dt %>% filter(row == 14 & col == 3) %>% 
      mutate(value = sapply(value, transform_score_to_rating)),
    mapping = aes(xintercept = value), color = "red"
  )


# Related Ratings vs Sd (unobserved) --------------------------------------

observed.dt %>% 
  complete(row, col) %>%
  # Add number of available users who rated movie
  group_by(col) %>% 
  mutate(number_of_users_who_rated = length(unique(row[!is.na(value)]))) %>% 
  ungroup() %>% 
  group_by(row) %>% 
  mutate(number_of_movies_rated = length(unique(col[!is.na(value)]))) %>% 
  ungroup() %>% 
  mutate(total_related_ratings = number_of_users_who_rated + number_of_movies_rated) %>%
  mutate(total_related_ratings = ifelse(!is.na(value), total_related_ratings - 2, total_related_ratings)) %>%
  select(row, col, value, total_related_ratings) %>% 
  inner_join(
    predictions.dt %>% 
      group_by(user_index, movie_index) %>% 
      summarise(sd = sd(simulated_rating)),
    by = c("row" = "user_index", "col" = "movie_index")
  ) %>% 
  mutate(is_observed = !is.na(value)) %>% 
  filter(!is_observed) %>% 
  ggplot(mapping = aes(x = total_related_ratings, y = sd)) +
  geom_point() + geom_smooth(method = "lm")


# Squared Difference Decreases the more Related Ratings there are ---------

observed.dt %>% 
  mutate(value = sapply(value, transform_score_to_rating)) %>% 
  inner_join(
    predictions.dt %>% group_by(user_index, movie_index) %>% 
      summarise(mean_rating = mean(simulated_rating)) %>% 
      ungroup() %>% 
      mutate(mean_rating = sapply(mean_rating, transform_score_to_rating)),
    c("row" = "user_index", "col" = "movie_index")
  ) %>% 
  # Add number of available users who rated movie
  group_by(col) %>% 
  mutate(number_of_users_who_rated = length(unique(row))) %>% 
  ungroup() %>% 
  group_by(row) %>% 
  mutate(number_of_movies_rated = length(unique(col))) %>% 
  ungroup() %>% 
  mutate(total_related_ratings = number_of_users_who_rated + number_of_movies_rated - 2) %>% 
  # Difference
  mutate(squared_difference = (value - mean_rating)^2) %>% 
  ggplot(mapping = aes(x = total_related_ratings, y = squared_difference)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Total Related Ratings", y = "Squared Difference")
