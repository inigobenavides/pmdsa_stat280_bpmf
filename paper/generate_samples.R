
# Preparations ------------------------------------------------------------

# Load Necessary Functions 
source("code/sample_utils.R")
source("code/utils.R")

# Load Libraries
library(magrittr)
library(tidyverse)
library(feather)
library(glue)

# Read File ---------------------------------------------------------------

# Ensure that rating.feather file is in root folder
ratings <- read_feather("rating.feather")

# Sample ------------------------------------------------------------------

seed <- 42
set.seed(seed)

n_users <- 30
n_movies <- 30

# Non-sparse
non_sparse.dt <- sample_ratings_top_users(ratings, n_users = n_users,
                         n_movies = n_movies, 
                         max_num_users_per_movie = 20,
                         min_num_users_per_movie = 15)

# Measuring Sparsity
full.sparsity <- nrow(non_sparse.dt)/(n_users * n_movies)
decreased.sparsity <- full.sparsity * 0.5
num_ratings_sparse <- round(decreased.sparsity * (n_users * n_movies))

# Repeat sampling until desired sparsity is achieved without losing number of users and movies
repeat {
  sparse.dt <- non_sparse.dt %>% 
    sample_n(num_ratings_sparse)
  
  # Check
  check <- length(unique(sparse.dt$row)) == n_users & length(unique(sparse.dt$col)) == n_movies
  
  if (check) break
}

nrow(sparse.dt)/(n_users * n_movies)

# Write RDS ---------------------------------------------------------------

non_sparse.dt %>% saveRDS(glue("non_sparse_seed_{seed}.rds"))
sparse.dt %>% saveRDS(glue("sparse_seed_{seed}.rds"))
