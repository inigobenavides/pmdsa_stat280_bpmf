library(tidyverse)
library(ggplot2)
library(glue)

source("code/visualisations.R")
source("code/utils.R")

sparse.dt <- readRDS("sparse_seed_42.rds")
non_sparse.dt <- readRDS("non_sparse_seed_42.rds")


# Generate Visual ---------------------------------------------------------

nrow(non_sparse.dt)/(30 * 30)

non_sparse.dt %>% 
  mutate(row = as.numeric(factor(row, labels = 1:30))) %>% 
  mutate(col = as.numeric(factor(col, labels = 1:30))) %>% 
  # mutate(value = sapply(value, transform_rating_to_score))
  vis_matrix(row_col = "row", column_col = "col", value_col = "value") +
  labs(x = "Movie", y = "User") + theme(legend.position = "none")

sparse.dt %>% 
  mutate(row = as.numeric(factor(row, labels = 1:30))) %>% 
  mutate(col = as.numeric(factor(col, labels = 1:30))) %>% 
  # mutate(value = sapply(value, transform_rating_to_score))
  vis_matrix(row_col = "row", column_col = "col", value_col = "value") +
  labs(x = "Movie", y = "User") + theme(legend.position = "none")
