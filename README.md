# Bayesian Probabilistic Matrix Factorization via Gibbs Sampling

This project implements Salakhutdinov and Mnih's [*Bayesian Probabilistic Matrix Factorization using Markov Chain Monte Carlo*](https://www.cs.toronto.edu/~amnih/papers/bpmf.pdf) (2008) model in R. We use the [MovieLens (20M)](https://www.kaggle.com/grouplens/movielens-20m-dataset/data#), which contains 20,000,263 ratings across 27,278 movies from 138,493 users.

## Data Setup

1) Add `ratings.feather` into base folder.

## How to run bpmf_shiny

1) Create `dummy_data` folder in `bpmf_shiny` folder
2) Open project of rmd, run the chunk for simulations
2.5) Run `bpmf_shiny/shiny_sandbox.R` to generate data for netflix sample
3) Open project of Shiny
4) Run Shiny App