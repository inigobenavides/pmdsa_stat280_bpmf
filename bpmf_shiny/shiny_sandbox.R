ratings <- read_feather("../rating.feather")

source('global.R')

sample_ratings_top_users(ratings, n_users = 10,
                         n_movies = 10, 
                         max_num_users_per_movie = 7,
                         min_num_users_per_movie = 3) %>% 
  saveRDS("dummy_data/netflix_sample.rds")

sample_ratings_top_users(ratings, n_users = 30,
                         n_movies = 30, 
                         max_num_users_per_movie = 20,
                         min_num_users_per_movie = 10) %>% 
  saveRDS("dummy_data/netflix_sample_large.rds")



# sandbox -----------------------------------------------------------------

# Controls for Visualising Observed Matrix
output$matrix_controls_ui <- renderUI({
  req(observed.dt())
  
  tagList(
    sliderInput(
      inputId = "user_range",
      label = "Select Range of Users to Visualise",
      min = 1, max = length(unique(observed.dt()$row)), 
      value = min(10, length(unique(observed.dt()$row)))
    ),
    sliderInput(
      inputId = "movie_range",
      label = "Select Range of Users to Visualise",
      min = 1, max = length(unique(observed.dt()$col)), 
      value = min(10, length(unique(observed.dt()$col)))
    )
  )
})
