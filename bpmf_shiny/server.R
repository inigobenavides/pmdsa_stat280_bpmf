


# Server 

server <- function(input, output, session) {
  
  # Read Files 
  observed.dt <- reactive({
    readRDS("dummy_data/synthetic_simulations_sparse.rds")
  })
  
  # Visualising the observed matrix
  output$observed_ratings <- renderPlotly({
    ggplotly({
      observed.dt() %>% 
        mutate(rating = sapply(value, transform_score_to_rating)) %>% 
        vis_matrix(
          row_col = "row", 
          column_col = "col",
          value_col = "rating"
        ) + labs(x = "Movie", y = "User") +
        theme(legend.position = "none")
    })
  })
  
  # Generating the simulations ---------------
  simulations.dt <- eventReactive(input$generate_simulations, {
    req(observed.dt())
    
    withProgress(message = "Starting Generation", {
      # Implement Gibbs sampler
      n_replications <- 1000
      
      # Define initial parameters
      k_estimate <- 3
      alpha <- 10
      mu_0 <- rep(0, k_estimate)
      beta_0 <- 1
      nu_0 <- 1
      W_0 <- diag(k_estimate)
      n_users <- length(unique(observed.dt()$row))
      n_movies <- length(unique(observed.dt()$col))
      
      incProgress(amount = 0.5, message = "Generating BMPF Samples")
      
      # Run
      simulation_results <- BPMF_Gibbs_Sampler(
        observed.dt(), 
        k_estimate, 
        n_replications, 
        n_users, 
        n_movies, 
        mu_0, beta_0, nu_0, W_0, alpha
      )
      
      incProgress(amount = 0.25, message = "Forming Data")
      
      # Take simulated ratings for each user 
      output <- map2_df(
        .x = expand.grid(1:n_users, 1:n_movies)$Var1,
        .y = expand.grid(1:n_users, 1:n_movies)$Var2,
        .f = function(x,y) {
          extract_simulated_ratings(simulation_results, x,y) %>% 
            mutate(user_index = x) %>% 
            mutate(movie_index = y)
        }
      )
      
      incProgress(amount = 0.25, message = "Finished!")
    })
    
    # Should be a dataframe with user_index, movie_index, score
    return(output)
  })
  
  # Visual for Generated Ratings 
  output$simulated_ratings_mean <- renderPlotly({
    req(simulations.dt())
    
    ggplotly({
      simulations.dt() %>% 
        group_by(user_index, movie_index) %>% 
        summarise(mean_rating = mean(simulated_rating)) %>% 
        ungroup() %>% 
        mutate(mean_rating = sapply(mean_rating, transform_score_to_rating)) %>% 
        vis_matrix(
          row_col = "user_index",
          column_col = "movie_index",
          value_col = "mean_rating"
        ) + labs(x = "Movie", y = "User") +
        theme(legend.position = "none")
    }, source = "full_matrix")
  })
  
  # Detect clicks
  selected_datapoint <- reactive({
    event.data <- event_data("plotly_click", source = "full_matrix")
    list(
      user = length(unique(observed.dt()$row)) - as.numeric(event.data$pointNumber[[1]][1]),
      movie = as.numeric(event.data$pointNumber[[1]][2]) + 1
    )
  })  
  
  # Visuals For density comparison -----------------
  output$rating_density <- renderPlot({
    req(simulations.dt())
    req(observed.dt())
    req(selected_datapoint())
    req(input$density_type)
    
    if(length(selected_datapoint()$user)==0){return(NULL)}
    
    density_type <- input$density_type
    
    modified_table_simulated <- simulations.dt() %>% 
      filter(user_index == selected_datapoint()$user) %>% 
      filter(movie_index == selected_datapoint()$movie) %>% 
      rowwise() %>% 
      mutate(rating = ifelse(
        density_type == "Score",
        simulated_rating,
        transform_score_to_rating(simulated_rating)
      )) %>% 
      ungroup()
    
    modified_table_observed <- observed.dt() %>% 
      filter(row == selected_datapoint()$user) %>% 
      filter(col == selected_datapoint()$movie) %>% 
      rowwise() %>% 
      mutate(rating = ifelse(
        density_type == "Score",
        value,
        transform_score_to_rating(value)
      )) %>% 
      ungroup()
    
    withProgress({
      incProgress(amount = 0.4, message = "Generating Plot")
      
      plot <- modified_table_simulated %>% 
        ggplot() +
        geom_density(
          mapping = aes(x = rating)
        ) + 
        geom_vline(
          data = function(data) {data %>% summarise(mean = mean(rating))},
          mapping = aes(xintercept = mean),
          color = "blue"
        ) +
        geom_vline(
          data = modified_table_observed,
          mapping = aes(xintercept = rating),
          color = "green"
        ) +
        theme_bw() +
        labs(
          x = ifelse(density_type == "Score", "Score", "Rating"),
          title = glue("User: {selected_datapoint()$user}; Movie: {selected_datapoint()$movie}")
        )
      
      if(density_type == "Score") {
        plot <- plot + 
          scale_x_continuous(limits = c(-15, 15))
      } else {
        plot <- plot +
          scale_x_continuous(limits = c(0.5, 5))
      }
      
      incProgress(amount = 0.6, message = "Finished!")
    })
    
    return(plot)
  })
}