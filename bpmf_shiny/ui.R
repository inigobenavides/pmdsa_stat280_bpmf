

ui <- 
  dashboardPage(
    title = "BPMF Visualiser", skin = "black",
    
    header = dashboardHeader(
      title = "BPMF Visualiser"
    ),
    
    sidebar = dashboardSidebar(disable = TRUE),
    
    body = 
      dashboardBody(
        tags$head(
          tags$link(rel = 'stylesheet', type = 'text/css', href = "fc.css")
        ),
        fluidRow(
          # Inputs
          column(
            width = 3,
            box(
              title = "Input Panel",
              width = 12,
              hr(),
              h3("Controls"),
              div(
                style = "text-align:center",
                actionButton(
                  inputId = "generate_simulations",
                  label = "Generate Simulations"
                ),
                radioButtons(
                  inputId = "remove_text",
                  label = "Remove Text",
                  choices = c("Yes", "No"),
                  inline = TRUE,
                  selected = "No"
                )
              ),
              hr(),
              h3("Simulation Setup"),
              sliderInput(
                inputId = "hyperparameter_k",
                label = "K hyperparameter",
                min = 1, max = 10, value = 5
              ),
              numericInput(
                inputId = "num_simulations",
                label = "Number of Simulations",
                value = 1500
              ),
              h3("Others"),
              actionButton(
                inputId = "debug",
                label = "Debug"
              )
            )
          ),
          # Visuals
          column(
            width = 9,
            fluidRow(
              box(
                width = 12,
                column(
                  width = 6,
                  tabsetPanel(
                    tabPanel(
                      title = "Observed Ratings",
                      plotlyOutput("observed_ratings")
                    )
                  )
                ),
                column(
                  width = 6,
                  tabsetPanel(
                    tabPanel(
                      title = "Mean Ratings Plot",
                      plotlyOutput("simulated_ratings_mean")
                    ),
                    tabPanel(
                      title = "Difference Plot",
                      plotlyOutput("difference_plot")
                    )
                  )
                )
              )
            ),
            # Clicked Reactives
            fluidRow(
              box(
                width = 12,
                title = "Summary Comparisons",
                p("Green indicates observed, Blue indicates Posterior mean"),
                fluidRow(
                  column(
                    width = 4,
                    uiOutput("summary_ui")
                  ),
                  column(
                    width = 8,
                    p("Click the Simulated Mean Ratings block to view a density"),
                    radioButtons(
                      inputId = "density_type",
                      choices = c("Rating", "Score"),
                      selected = "Rating",
                      label = "Select for Distribution of Rating or Score",
                      inline = TRUE
                    ),
                    plotOutput("rating_density")
                  )
                )
              )
            )
          )
        )
      )
  )