

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
              width = 12,
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
                  h3("Observed Ratings"),
                  plotlyOutput("observed_ratings"),
                  textOutput("testing")
                ),
                column(
                  width = 6,
                  h3("Simulated Mean Ratings"),
                  plotlyOutput("simulated_ratings_mean")
                )
              )
            ),
            # Clicked Reactives
            fluidRow(
              box(
                width = 12,
                title = "Density Comparisons",
                p("Green indicates observed, Blue indicates Posterior mean"),
                fluidRow(
                  column(
                    width = 4,
                    radioButtons(
                      inputId = "density_type",
                      choices = c("Rating", "Score"),
                      selected = "Score",
                      label = "Select for Distribution of Rating or Score",
                      inline = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    p("Click the Simulated Mean Ratings block to view a density"),
                    plotOutput("rating_density")
                  )
                )
              )
            )
          )
        )
      )
  )