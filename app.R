library(shiny)
options(shiny.launch.browser = TRUE)


teams <- unique(swac_data$team)

ui <- fluidPage(
  titlePanel("Elo Ratings Time Series"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team", choices = teams),
      width = 3
    ),
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

server <- function(input, output, session) {
  # Filter data based on selected team
  filtered_data <- reactive({
    filter(swac_data, team == input$team)
  })
  
  # Render line plot
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = season_day, y = ELO)) +
      geom_line() +
      labs(title = paste("Elo Ratings for", input$team),
           x = "Season Day",
           y = "Elo Rating")
  })
}

shinyApp(ui, server)
