library(shiny)
options(shiny.launch.browser = TRUE)


teams <- c("No team selected", unique(swac_data$team))
team_colors <- c(" Green Hope"="Dark Green", " Green Level"="Navy", " Panther Creek"="Sky Blue", " Middle Creek"="Black", " Holly Springs"="Purple", " Apex"="Yellow", " Apex Friendship"="Red", " Cary"="Lime Green")  # Add colors for each team

ui <- fluidPage(
  titlePanel("Elo Ratings Time Series"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team", choices = teams)
    ),
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

server <- function(input, output, session) {
  # Filter data based on selected team
  filtered_data <- reactive({
    if (input$team == "No team selected") {
      return(swac_data)
    } else {
      filter(swac_data, team == input$team)
    }
  })
  
  # Render line plot
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = season_day, y = ELO, color = team)) +
      geom_line() +
      labs(title = ifelse(input$team == "No team selected", "Elo Ratings for All Teams", paste0("Elo Ratings for", input$team)),
           x = "Season Day",
           y = "Elo Rating",
           color = "Team") +
      theme(legend.position = "bottom")+scale_color_manual(values = team_colors)
    
  })
}
shinyApp(ui, server)
