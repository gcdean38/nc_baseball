library(shiny)
options(shiny.launch.browser = TRUE)
library(rsconnect)
library(ggplot2)
#rsconnect::setAccountInfo(name='n8qea1-geoffrey-dean', token='54D148B5AEDE2FD8CD5008D33B027EB5', secret='VQc007IkmWCQ/elz8If6WhlwN3iwIgtL+R8/1gcD')


#teams <- c("No team selected", unique(swac_data$team))
team_colors <- c(" Green Hope"="#3c6a3e", " Green Level"="#6eac3c", " Panther Creek"="#6e8ab0",
                 " Middle Creek"="black", " Holly Springs"="purple", 
                 " Apex"="#ffc72c", " Apex Friendship"="#EA0029", " Cary"="#00AE42",
                 " Heritage"="#c7d744"," Knightdale" = "#bfb25f", " Rolesville" = "#EA0029",
                 " Wake Forest"= "#002169"," Millbrook"="#689ccf"," Wakefield" = "#660122",
                 " Cardinal Gibbons" = "#447843", " Sanderson"="#EA0029"," Enloe"="#376014",
                 " Athens Drive"="#d5762d"," Broughton"="#4c3484"," Leesville Road"="#142957",
                 " Jordan" = "#cb2129"," Chapel Hill"="#d4aa27"," East Chapel Hill"="#c8c8c8",
                 " Hillside"="#5189c5"," Riverside-Durham"="#440081",
                 " Willow Springs"="#f5e235"," Clayton"="#5a7abb"," Cleveland"="#202348",
                 " Fuquay-Varina"="#dba329"," Garner"="#ecb941", " Corinth Holders"="#532782",
                 " South Garner"="#798187"," Southeast Raleigh"="#2b4811")  # Add colors for each team

library(shiny)

# Sample data for SWAC, CAP 6, and DC 6 conferences

ui <- fluidPage(
  titlePanel("Elo Ratings Time Series"),
  sidebarLayout(
    sidebarPanel(
      selectInput("conference", "Select Conference", choices = c("Select Conference",unique(big_five_data$conference))),
      uiOutput("teamSelector")
    ),
    mainPanel(
      plotOutput("line_plot", height = "600px")  # Set the height of the plot area here
      
    )
  )
)

server <- function(input, output, session) {
  # Define teams based on the selected conference
  conference_teams <- reactive({
    req(input$conference)
    unique(big_five_data$team[big_five_data$conference == input$conference])
  })
  
  # Render team selector based on the selected conference
  output$teamSelector <- renderUI({
    selectInput("team", "Select Team", choices = c("No team selected", conference_teams()))
  })
  
  # Filter data based on selected conference and team
  filtered_data <- reactive({
    req(input$team)
    if (input$conference == "Select Conference") {
      big_five_data
    } else if (input$team == "No team selected") {
      filter(big_five_data, conference == input$conference)
    } else {
      filter(big_five_data, conference == input$conference, team == input$team)
    }
  })
  
  # Render line plot
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.Date(game_date), y = ELO, color = team)) +
      geom_line(linewidth=1.5) +
      labs(title = ifelse(input$team == "No team selected", paste("Elo Ratings for", input$conference), paste("Elo Ratings for", input$team)),
           x = "Season Day",
           y = "Elo Rating",
           color = "Team") +
      theme(legend.position = "bottom") +
      scale_color_manual(values = team_colors)
  })
}

shinyApp(ui, server)

