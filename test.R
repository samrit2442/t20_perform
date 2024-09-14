# Load necessary libraries
library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Value Box Example"),
  dashboardSidebar(),
  dashboardBody(
    # Place the value box output
    fluidRow(
      valueBoxOutput("valueBox1"),
      valueBoxOutput("valueBox2")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render the first value box
  output$valueBox1 <- renderValueBox({
    valueBox(
      value = 100, # The main value to display
      subtitle = "Sales Today", # Subtitle or description
      icon = icon("shopping-cart"), # Optional icon
      color = "green" # Box color (can be "red", "blue", "yellow", "green", etc.)
    )
  })

  # Render the second value box
  output$valueBox2 <- renderValueBox({
    valueBox(
      value = 250,
      subtitle = "New Users",
      icon = icon("users"),
      color = "blue"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
