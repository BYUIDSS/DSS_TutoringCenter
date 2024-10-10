library(shiny)

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Basic R Shiny App"),
  
  # Sidebar layout with input and output
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Slider for selecting a number
      sliderInput("num", 
                  "Choose a number:", 
                  min = 1, 
                  max = 100, 
                  value = 50)
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Display the chosen number
      textOutput("chosen_num")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Output: Display the chosen number
  output$chosen_num <- renderText({
    paste("You selected:", input$num)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

