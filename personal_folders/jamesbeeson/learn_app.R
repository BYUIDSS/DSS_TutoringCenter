library(shiny)

# UI definition
ui <- fluidPage(
  titlePanel("Filter Dataset and Update Graph"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "min_mpg",
        label = "Minimum MPG:",
        value = NA,
        min = 0,
        step = 1
      ),
      numericInput(
        inputId = "max_hp",
        label = "Maximum Horsepower:",
        value = NA,
        min = 0,
        step = 10
      ),
      numericInput(
        inputId = "max_wt",
        label = "Maximum Weight:",
        value = NA,
        min = 0,
        step = 0.1
      )
    ),
    
    mainPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      plotOutput(outputId = "scatterPlot"),
      plotOutput(outputId = "hpScatterPlot"),
      plotOutput(outputId = "combinedScatterPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Reactive expression to filter the dataset by MPG
  filteredData <- reactive({
    if (is.na(input$min_mpg)) {
      mtcars
    } else {
      mtcars[mtcars$mpg >= input$min_mpg, ]
    }
  })
  
  # Reactive expression to filter the dataset by Horsepower
  filteredHPData <- reactive({
    if (is.na(input$max_hp)) {
      mtcars
    } else {
      mtcars[mtcars$hp <= input$max_hp, ]
    }
  })
  
  # Reactive expression to apply additional filters to filteredData
  combinedFilteredData <- reactive({
    data <- filteredData()
    if (!is.na(input$max_wt)) {
      data <- data[data$wt <= input$max_wt, ]
    }
    data
  })
  
  # Render plot based on filtered data by MPG
  output$scatterPlot <- renderPlot({
    data <- filteredData()
    
    plot(
      data$wt, data$mpg,
      xlab = "Weight (1000 lbs)",
      ylab = "Miles Per Gallon (MPG)",
      main = "Filtered Scatter Plot by MPG",
      pch = 19, col = "blue"
    )
  })
  
  # Render plot based on filtered data by Horsepower
  output$hpScatterPlot <- renderPlot({
    data <- filteredHPData()
    
    plot(
      data$wt, data$hp,
      xlab = "Weight (1000 lbs)",
      ylab = "Horsepower (HP)",
      main = "Filtered Scatter Plot by Horsepower",
      pch = 19, col = "red"
    )
  })
  
  # Render plot based on combined filtered data
  output$combinedScatterPlot <- renderPlot({
    data <- combinedFilteredData()
    
    plot(
      data$wt, data$mpg,
      xlab = "Weight (1000 lbs)",
      ylab = "Miles Per Gallon (MPG)",
      main = "Combined Filters Scatter Plot",
      pch = 19, col = "green"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
