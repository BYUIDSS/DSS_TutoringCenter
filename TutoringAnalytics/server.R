#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

options(shiny.maxRequestSize = 30*1024^2)

# Define server logic required to draw a histogram
function(input, output, session) {

    # Reactive expression to read the uploaded data
    data <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath)
        # Update selectizeInput choices based on uploaded data
        updateSelectizeInput(session, "courses", choices = unique(df$Course), server = TRUE) 
        
        df
    })
    
    output$avg_dur <- renderText({ 
        data = data()
        print(names(data))
        mean(data$`Visit.duration..in.minutes.`, na.rm=TRUE)
    })
}
