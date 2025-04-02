library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(shinyjs)


# Define the file to store the layout data
layout_file <- "layout_data.rds"

# Define plot limits
xlim_max <- 30
ylim_max <- 25  # Adjusted for 4:3 aspect ratio


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive Rectangle Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("label_choice", "Label for New Rectangle:",
                  choices = c("100/101", "108", "Pre-Calc", "Calc 1", "Calc 2/3", "221", "Custom"), selected = "A"),
      conditionalPanel(
        condition = "input.label_choice == 'Custom'",
        textInput("custom_label", "Enter Custom Label:")
      ),
      selectInput("layout_choice", "Choose Layout:",
                  choices = c("Layout 1", "Layout 2", "Custom"), selected = "Layout 1"),
      actionButton("save_layout", "Save Layout"),
      downloadButton("download_layout", "Download Layout"),
      textOutput("save_message"),
      checkboxInput("delete_mode", "Delete Mode", value = FALSE),
      numericInput("numericInput", "Observations:", 10, min = 1, max = 100)# Toggle switch
    ),
    
    mainPanel(
      uiOutput("plot_ui", width = "100%"),  # Take full width of the main panel
      )
  )
)


server <- function(input, output, session) {
  
  # Load layout data from file (if it exists)
  initial_layout_data <- list(
    `Layout 1` = data.frame(xmin = 1, xmax = 3, ymin = 2, ymax = 4, lab_area = "A", stringsAsFactors = FALSE),
    `Layout 2` = data.frame(xmin = 5, xmax = 7, ymin = 6, ymax = 5, lab_area = "B", stringsAsFactors = FALSE) #y limit changed
  )
  if (file.exists(layout_file)) {
    initial_layout_data <- readRDS(layout_file)
  }
  
  # Reactive value to store rectangles data
  rectangles <- reactiveVal(data.frame(xmin = numeric(), xmax = numeric(),
                                       ymin = numeric(), ymax = numeric(),
                                       lab_area = character(), stringsAsFactors = FALSE))
  
  # Reactive value to store layout data
  layout_data <- reactiveVal(initial_layout_data)  # Initialize with loaded data
  
  # Reactive value to store messages
  save_message <- reactiveVal("")
  
  # Reactive value to store the current brush (before double click)
  current_brush <- reactiveVal(NULL)
  
  
  
  # Observe layout choice and load layout data
  observeEvent(input$layout_choice, {
    if (input$layout_choice != "Custom") {
      rectangles(layout_data()[[input$layout_choice]])
    }
    else {
      rectangles(data.frame(xmin = numeric(), xmax = numeric(),
                            ymin = numeric(), ymax = numeric(),
                            lab_area = character(), stringsAsFactors = FALSE))
    }
  })
  
  observe({
    inputValue <- input$numericInput
    
    # Check if the input is not NULL and is numeric (to avoid errors on initial load or invalid input)
    if (!is.null(inputValue) && is.numeric(inputValue)) {
      roundedValue <- round(inputValue)
      
      # Update the numeric input with the rounded value
      updateNumericInput(session, "numericInput", value = roundedValue)
      
      # (Optional) Display the rounded value
      output$roundedValue <- renderText({
        paste("Rounded value:", roundedValue)
      })
    }
  })
  
  
  # Dynamically create plotOutput in the server
  output$plot_ui <- renderUI({
    plotOutput("interactive_plot",
               brush = brushOpts(id = "plot_brush", direction = "xy", resetOnNew = TRUE, fill = "skyblue", stroke = "black"),
               dblclick = "plot_dblclick",
               width = "100%",  # Take full width of the main panel
               height = "600px" # Set height to maintain aspect ratio. Adjust as needed
    )
  })
  
  
  # Observe brush events and store the brush
  observeEvent(input$plot_brush, {
    current_brush(input$plot_brush)  # Store the brush
  })
  
  
  # Observe double-click events - handles both confirmation and deletion
  observeEvent(input$plot_dblclick, {
    click <- input$plot_dblclick
    brush <- current_brush()
    
    if (input$delete_mode) {  # Delete Mode
      
      if (!is.null(click)) {
        x_click <- click$x
        y_click <- click$y
        
        # Filter out rectangles that *do not* intersect the double-click point
        rectangles(
          rectangles() %>%
            filter(
              !(x_click >= xmin & x_click <= xmax &
                  y_click >= ymin & y_click <= ymax)
            )
        )
      }
      current_brush(NULL) # Clear brush in delete mode
      
    } else {  # Confirmation Mode (Create Rectangle)
      
      if (!is.null(brush)) {
        # Get the label based on user input
        lab_area <- if (input$label_choice == "Custom") {
          input$custom_label
        } else {
          input$label_choice
        }
        
        # Round the brush coordinates
        xmin_rounded <- round(brush$xmin * 2) / 2
        xmax_rounded <- round(brush$xmax * 2) / 2
        ymin_rounded <- round(brush$ymin * 2) / 2
        ymax_rounded <- round(brush$ymax * 2) / 2
        
        # Adjust for exceeding plot limits
        xmin_rounded <- max(0, xmin_rounded) # Ensure xmin is not less than 0
        xmax_rounded <- min(xlim_max, xmax_rounded) # Ensure xmax is not greater than xlim_max
        ymin_rounded <- max(0, ymin_rounded) # Ensure ymin is not less than 0
        ymax_rounded <- min(ylim_max, ymax_rounded) # Ensure ymax is not greater than ylim_max
        
        
        # Validate brush dimensions
        if (xmax_rounded - xmin_rounded > 0 && ymax_rounded - ymin_rounded > 0) {
          
          # Check for overlap with existing rectangles
          overlap <- rectangles() %>%
            rowwise() %>%
            filter(
              xmin_rounded < (xmax + 0.25) &&
                xmax_rounded > (xmin - 0.25) &&
                ymin_rounded < (ymax + 0.25) &&
                ymax_rounded > (ymin - 0.25)
            )
          
          if (nrow(overlap) > 0) { # Overlap detected
            
            # Show a modal popup
            showModal(modalDialog(
              title = "Overlap Detected",
              "Areas cannot overlap. Please draw a new rectangle in a non-overlapping area.",
              easyClose = TRUE,
              footer = NULL  # Remove default OK button
            ))
            
          } else if (abs(xmin_rounded - xmax_rounded) < 2 | abs(ymin_rounded - ymax_rounded) < 2)  {
            showModal(modalDialog(
              title = "Area too Small",
              "Areas must be larger than 2x2. Please draw a new rectangle that is at least this large.",
              easyClose = TRUE,
              footer = NULL  # Remove default OK button
            ))
          } else {
            
            new_rect <- data.frame(
              xmin = xmin_rounded,
              xmax = xmax_rounded,
              ymin = ymin_rounded,
              ymax = ymax_rounded,
              lab_area = lab_area
            )
            
            rectangles(rbind(rectangles(), new_rect))  # Append to reactive dataframe
          }
        }
        
        current_brush(NULL) # Clear the current brush after confirmation
      }
    }
  })
  
  
  # Observe Save Layout button click
  observeEvent(input$save_layout, {
    layout_name <- input$layout_choice
    if (layout_name == "Custom"){
      save_message("Cannot save the layout because custom mode is selected. Select another Layout to save to.")
    } else {
      # Safely access and update the list
      current_layouts <- layout_data()
      current_layouts[[layout_name]] <- rectangles()
      layout_data(current_layouts)
      save_message("Layout saved!")
      
      # Save the layout data to a file
      saveRDS(current_layouts, layout_file)  # Save to disk
    }
  })
  
  # Provide Download Layout functionality
  output$download_layout <- downloadHandler(
    filename = function() {
      paste("layout-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (input$layout_choice == "Custom") {
        write.csv(rectangles(), file, row.names = FALSE)
      }
      else {
        write.csv(layout_data()[[input$layout_choice]], file, row.names = FALSE)
      }
    }
  )
  
  output$save_message <- renderText({
    save_message()
  })
  
  # Save layout data on session end (optional, but recommended)
  session$onSessionEnded(function() {
    saveRDS(layout_data(), layout_file) # Save when session ends
  })
  
  output$interactive_plot <- renderPlot({
    initial_data <- if (input$layout_choice != "Custom") {
      layout_data()[[input$layout_choice]]
    } else {
      data.frame(xmin = numeric(), xmax = numeric(),
                 ymin = numeric(), ymax = numeric(),
                 lab_area = character(), stringsAsFactors = FALSE)
    }
    
    ggplot() +
      geom_rect(data = rectangles(),  # Use reactive data here
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA,  # No fill
                color = "black",
                linewidth = 2) +  # Black outline
      geom_text(data = rectangles(),
                aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = lab_area),
                size = 5) +
      xlim(0, xlim_max) +
      ylim(0, ylim_max) +
      theme_bw() +
      labs(title = "Interactive Rectangle Plot")
    
  })
  
  
}

shinyApp(ui = ui, server = server)