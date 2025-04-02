#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(ggridges)
library(DT)
library(shinyWidgets)
library(ggforce)
library(plotly)

options(shiny.maxRequestSize = 30*1024^2)


MathCourses <- list(
    Foundational = list("MATH 100A","MATH 100B","MATH 101","MATH 108X","MATH 108Y"),
    Precalculus = list("MATH 109","MATH 110X","MATH 111"),
    Calculus = list("MATH 112X","MATH 113","MATH 214","MATH 214E","MATH 215","MATH 472"),
    AppliedMath  = list("MATH 119","MATH 282","MATH 283","MATH 284"),
    Statistics = list("MATH 124","MATH 221A","MATH 221B","MATH 221C","MATH 221D","MATH 325","MATH 326","MATH 330","MATH 423","MATH 424","MATH 425","MATH 428","MATH 494R","MATH 488"),
    LinearAlgebra = list("MATH 241","MATH 316","MATH 341","MATH 443"),
    TeacherEducation = list("MATH 205","MATH 206","MATH 275","MATH 390","MATH 490","MATH 491"),
    AdvancedMath  = list("MATH 301","MATH 340","MATH 350","MATH 411","MATH 412","MATH 440","MATH 441","MATH 442","MATH 461","MATH 462","MATH 463"),
    OtherMath  = list("MATH 190","MATH 191","MATH 280","MATH 391","MATH 450","MATH 295R","MATH 495R","MATH 498R","MATH 499R")
)

NotableCourses <- list(
    # Add A new group as follows:
    # NameOfGroup = list("Value1", "Value2")
    Economics = list("ECON 150", "ECON 151", "ECON 215", "ECON 278", "ECON 455", "ECON 380", "ECON 381", "ECON 388", "ECON 476")
)


# Define the file to store the layout data
layout_file <- "layout_data.rds"

# Define plot limits
xlim_max <- 30
ylim_max <- 25  # Adjusted for 4:3 aspect ratio


# Define server logic required to draw a histogram
function(input, output, session) {
    
# Stats ####
    
## Wrangling ####

    # Reactive expression to read the uploaded data
    data <- reactive({
        req(input$file1)
        df <- read_csv(input$file1$datapath)
        
        # Prettifies and adds function
        df <- df %>% rename(
            course = Course,
            professor = Professor,
            class = "Student's class",
            duration = "Visit duration (in minutes)",
            datetime = "Date (of the appointment/visit)",
            type = "Status (of the appointment/visit)",
            appt_id = "Appointment Extended properties Id",
            student_id = ID
        ) %>% 
            mutate(
                id = seq_len(n()), # id added to fix waterfall chart
                wday = wday(datetime, label = TRUE, abbr = TRUE),
                Week = week(datetime),
                Year = year(datetime),
                Semester = case_when(
                    Week <= 15 ~ 'Winter',
                    Week > 15 & Week <= 30 ~ 'Spring',
                    Week > 30 & Week <= 36 ~ 'Summer',
                    Week > 36 & Week <= 54 ~ 'Fall'
                ),
                Week_of_Sem = case_when(
                    Semester == "Winter" ~ Week, 
                    Semester == "Spring" ~ Week-15,
                    Semester == "Summer" ~ Week-30,
                    Semester == "Fall" ~ Week-36
                ),
                endtime = datetime + duration * 60,
                start_minute = minute(datetime) + hour(datetime) * 60, 
                end_minute = minute(endtime) + hour(endtime) * 60,
                
            )
        #print(MathCourses)
        # Impute average duration for invalid durations (<= 60 or < 0)
        df <- df %>%
            group_by(course) %>%
            mutate(
                valid_duration = ifelse(duration == 60 | duration < 1, NA, duration),
                avg_duration = mean(valid_duration, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            mutate(
                hour = hour(datetime),                     # Extract hour
                date = date(datetime)                      # Extract date part
            ) %>%
            group_by(student_id, date, hour) %>%
            distinct(student_id, date, hour, .keep_all = TRUE) %>%
            ungroup() %>%
            mutate(
                duration = ifelse(is.na(valid_duration), avg_duration, duration),
                lab_area = factor(case_when(course %in% c("MATH 100A","MATH 100B","MATH 101") ~ "100/101",
                                            course %in% c("MATH 108X","MATH 108Y") ~ "108",
                                            course %in% c("MATH 221A","MATH 221B","MATH 221C","MATH 221D") ~ "221",
                                            course %in% c("MATH 109","MATH 110X","MATH 111") ~ "Pre-Calc",
                                            course == "MATH 112X" ~ "Calc 1",
                                            course %in% c("MATH 113","MATH 214","MATH 214E","MATH 215") ~ "Calc 2/3"))
            ) %>%
            select(-valid_duration, -avg_duration)
        df
    })

    
    joineddata <- reactive({
        req(input$layout_choice, data())
        df <- data()
        lab <- layout_data()[[input$layout_choice]]
        
        summarized_data <- df %>%
            drop_na(lab_area) %>%
            filter(hour > 7 & hour < 22) %>%
            group_by(lab_area, hour, date) %>%    # Group by lab_area, wday, hour
            summarize(
                daily_count = n(),                        # Count observations for each day/hour/area combo
                .groups = 'drop_last'                     # Keep group level for next step (lab_area, wday, hour)
            ) %>%
            group_by(lab_area) %>%
            complete(hour, date, 
                     fill = list(daily_count=0),
                     explicit = FALSE
            ) %>%
            #group_by(lab_area, hour, wday) %>% 
            group_by(lab_area, hour) %>% 
            # Group again by lab_area, wday, hour
            summarize(
                avg_val = round(quantile(daily_count, 0.80),1), # Staff relative to 80th percentile of attendence             # Calculate the average of daily_count
                .groups = 'drop'                         # Drop all grouping
            )
        
        left_join(lab, summarized_data, by = "lab_area")
    })
    
## Observes ####
    
### Update Inputs ####
    
    observe({
        # Update selectizeInput choices based on uploaded data
        # Get existing courses in the list
        All_courses <- c(MathCourses, NotableCourses)
        existing_courses <- unlist(All_courses) # Flatten the list
        unique_courses <- unique(data()$course)
        # Find new courses
        new_courses <- setdiff(unique_courses, existing_courses)
        All_courses$Other <- list(new_courses)
        #print(All_courses)
        print(class(MathCourses))
        updateVirtualSelect(
            session = session,
            inputId = "courses",
            choices = All_courses,
            selected = c(MathCourses[[1]], MathCourses[[2]], MathCourses[[3]],MathCourses[[4]],MathCourses[[5]],MathCourses[[6]],MathCourses[[7]],MathCourses[[8]],MathCourses[[9]])
        )
        updateVirtualSelect(
            session = session,
            inputId = "boxCourses",
            choices = All_courses
            )
        #updateVirtualSelect(session, "courses", choices = MathCourses) 
    })
    
## Apply Filters ####
    
    filtered_data <- reactive({

        df <- data() %>% filter(datetime > input$dateRange[1], datetime < input$dateRange[2])

        if (!is.null(input$courses)) {
            df <- df %>% filter(course %in% input$courses)
        }
        
        if (input$dayOfWeek != "(select)") {
            df <- df %>% filter(wday == input$dayOfWeek)
        }
        
        df
    })
    
    
## Summary Stats ####
    
    output$avg_dur <- renderText({ 
        data = filtered_data()
        paste(round(median(data$duration, na.rm = TRUE)), "mins")
    })
    
    output$sd <- renderText({ 
        data = filtered_data()
        paste(round(sd(data$duration, na.rm = TRUE)), "mins")
    })
    
    output$max <- renderText({ 
        data = filtered_data()
        paste0(floor(max(data$duration) / 60), " hrs ", round(max(data$duration) %% 60), " mins")
    })
    
    
## Plots ####
    
    output$boxplot <- renderPlot({
        dat <- data()
        dat <- dat %>% 
            filter(course %in% input$boxCourses & duration <=300)
        
        ggplot(dat, aes(x = course, y =duration)) + 
            geom_boxplot() +
            facet_zoom(ylim = c(0, 130), zoom.data=zoom, zoom.size = 1.5) # creates the zoom
        
    })
    
    # output$waterfall <- renderPlot({
    #     date_chosen <- input$waterfall_day
    # 
    #     data() %>% filter(date(datetime) == date_chosen) %>%
    #         ggplot() +
    #         geom_segment(aes(x = datetime, xend = endtime,
    #                          y = reorder(id, datetime),
    #                          yend = reorder(id, datetime)),
    #                       size = 1.5, color = "#202020") +
    #         scale_x_datetime(
    #             date_labels = "%I:%M %p", # Custom format: 12-hour format with AM/PM
    #             date_breaks = "2 hours"    # Adjust breaks as needed
    #         ) +
    #         labs(
    #             x = "Appointment Start Time to End Time",
    #             y = "Individual Visits"
    #         ) +
    #         theme_bw() +
    #         theme(
    #             axis.text.y = element_blank(),
    #             axis.ticks.y = element_blank(),
    #             axis.text = element_text(color = "black", size = 15),
    #             axis.title = element_text(color = "#444444", size = 15),
    #             panel.grid.major.y = element_blank(),
    #             panel.grid.minor.y = element_blank()
    #         )
    # })
    
    output$frequency <- renderPlot({
        df <- filtered_data()#course %in% input$courses) #date(datetime) > input$dateRange[[0]], date(datetime) < input$dateRange)
        
        # get a range
        time_range <- seq(
            from = 0,
            to = 1440,
            by = 15
        )
        
        time_labels <- function(x) {
            sprintf("%d:00 %s", ifelse(x > 12, x - 12, x), ifelse(x >= 12, "PM", "AM"))
        }
        
        # chart
        data.frame(
            time = time_range,
            students = sapply(time_range, function(x) sum(df$start_minute < x & x < df$end_minute))
        ) %>% ggplot(aes(time / 60, students)) +
            geom_col(width = 0.2) + #, color = "#777790") +
            scale_x_continuous(breaks = seq(8, 22, by = 2), labels = time_labels) +
            labs(
                y = "Attendant Students",
                x = ""
            ) +
            coord_cartesian(xlim = c(8,22.5))+
            theme_bw() +
            theme(
                axis.text = element_text(color = "black", size = 15),
                axis.title = element_text(color = "#444444", size = 15),
                panel.grid.major.y = element_line(linetype = "dotted"),
                panel.grid.minor.y = element_line(linetype = "dotted")
            )
    })
    
    output$heatmap <- renderPlotly({
        joined_data <- joineddata()
        fig <- plot_ly(
            type= "scatter",
            mode= "markers+text"
        )
        
        min_value <- 0
        max_value <- max(joined_data$avg_val)
        
        # Function to create a color ramp between two colors
        color_ramp <- colorRampPalette(c("forestgreen", "red"))
        custom_colors <- color_ramp(100) # Create 100 colors in the ramp
        
        
        for (h in hours) {
            current_hour_data <- joined_data %>% filter(hour == h)
            
            # Add polygons for the current hour, setting frame and group
            for (i in 1:nrow(current_hour_data)) {
                value_norm <- (current_hour_data$avg_val[i] - min_value) / (max_value - min_value) # Normalize value to 0-1
                # Map normalized value to custom color ramp
                current_color <- custom_colors[floor(value_norm * 99) + 1]
                
                fig <- fig %>% add_polygons(
                    x = c(current_hour_data$xmin[i], current_hour_data$xmax[i], current_hour_data$xmax[i], current_hour_data$xmin[i]),
                    y = c(current_hour_data$ymin[i], current_hour_data$ymin[i], current_hour_data$ymax[i], current_hour_data$ymax[i]),
                    fillcolor = current_color,
                    line = list(color = "rgba(0,0,0,0)"),
                    text = paste(current_hour_data$lab_area[i], "<br><b>Hour</b>:", h, "<br><b>Value</b>:", round(current_hour_data$avg_val[i],2)), # Enhanced hover text
                    hoverinfo = "text",
                    frame = h, # Set the frame for animation
                    #group = current_hour_data$label[i], # Group for consistent polygon identity across frames (optional but good practice)
                    name = paste(current_hour_data$lab_area[i], "<br><b>Hour</b>:", h, "<br><b>Value</b>:", round(current_hour_data$avg_val[i],2)) # Optional name
                )
            }
        }
        fig %>% layout(showlegend = FALSE)
        
    })
    
# Layout ####
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
                       label = character(), stringsAsFactors = FALSE)
        }
        
        ggplot() +
            geom_rect(data = rectangles(),  # Use reactive data here
                      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      fill = NA,  # No fill
                      color = "black",
                      linewidth = 2) +  # Black outline
            geom_text(data = rectangles(),
                      aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
                      size = 5) +
            xlim(0, xlim_max) +
            ylim(0, ylim_max) +
            theme_bw() +
            labs(title = "",
                 xlab = "",
                 ylab = "")
        
    })
}
