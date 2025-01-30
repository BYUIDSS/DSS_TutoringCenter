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

options(shiny.maxRequestSize = 30*1024^2)

# Define server logic required to draw a histogram
function(input, output, session) {

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
                endtime = datetime + duration * 60,
                start_minute = minute(datetime) + hour(datetime) * 60, 
                end_minute = minute(endtime) + hour(endtime) * 60
            )
        
        # This just wipes out the dataset - drops all errors
        # And limits to just math courses
        df <- df %>% 
            filter(duration != 60, duration > 1) %>% 
            filter(str_starts(course, "MATH"))
        
        # Prepare to remove any weird days
        min_max_days <- df %>% 
            mutate(
                date = date(datetime)
            ) %>% 
            group_by(date) %>% 
            summarize(
                min = min(minute(datetime) + (hour(datetime)) * 60) / 60,
                max = max(minute(datetime) + (hour(datetime)) * 60) / 60
            )
        bad_days <- min_max_days$date[min_max_days$min < 6]
        
        # Remove weird days
        df <- df %>% filter(!(date(datetime) %in% bad_days))
        
        # Update selectizeInput choices based on uploaded data
        updateSelectizeInput(session, "courses", choices = unique(df$course), server = TRUE) 
        
        df
    })
    
    dynamic_data <- reactive({
        req(input$file1)

        df <- data() %>% filter(datetime > input$dateRange[1], datetime < input$dateRange[2])

        if (!is.null(input$courses)) {
            df <- df %>% filter(course %in% input$courses)
        }
        
        if (input$dayOfWeek != "(select)") {
            df <- df %>% filter(wday(datetime, label = TRUE, abbr = FALSE) == input$dayOfWeek)
        }
        
        df
    })
    
    output$avg_dur <- renderText({ 
        data = data()
        paste(round(median(data$duration, na.rm = TRUE)), "mins")
    })
    
    output$sd <- renderText({ 
        data = dynamic_data()
        paste(round(sd(data$duration, na.rm = TRUE)), "mins")
    })
    
    output$max <- renderText({ 
        data = dynamic_data()
        paste0(floor(max(data$duration) / 60), " hrs ", round(max(data$duration) %% 60), " mins")
    })
    
    output$waterfall <- renderPlot({
        date_chosen <- input$waterfall_day

        data() %>% filter(date(datetime) == date_chosen) %>%
            ggplot() +
            geom_segment(aes(x = datetime, xend = endtime,
                             y = reorder(id, datetime),
                             yend = reorder(id, datetime)),
                         size = 1.5, color = "#202020") +
            scale_x_datetime(
                date_labels = "%I:%M %p", # Custom format: 12-hour format with AM/PM
                date_breaks = "2 hours"    # Adjust breaks as needed
            ) +
            labs(
                x = "Appointment Start Time to End Time",
                y = "Individual Visits"
            ) +
            theme_bw() +
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text = element_text(color = "black", size = 15),
                axis.title = element_text(color = "#444444", size = 15),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    output$frequency <- renderPlot({
        df <- dynamic_data()#course %in% input$courses) #date(datetime) > input$dateRange[[0]], date(datetime) < input$dateRange)
        
        # get a range
        time_range <- seq(
            from = min(df$start_minute),
            to = max(df$end_minute),
            by = 1
        )
        
        time_labels <- function(x) {
            sprintf("%d:00 %s", ifelse(x > 12, x - 12, x), ifelse(x >= 12, "PM", "AM"))
        }
        
        # chart
        data.frame(
            time = time_range,
            students = sapply(time_range, function(x) sum(df$start_minute < x & x < df$end_minute))
        ) %>% ggplot(aes(time / 60, students)) +
            geom_col(width = 0.02) + #, color = "#777790") +
            scale_x_continuous(breaks = seq(8, 22, by = 2), labels = time_labels) +
            labs(
                y = "Attendant Students",
                x = ""
            ) +
            theme_bw() +
            theme(
                axis.text = element_text(color = "black", size = 15),
                axis.title = element_text(color = "#444444", size = 15),
                panel.grid.major.y = element_line(linetype = "dotted"),
                panel.grid.minor.y = element_line(linetype = "dotted")
            )
    })
    
    observeEvent(input$courses, {
        df <- data() %>% filter(course %in% input$courses)
        
    })
}
