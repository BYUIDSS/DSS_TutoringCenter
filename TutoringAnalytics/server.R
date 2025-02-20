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

options(shiny.maxRequestSize = 30*1024^2)


MathCourses <- list(
    Foundational = list("MATH 100A","MATH 100B","MATH 101","MATH 108X","MATH 108Y"),
    Precalculus = list("MATH 109","MATH 110X","MATH 111"    ),
    Calculus = list("MATH 112X","MATH 113","MATH 214","MATH 214E","MATH 215","MATH 472"    ),
    AppliedMath  = list("MATH 119","MATH 282","MATH 283","MATH 284"    ),
    Statistics = list("MATH 124","MATH 221A","MATH 221B","MATH 221C","MATH 221D","MATH 325","MATH 326","MATH 330","MATH 423","MATH 424","MATH 425","MATH 428","MATH 494R","MATH 488"    ),
    LinearAlgebra = list("MATH 241","MATH 316","MATH 341","MATH 443"    ),
    TeacherEducation = list("MATH 205","MATH 206","MATH 275","MATH 390","MATH 490","MATH 491"    ),
    AdvancedMath  = list("MATH 301","MATH 340","MATH 350","MATH 411","MATH 412","MATH 440","MATH 441","MATH 442","MATH 461","MATH 462","MATH 463"    ),
    OtherMath  = list("MATH 190","MATH 191","MATH 280","MATH 391","MATH 450","MATH 295R","MATH 495R","MATH 498R","MATH 499R"    )
)

NotableCourses <- list(
    # Add A new group as follows:
    # NameOfGroup = list("Value1", "Value2")
    Economics = list("ECON 150", "ECON 151", "ECON 215", "ECON 278", "ECON 455", "ECON 380", "ECON 381", "ECON 388", "ECON 476")
)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    
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
                endtime = datetime + duration * 60,
                start_minute = minute(datetime) + hour(datetime) * 60, 
                end_minute = minute(endtime) + hour(endtime) * 60
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
                duration = ifelse(is.na(valid_duration), avg_duration, duration)
            ) %>%
            select(-valid_duration, -avg_duration)
        df
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
            choices = All_courses  # Update the choices
        )
        #updateVirtualSelect(session, "courses", choices = MathCourses) 
    })
    
## Apply Filters ####
    
    filtered_data <- reactive({

        df <- data() %>% filter(datetime > input$dateRange[1], datetime < input$dateRange[2])

        # if (!is.null(input$courses)) {
        #     df <- df %>% filter(course %in% input$courses)
        # }
        
        if (input$dayOfWeek != "(select)") {
            df <- df %>% filter(wday(datetime, label = TRUE, abbr = FALSE) == input$dayOfWeek)
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
        df <- filtered_data()#course %in% input$courses) #date(datetime) > input$dateRange[[0]], date(datetime) < input$dateRange)
        
        # get a range
        time_range <- seq(
            from = min(df$start_minute),
            to = max(df$end_minute),
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
            geom_col(width = 0.15) + #, color = "#777790") +
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
}
