#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

cards <- list(
    card(
        full_screen = TRUE,
        card_header("Total Students Attended - Aggregated by day over entire date range"),
        plotOutput("frequency")
    ),
    card(
        full_screen = TRUE,
        card_header("Student Visits - on a single given day"),
        dateInput("waterfall_day", "Choose a Day", value = "2024-02-05"),
        plotOutput("waterfall"),
    )
)

fluidPage(
    bslib::page_sidebar(
    title = "Tutoring Attendance Dashboard",
    sidebar = list(
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        selectInput("dayOfWeek", "Day of Week", choices = c("(select)", "Monday", "Tuesday", "Wednesday","Thursday","Friday", "Saturday", "Sunday")),
        selectizeInput("courses", "Course(s)",  choices = NULL, multiple = TRUE),
        dateRangeInput("dateRange", "Date Range", start = "2020-01-01")
    ),
    layout_columns(
        fill = FALSE,
        value_box(
            title = "Median appt duration",
            value = textOutput("avg_dur"),
            showcase = bsicons::bs_icon("align-center")
        ),
        value_box(
            title = "Standard Deviation",
            value = textOutput("sd"),
            showcase = bsicons::bs_icon("signpost-split-fill")
        ),
        value_box(
            title = "Max appt Length",
            value = textOutput("max"),
            showcase = bsicons::bs_icon("align-top")
        )
    ),
    cards[[1]],
    cards[[2]]
)
)
