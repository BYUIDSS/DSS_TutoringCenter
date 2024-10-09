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
        card_header("Bill Length"),
        plotOutput("bill_length")
    ),
    card(
        full_screen = TRUE,
        card_header("Bill depth"),
        plotOutput("bill_depth")
    ),
    card(
        full_screen = TRUE,
        card_header("Body Mass"),
        plotOutput("body_mass")
    )
)

fluidPage(
    bslib::page_sidebar(
    title = "Penguins dashboard",
    sidebar = list(
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        selectInput("dayOfWeek", "Day of Week", choices = c("Monday", "Tuesday", "Wednesday","Thursday","Friday", "Saturday", "Sunday")),
        selectInput("hourOfDay", "Hour of Day", choices = 0:23),
        selectizeInput("courses", "Course(s)",  choices = NULL, multiple = TRUE),
        dateRangeInput("dateRange", "Date Range")
    ),
    layout_columns(
        fill = FALSE,
        value_box(
            title = "Average appt duration",
            value = textOutput("avg_dur"),,
            showcase = bsicons::bs_icon("align-bottom")
        ),
        value_box(
            title = "Average bill depth",
            value = 6,
            showcase = bsicons::bs_icon("align-center")
        ),
        value_box(
            title = "Average body mass",
            value = 4,
            showcase = bsicons::bs_icon("handbag")
        )
    ),
    layout_columns(
        cards[[1]], cards[[2]]
    ),
    cards[[3]]
)
)
