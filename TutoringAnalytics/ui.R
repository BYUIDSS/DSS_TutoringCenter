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
library(shinyWidgets)
library(bsicons)
library(plotly)

boxControls <- popover(
    bs_icon("gear"),
    virtualSelectInput("boxCourses", "Course(s)",  choices = NULL, multiple = TRUE, zIndex = 900 ),
    title = "Box Plot Data Selection"
)

navbarPage( # UI ####
    title = "BYU-I Tutoring Analytics",
    selected = "Math Center Analytics",
    collapsible = TRUE,
    tabPanel(
        title = "Math Center Analytics",
        page_sidebar(
            sidebar = list(
                fileInput("file1", "Choose CSV File", accept = ".csv"),
                virtualSelectInput("courses", "Course(s)",  choices = NULL, multiple = TRUE, zIndex = 900 ),#, showValueAsTags = TRUE),
                selectInput("dayOfWeek", "Day of Week", choices = c("(select)", "Monday", "Tuesday", "Wednesday","Thursday","Friday", "Saturday", "Sunday")),
                dateRangeInput("dateRange", "Date Range", start = "2020-01-01"),
                selectInput("semester", "Semester(s)", choices = c("Winter", "Spring", "Summer","Fall"), multiple = TRUE),
                selectInput("weekofsemester", "Week(s) of Semester", choices = c(1:15), multiple = TRUE)
            ),
            layout_columns(
                fill = FALSE,
            #     value_box(
            #         title = "Median appt duration",
            #         value = textOutput("avg_dur"),
            #         showcase = bsicons::bs_icon("align-center")
            #     ),
            #     value_box(
            #         title = "Standard Deviation",
            #         value = textOutput("sd"),
            #         showcase = bsicons::bs_icon("signpost-split-fill")
            #     ),
            #     value_box(
            #         title = "Max appt Length",
            #         value = textOutput("max"),
            #         showcase = bsicons::bs_icon("align-top")
            #     )
            ),
            card(
                full_screen = TRUE,
                card_header("Total Students Attended - Aggregated by day over entire date range"),
                plotOutput("frequency")
            ),
            card(
                full_screen = TRUE,
                card_header("Attendance Duration Distribution By Course", boxControls,class = "d-flex justify-content-between"),
                plotOutput("boxplot"),
            ),
            card(
                full_screen = TRUE,
                card_header("Attendance Duration Distribution By Course", boxControls,class = "d-flex justify-content-between"),
                plotlyOutput("heatmap"),
            ),
            card(
                full_screen = TRUE,
                card_header("WIP"),#, compControls,class = "d-flex justify-content-between"),
                plotOutput("comp1"),
            )
        )
    ),
    tabPanel(
        title = "Study Center Layout Builder",
        page_sidebar(
            sidebar = list(
                selectInput("label_choice", "Label for New Table:",
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
                checkboxInput("delete_mode", "Delete Mode", value = FALSE)
                #numericInput("numericInput", "Observations:", 10, min = 1, max = 100) # Not in use
            ),
            layout_columns(
                card(
                    full_screen = FALSE,
                    card_header("Math Study Center Design"),
                    uiOutput("plot_ui", width = "100%")
                )
            )
        )
    )
)

