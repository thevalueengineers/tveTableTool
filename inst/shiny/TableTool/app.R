library(shiny)
library(shinyWidgets)
library(datamods)
library(tveDataLoader)
library(dplyr)
library(CelloSegmentation)
library(tidyr)
library(labelled)
library(DT)

ui <- fluidPage(

    # Application title
    titlePanel("TVE Tables Tool"),

    # load data ----
    wellPanel(
        h3("Load spss data"),
        import_file_ui("load_data", file_types = ".sav")
    ),


    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(
            wellPanel(
                h3("Choose your rows"),
                uiOutput("row_choice")
            ),
            wellPanel(
                h3("Choose your columns"),
                uiOutput("col_choice")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            switchInput(
                inputId = "show_percents",
                onLabel = "Percentages",
                offLabel = "Counts"
            ),


            dataTableOutput("out_tab"),
            textOutput("row_chosen"),
            textOutput("col_chosen"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    options(shiny.maxRequestSize=1024*1024^2)

    # load data
    imported <- import_file_server("load_data")

    # remove OEs
    tab_data <- reactive({
        req(imported$data())
        select(imported$data(), where(is.numeric))
    })

    variable_labels <- reactive({get_varLabels(tab_data())})

    # row selection ui
    output$row_choice <- renderUI({
        pickerInput(
            "row_var",
            label = "Row variables",
            choices = variable_labels()$label[-1],
            multiple = TRUE,
            options = list(
                `live-search` = TRUE
            )
        )
    })

    # column selection ui
    output$col_choice <- renderUI({
        pickerInput(
            "col_var",
            label = "Column variables",
            choices = variable_labels()$label[-1],
            options = list(
                `live-search` = TRUE
            )
        )
    })

    # get row & column variable names of selected variables
    row_var <- reactive({
        variable_labels()[variable_labels()$label %in% input$row_var, ]$variable
    })
    col_var <- reactive({
        variable_labels()[variable_labels()$label == input$col_var, ]$variable
    })


    # output table
    output$out_tab <- renderDataTable({
        req(input$row_var)


        dat <- select(tab_data(), all_of(c(row_var(), col_var()))) %>%
            # if labelled convert to ordered factor
            mutate(across(where(is.labelled), ~haven::as_factor(.x, ordered = TRUE))) %>%
            mutate(across(where(~!is.factor(.x)), ~factor(.x, levels = sort(unique(.x))))) %>%
            mutate(across(everything(), forcats::fct_explicit_na)) %>%
            mutate(across(everything(), as.character)) %>%
            pivot_longer(-all_of(col_var())) %>%
            group_by(across(everything())) %>%
            count() %>%
            ungroup() %>%
            pivot_wider(names_from = all_of(col_var()), values_from = n)

        if(input$show_percents == TRUE) {
            dat %>%
                group_by(name) %>%
                mutate(across(-1, ~.x * 100 / sum(.x, na.rm = TRUE), 0))
        } else {
            dat
        }



    })

    output$row_chosen <- renderText({row_var()})
    output$col_chosen <- renderText({col_var()})
}

# Run the application
shinyApp(ui = ui, server = server)
