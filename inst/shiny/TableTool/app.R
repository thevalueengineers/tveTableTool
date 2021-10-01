library(shiny)
library(shinyWidgets)
library(datamods)
library(tveDataLoader)
library(dplyr)
library(tidyr)
library(labelled)
library(DT)
library(tveTableTool)

ui <- fluidPage(

    # Application title
    titlePanel("TVE Tables Tool"),

    # load data ----
    wellPanel(
        h3("Load spss data"),
        import_file_ui("load_data", file_types = ".sav")
    ),



    # sidebarLayout(

    fluidRow(
        column(6,
               wellPanel(
                   h3("Choose your row variables"),
                   uiOutput("row_choice")
               )
        ),
        column(6,
               wellPanel(
                   h3("Choose your column variable"),
                   uiOutput("col_choice")
               )
        )
        ),

        # Show a plot of the generated distribution
        wellPanel(
            switchInput(
                inputId = "show_percents",
                onLabel = "Percentages",
                offLabel = "Counts",
                value = TRUE
            ),


            DT::dataTableOutput("out_tab")
        )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    options(shiny.maxRequestSize=1024*1024^2)

    # load data
    imported <- import_file_server("load_data",
                                   trigger_return = "change",
                                   btn_show_data = FALSE)

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
                `live-search` = TRUE,
                `actions-box` = TRUE
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
    output$out_tab <- DT::renderDataTable({
        req(input$row_var)

        dat <- generate_table(
            tab_data(),
            row_var(),
            col_var(),
            variable_labels(),
            input$show_percents
        )

        col_label <- filter(variable_labels(), variable == col_var()) %>%
            pull(label)

        sketch = htmltools::withTags(table(
            class = 'display',
            thead(
                tr(
                    th(rowspan = 2, 'Variable'),
                    th(rowspan = 2, 'Label'),
                    th(rowspan = 2, 'Value'),
                    th(colspan = ncol(dat) - 3, col_label)
                ),
                tr(
                    lapply(names(dat)[-c(1:3)], th)
                )
            )
        ))

        out <- datatable(
            dat,
            container = sketch,
            rownames = FALSE,
            extensions = "Buttons",
            options = list(
                dom = "Bplft",
                buttons = c("copy", "csv", "excel"),
                pageLength = 100,
                lengthMenu = list(c(100, 200, 500, 1000, -1), c("100", "200", "500", "1000", "All")),
                scrollX = TRUE,
                columnDefs = list(
                    list(width = "50px", targets = c(0, 2)),
                    list(width = "200px", targets = 1),
                    list(width = "100px", targets = 3:(ncol(dat) - 1))
                )
            )
        )

        if(input$show_percents == TRUE) {
            out <- out %>%
                formatPercentage(4:ncol(dat), 2)
        }
        out


    })

    output$row_chosen <- renderText({row_var()})
    output$col_chosen <- renderText({col_var()})
}

# Run the application
shinyApp(ui = ui, server = server)
