library(shiny)
library(shinyjs)
library(shinyWidgets)
library(datamods) # tve version
library(tveDataLoader)
library(dplyr)
library(tidyr)
library(labelled)
library(DT)
library(tveTableTool) # need to install the dev version for dev version of the app
# devtools::load_all()
source("global.R")
source("helpers.R")

# detect whether running locally or on shinyapps
dev <- Sys.getenv("R_CONFIG_ACTIVE") != "shinyapps"


# devtools::load_all()

options(shiny.maxRequestSize=2024*1024^2)

ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("TVE Tables Tool"),



    # load data ----
    wellPanel(
        h3("1. Load spss data"),
        import_file_ui("load_data", file_types = ".sav")
    ),

    # set weight and filters ----
    wellPanel(
        h3("2. Include weight and filter variables"),
        "NOTE: If you make changes here you may need to re-specify your row and column variables below.",
        fluidRow(
            column(6,
                   wellPanel(
                       weightVariable_ui("weight_variable")
                   )
            ),
            column(6,
                   wellPanel(
                       filterVariable_ui("filter_variables")
                   )
            )
        )
    ),



    # pick row and column vars ----
    wellPanel(
        h3("3. Choose row and column variables"),
        fluidRow(
            column(6,
                   wellPanel(
                       h3("Choose your row variables"),
                       rowChoice_ui("row_vars")
                   )
            ),
            column(6,
                   wellPanel(
                       h3("Choose your column variable"),
                       colChoice_ui("col_vars")
                   )
            )
        )
    ),

    # Show a plot of the generated distribution
    wellPanel(
        uiOutput("resp_filters"),
        outputTab_ui("out")
    ),

    ## debug ----
    if(dev == TRUE) {
        wellPanel(
            "Weight variable: ", verbatimTextOutput("weightingVariable"),
            "Filters: ", verbatimTextOutput("include_filters"),
            "Filter var names: ", verbatimTextOutput("filter_var_names"),
            "Filter var character: ", verbatimTextOutput("filter_var_char"),
            # "Selection vars: ", verbatimTextOutput("selectionVars"),
            # "Row vars: ", verbatimTextOutput("rowVars"),
            # "Column vars: ", verbatimTextOutput("colVars"),
            # "Variable labels: ", shiny::dataTableOutput("varLabels"),
            # "Filter data: ", shiny::dataTableOutput("filter_data"),
            "dat: ", shiny::dataTableOutput("dat"),
            "Expr: ", verbatimTextOutput("expr"),
            "Meta: ", verbatimTextOutput("meta"),
            # "Output stat: ", verbatimTextOutput("output_stat"),
            "tab: ", shiny::dataTableOutput("tab"),
            # "out_dat: ", shiny::dataTableOutput("out_dat"),
            # "out_row_var: ", verbatimTextOutput("out_row_var"),
            # "out_col_var: ", verbatimTextOutput("out_col_var"),
            # "out_weight_var: ", verbatimTextOutput("out_weight_var"),
            # "out_variable_labels: ", shiny::dataTableOutput("out_variable_labels")
        )
    }

)


# for authentication
ui_func <- function(req) {
    if (dev == FALSE){
        opts <- parseQueryString(req$QUERY_STRING)
        if(is.null(opts$code))
        {
            auth_uri <- build_authorization_uri(resource, tenant, app, redirect_uri=redirect, version=2)
            redir_js <- sprintf("location.replace(\"%s\");", auth_uri)
            tags$script(HTML(redir_js))
        }
        else ui
    } else ui
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {



    # load data ----
    imported <- import_file_server("load_data",
                                   trigger_return = "change",
                                   btn_show_data = FALSE)
    dat_file_name <- reactive({imported$name()})


    # prepData ----
    # only required if loading data via datamods::import_file
    tab_data <- prepData("id", imported$data)

    ## variable labels ----
    # variable_labels <- reactive({getVariableLabels(tab_data())})
    variable_labels <- getVariableLabels("id2", tab_data, "no_weighting")
    output$varLabels <- shiny::renderDataTable({head(variable_labels())})


    # weight variable selection ----
    weight_var <- weightVariable_server("weight_variable", variable_labels)

    output$weightingVariable <- renderText({weight_var()})

    # filter variable selection ----
    filters <- filterVariable_server("filter_variables", variable_labels, tab_data)

    output$filter_var_names <- renderText({names(filters()$filter_vars)})
    output$filter_var_char <- renderText({as.character(filters()$filter_vars)})
    output$filter_data <- shiny::renderDataTable({filters()$filter_dat})

    output$include_filters <- renderText({
        req(filters())
        filters()$inc_filters
    })

    # variables available for column & row selection ----
    selection_vars <- selectionVars_server("selVars", variable_labels, weight_var())
    row_var <- rowChoice_server("row_vars", selection_vars, variable_labels)
    col_var <- colChoice_server("col_vars", selection_vars, variable_labels)

    output$selectionVars <- renderText({
        req(selection_vars())
        selection_vars()
    })
    output$rowVars <- renderText({
        req(row_var())
        row_var()
    })
    output$colVars <- renderText({
        req(col_var())
        col_var()
    })

    # filters ----
    output$resp_filters <- renderUI({
        req(filters()$inc_filters)
        if (filters()$inc_filters == TRUE) {
            tagList(
                tags$strong("Respondent filters"),
                br(),
                filter_data_ui("resp_filters2"),
            )
        }
    })

    inc_filters <- reactive({filters()$inc_filters})
    filter_dat <- reactive({filters()$filter_dat})
    filter_vars <- reactive({filters()$filter_vars})

    filtering <- filter_data_server(
        id = "resp_filters2",
        data = filter_dat,
        vars = filter_vars,
        name = reactive("data")
    )

    filtering_exp <- reactive({filtering$expr()})

    # output table ----
    dat <- reactive({
        if(inc_filters() == TRUE) {
            dat = filtering$filtered()
        } else {
            dat = tab_data()
        }
        dat
    })

    output$dat <- shiny::renderDataTable({head(dat())})

    outTable <- outputTab_server("out",
                                 dat = dat,
                                 filtering_exp = filtering_exp,
                                 dat_file_name = dat_file_name,
                                 weight_var = weight_var,
                                 row_var = row_var,
                                 col_var = col_var,
                                 variable_labels = variable_labels)

    output$expr = renderText({outTable()$expr})
    output$meta = renderText({outTable()$meta})
    output$output_stat <- renderText({outTable()$output_stat})
    output$tab <- shiny::renderDataTable({head(outTable()$tab)})
    output$out_dat <- shiny::renderDataTable({head(outTable()$dat)})
    output$out_row_var <- renderText({outTable()$row_var})
    output$out_col_var <- renderText({outTable()$col_var})
    output$out_weight_var <- renderText({outTable()$weight_var})
    output$out_variable_labels <- shiny::renderDataTable({head(outTable()$variable_labels)})




    # authentication ----
    shinyjs::runjs(clean_url_js)

    opts <- parseQueryString(isolate(session$clientData$url_search))
    if(is.null(opts$code))
        return()

    # this assumes your app has a 'public client/native' redirect:
    # if it is a 'web' redirect, include the client secret as the password argument
    token <- get_azure_token(resource,
                             tenant,
                             app,
                             password = secret,
                             auth_type="authorization_code",
                             authorize_args=list(redirect_uri=redirect),
                             version=2,
                             use_cache=FALSE,
                             auth_code=opts$code)

}

# Run the application
shinyApp(ui = ui_func, server = server)
