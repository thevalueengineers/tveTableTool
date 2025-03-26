
# prepData <- function(id, data) {
#
#   moduleServer(
#     id,
#     function(input, output, session) {
#       reactive({
#         shiny::req(data())
#         select(data(), where(is.numeric)) %>%
#           # add no_weighting variable
#           mutate(no_weighting = 1)
#       })
#     }
#   )
# }

# getVariableLabels <- function(id, dat) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       reactive({
#         shiny::req(dat())
#         vars_with_labels <- tveDataLoader::get_varLabels(dat())
#         vars_without_labels <- names(dat())[!names(dat()) %in% vars_with_labels$variable]
#
#         # add vars without labels to variable_labels
#         bind_rows(
#           vars_with_labels,
#           tibble(
#             variable = vars_without_labels,
#             label = vars_without_labels
#           )
#         ) %>%
#           # make sure no_weighting var isn't included
#           filter(variable != "no_weighting")
#       })
#     }
#   )
# }

# weightVariable_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("inc_weight")),
#     uiOutput(ns("weight_variable"))
#   )
# }
#
# weightVariable_server <- function(id, varLabels) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#
#       output$inc_weight <- renderUI({
#         shiny::req(varLabels)
#         shinyWidgets::materialSwitch(
#           inputId = ns("inc_weight2"),
#           label = "Include weighting variable",
#           value = FALSE,
#           status = "primary"
#         )
#       })
#
#       output$weight_variable <- renderUI({
#         shiny::req(input$inc_weight2)
#         if(input$inc_weight2 == TRUE) {
#           choices <- slice(varLabels(), -1) %>% pull(label)
#
#           shinyWidgets::pickerInput(
#             ns("weight_var"),
#             label = "Weight variable",
#             choices = choices,
#             options = list(
#               `live-search` = TRUE
#             )
#           )
#         }
#       })
#
#       return(
#         reactive({
#
#           if(input$inc_weight2 == TRUE) {
#             weight <- filter(varLabels(), label %in% input$weight_var) %>%
#               pull(variable)
#           } else {
#             weight <- "no_weighting"
#           }
#
#           weight
#         })
#       )
#     }
#   )
# }


#
# filterVariable_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("inc_filters")),
#     uiOutput(ns("filter_variables"))
#   )
# }
#
# filterVariable_server <- function(id, varLabels, tab_data) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#
#       output$inc_filters <- renderUI({
#         shinyWidgets::materialSwitch(
#           inputId = ns("inc_filters2"),
#           label = "Include respondent filters",
#           value = FALSE,
#           status = "primary"
#         )
#       })
#
#       output$filter_variables <- renderUI({
#         shiny::req(input$inc_filters2)
#
#         if(input$inc_filters2 == TRUE) {
#           choices <- slice(varLabels(), -1) %>% pull(label)
#
#           shinyWidgets::pickerInput(
#             ns("filter_vars"),
#             label = "Filter variables",
#             choices = choices,
#             multiple = TRUE,
#             options = list(
#               `live-search` = TRUE
#             )
#           )
#         }
#       })
#
#       filter_vars <- reactive({
#         setNames(
#           varLabels()[varLabels()$label %in% input$filter_vars, ]$variable,
#           varLabels()[varLabels()$label %in% input$filter_vars, ]$label
#         )
#       })
#
#       filter_dat <- reactive({
#         # shiny::req(tab_data())
#         # shiny::req(filter_vars())
#         mutate(
#           tab_data(),
#           across(all_of(as.character(filter_vars())), haven::as_factor)
#         )
#       })
#
#       return(
#         reactive({
#           # shiny::req(input$inc_filters2)
#           # shiny::req(filter_vars())
#
#           list(
#             inc_filters = input$inc_filters2,
#             filter_vars = filter_vars(),
#             filter_dat = filter_dat()
#           )
#         })
#       )
#
#     }
#   )
# }

#
#
# selectionVars_server <- function(id, varLabels, weightVar) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       selection_vars <- reactive({
#
#         shiny::req(varLabels())
#         shiny::req(weightVar())
#
#         filter(varLabels(), variable != weightVar()) %>%
#           slice(-1) %>%
#           pull(label)
#       })
#       return(selection_vars)
#     }
#   )
# }


#
# rowChoice_ui <- function(id) {
#   ns <- NS(id)
#   uiOutput(ns("row_choice"))
# }
#
# rowChoice_server <- function(id, selection_vars, varLabels) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#
#       output$row_choice <- renderUI({
#
#         shiny::req(selection_vars())
#
#         pickerInput(
#           ns("row_vars"),
#           label = "Row variables",
#           choices = selection_vars(),
#           multiple = TRUE,
#           options = list(
#             `live-search` = TRUE,
#             `actions-box` = TRUE
#           )
#         )
#       })
#
#       row_variables <- reactive({
#         filter(varLabels(), label %in% input$row_vars) %>%
#           pull(variable)
#       })
#
#       return(row_variables)
#
#     }
#   )
# }
#
#
# colChoice_ui <- function(id) {
#   ns <- NS(id)
#   uiOutput(ns("col_choice"))
# }
#
# colChoice_server <- function(id, selection_vars, varLabels) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#
#       output$col_choice <- renderUI({
#
#         shiny::req(selection_vars())
#
#         pickerInput(
#           ns("column_variable"),
#           label = "Column Variables",
#           choices = selection_vars(),
#           options = list(
#             `live-search` = TRUE,
#             `actions-box` = TRUE
#           )
#         )
#       })
#
#       column_variables <- reactive({
#         filter(varLabels(), label %in% input$column_variable) %>%
#           pull(variable)
#       })
#
#       return(column_variables)
#
#     }
#   )
# }


# outputTab_ui <- function(id) {
#   ns <- NS(id)
#
#   tagList(
#     radioGroupButtons(
#       inputId = ns("show_percents"),
#       label = "Show percents or counts",
#       choices = c("Column Percentages",
#                   "Row Percentages",
#                   "Counts"),
#       selected = "Column Percentages"
#     ),
#     hr(),
#     tags$strong("Output table"),
#     DT::dataTableOutput(ns("out_tab"))
#   )
# }
#
# outputTab_server <- function(id,
#                              dat,
#                              filtering_exp,
#                              dat_file_name,
#                              weight_var,
#                              row_var,
#                              col_var,
#                              variable_labels) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#
#       expr <- reactive({
#         # shiny::req(filtering_exp())
#         expr <- toString(deparse(filtering_exp()))
#         expr <- ifelse(is.null(filtering_exp()), "Total sample", expr)
#
#         expr <- stringr::str_remove_all(expr, "%")
#         expr <- stringr::str_replace_all(expr, "\\!", "NOT")
#         expr <- paste("Sample definition:", expr)
#         expr
#       })
#
#       meta <- reactive({
#         paste(
#           paste0("Data File: ", as.character(dat_file_name())),
#           paste0("Filters: ", expr()),
#           paste0("Weighting: ", stringr::str_replace(weight_var(), "_", " ")),
#           sep = " | "
#         )
#       })
#
#       # output table ----
#
#       stat <- reactive({
#         if(input$show_percents == "Column Percentages") {
#           "columns"
#         } else if (input$show_percents == "Row Percentages") {
#           "rows"
#         } else {
#           "none"
#         }
#       })
#
#
#       tab <- reactive({
#         shiny::req(row_var())
#         shiny::req(col_var())
#
#
#
#         tab <- generate_table(
#           dat(),
#           row_var(),
#           col_var(),
#           weight_var(),
#           variable_labels(),
#           stat()
#         )
#
#         # fix order of table
#         tab <- inner_join(
#           get_valLabels(dat()) %>%
#             filter(variable %in% row_var()) %>%
#             select(-value),
#           tab,
#           by = c("variable" = "Variable", "value label" = "Value")
#         ) %>%
#           select(variable, Label, `value label`, everything())
#
#         tab
#
#
#       })
#
#       output$out_tab <- DT::renderDataTable({
#         shiny::req(col_var())
#
#
#         col_label <- filter(variable_labels(), variable == col_var()) %>%
#           pull(label)
#
#         sketch = htmltools::withTags(table(
#           class = 'display',
#           thead(
#             tr(
#               th(rowspan = 2, 'Variable'),
#               th(rowspan = 2, 'Label'),
#               th(rowspan = 2, 'Value'),
#               th(colspan = ncol(tab()) - 3, col_label)
#             ),
#             tr(
#               lapply(names(tab())[-c(1:3)], th)
#             )
#           )
#         ))
#
#         out <- datatable(
#           tab(),
#           container = sketch,
#           rownames = FALSE,
#           extensions = c("Buttons", "RowGroup"),
#           caption = meta(),
#           options = list(
#             dom = "Bplft",
#             rowGroup = list(dataSrc = 1),
#             buttons = c("copy", "csv", "excel"),
#             pageLength = 100,
#             lengthMenu = list(c(100, 200, 500, 1000, -1), c("100", "200", "500", "1000", "All")),
#             scrollX = TRUE,
#             scrollY = "800px",
#             columnDefs = list(
#               list(width = "50px", targets = c(0, 2)),
#               list(width = "200px", targets = 1),
#               list(width = "100px", targets = 3:(ncol(tab()) - 1))
#             )
#           )
#         )
#
#         if(stat() %in% c("columns", "rows")) {
#           out <- out %>%
#             formatPercentage(4:ncol(tab()), 2)
#         }
#         out
#
#       })
#
#       return(
#         reactive({
#           list(
#             expr = expr(),
#             # meta = meta(),
#             dat = dat(),
#             row_var = row_var(),
#             col_var = col_var(),
#             weight_var = weight_var(),
#             variable_labels = variable_labels(),
#             output_stat = stat(),
#             tab = tab()
#           )
#         })
#
#       )
#     }
#   )
# }
