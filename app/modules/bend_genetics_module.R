bend_genetics_ui <- function(id){
    ns <- NS(id)
    tabPanel("Bend Genetics",
             
             tags$h2("Bend Genetics Data"),
             sidebarLayout(
                 sidebarPanel(width = 3,
                              fileInput(ns("bend_genetics_file"), "Select Bend Genetics File", multiple = TRUE),
                              actionButton(ns("reset"), "Reset")
                              ),
                 mainPanel(
                     tabsetPanel(
                         id = "tabs",
                         type = "pills",
                         tabPanel(
                             "Qa/
                                Qc",
                             value = "qa_qc",
                             tagList(
                                 tags$p(class = "p-3 border rounded",
                                        "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that
                                           all validations pass, and proceed to next tab when ready. Click on 'Reset' to clear all saved data and values in application.")
                             ),
                             card(card_header("Raw Data"), card_body(DT::dataTableOutput(ns("bend_genetics_table")))),
                             tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - 'O', - test passed, ', 'X' - test failed, '!' - verify manually (usually safe to ignore)"),
                             layout_column_wrap(
                                 width = 1/2,
                                 card(card_header("Range based rules"), card_body(tableOutput(ns("bend_genetics_qaqc_table")))),
                                 card(card_header("Custom rules"), card_body(tableOutput(ns("bend_genetics_custom_qaqc_table"))))
                             )
                         ),
                         tabPanel(
                             "Formatted Data",
                             tags$p(class = "p-3 border rounded",
                                    "Review WQX formatted data. Click 'Download' and then 'Upload to WQX' when ready."),
                             bslib::layout_columns(
                                 col_widths = c(2, 2, 2),
                                 downloadButton(ns("bend_genetics_download")),
                                 actionButton(ns("bend_genetics_upload"), label = "Upload to WQX", icon = shiny::icon("rocket")),
                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                  tags$div(HTML("<b> Starting WQX upload. Please wait 25 seconds for the upload status from CDX...</b>"),id="loadmessage")),
                                 uiOutput(ns("hydro_upload_status"))
                             ),
                             # DT::dataTableOutput("bend_genetics_wqx_formatted")
                             tableOutput(ns("bend_genetics_wqx_formatted"))
                         )
                     )
                 )
             ))
}

bend_genetics_server <- function(input, output, session){
            uploaded_bend_genetics_data <- reactive({
                req(input$bend_genetics_file$datapath)
                if (!any(endsWith(input$bend_genetics_file$datapath, c(".csv", ".CSV")))) {
                    sendSweetAlert(
                        session = session,
                        title = "Error",
                        text = "Please upload valid Bend Genetics data files with a '.csv' extension.",
                        type = "error"
                    )
                    return(NULL)
                }
                purrr::map_df(input$bend_genetics_file$datapath, \(x) parse_bend_genetics(x))
            })

            bend_genetics_comparison_table <- reactive({
                        uploaded_bend_genetics_data() |>
                            tidyr::pivot_wider(names_from = "Target", values_from = "Result", values_fn = as.numeric) |>
                            rename("Microcycstin Nod" = "Microcystin/Nod.")
                    })

    # handle data editing by the user
            rvals <- reactiveValues(data = NULL)

            observe({
                rvals$data <- uploaded_bend_genetics_data()
            })
    #
            observeEvent(input$reset, {

                rvals$data <- NULL
                hydro_signature <- NULL
                hydro_wqx_status <- NULL
                common_bend_genetics_wqx_data <- NULL
                bend_genetics_data$formatted_data <- NULL
                temp_data$filtered_data <- NULL
                wqx_data$empty_data <- NULL
            })
    #
            observeEvent(input$bend_genetics_table_cell_edit, {
                rvals$data <<- DT::editData(rvals$data, input$bend_genetics_table_cell_edit)
            })
    #
            output$bend_genetics_table <- DT::renderDataTable({
                if (is.null(rvals$data)) {
                    return(NULL)
                }
                validate(need(input$bend_genetics_file, message = "Select a file to view"))
                DT::datatable(rvals$data, editable = list(target = "cell", disable = list(columns = c(1,2, 16))),
                              options = list(scrollX = TRUE, dom = "t", ordering = FALSE, pageLength = 25))
            })

}