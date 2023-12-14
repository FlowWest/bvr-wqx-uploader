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
                             br(),
                             actionButton(ns("generate_formatted_df"), "Generate WQX Ready Data"),
                             br(),
                             # textOutput(ns("check_df_message")),
                             tags$p(class = "p-3 border rounded",
                                    "Review WQX formatted data. Click 'Download' and then 'Upload to WQX' when ready."),
                             bslib::layout_columns(
                                 col_widths = c(2, 2, 2),
                                 downloadButton(ns("bend_genetics_download")),
                                 actionButton(ns("bend_genetics_upload"), label = "Upload to WQX", icon = shiny::icon("rocket")),
                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                  tags$div(HTML("<b> Starting WQX upload. Please wait 25 seconds for the upload status from CDX...</b>"),id="loadmessage")),
                                 uiOutput(ns("bend_upload_status"))
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
                bend_signature <- NULL
                bend_wqx_status <- NULL
                common_bend_genetics_wqx_data <- NULL
                bend_genetics_data$formatted_data <- NULL
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
                DT::datatable(rvals$data, editable = list(target = "cell", disable = list(columns = c(1,16))),
                              options = list(scrollX = TRUE, dom = "t", ordering = FALSE, pageLength = 25))
            })
            
            output$bend_genetics_qaqc_table <- renderTable({
                if (is.null(rvals$data)) {
                    return(NULL)
                }
                validate(need(rvals$data, message = "Select a file to view qa/qc results."))
                validation_results <- validate::confront(rvals$data, bend_genetics_range_rules)
                as_tibble(summary(validation_results)) |>
                    mutate(pass = case_when(
                        error == TRUE ~ "!",
                        warning == TRUE ~ "!",
                        items == passes ~ "O",
                        fails > 0 ~ "X",
                        TRUE ~ "?"
                    ),
                    name = stringr::str_replace_all(name, "\\.", " ")) |>
                    select(-c("nNA","items","warning","expression"))
            })
            
            output$bend_genetics_custom_qaqc_table <- renderTable({
                if (is.null(rvals$data)) {
                    return(NULL)
                }
                validate(need(rvals$data, message = "Select a file to view custom qa/qc results."))
                validation_results <- validate::confront(rvals$data, bend_genetics_custom_rules)
                as_tibble(summary(validation_results)) |>
                    mutate(pass = case_when(
                        error == TRUE ~ "!",
                        warning == TRUE ~ "!",
                        items == passes ~ "O",
                        fails > 0 ~ "X",
                        TRUE ~ "?"
                    ),
                    name = stringr::str_replace_all(name, "\\.", " "))  |>
                    select(-c("nNA","items","warning","expression"))
            })
            
            
            # handle data uploads
            bend_signature <- reactiveVal(NULL)
            bend_wqx_status <- reactiveVal(NULL)
            common_bend_genetics_wqx_data <- reactiveVal(NULL)
            bend_genetics_data <- reactiveValues(formatted_data = NULL)
            
            bend_genetics_data_wqx <- reactive({
                bend_genetics_data$formatted_data <- bend_genetics_to_wqx(rvals$data)
                # bend_genetics_data$formatted_data <- clean_bend_wqx(bend_lab_data$formatted_data)
                # View(bend_genetics_data$formatted_data)

            })
            
            # bend_genetics_wqx_formatted <-  eventReactive(input$generate_formatted_df, {
            #     req(input$bend_genetics_file$datapath)
            #     bend_genetics_data$formatted_data <- clean_bend_wqx(bend_genetics_data$formatted_data)
            # })
            
            observeEvent(input$generate_formatted_df, {
                common_bend_genetics_wqx_data(clean_bend_wqx(bend_genetics_data_wqx()))
                # output$check_df_message <- renderText({
                #     Sys.sleep(0.5)
                #     "Check Formatted Data tab for generated WQX data sheet. To delete the added data, click on 'Delete Last Added Result'."
                # })
            })
            
            output$bend_genetics_wqx_formatted <- renderTable({
                req(common_bend_genetics_wqx_data())
                common_bend_genetics_wqx_data()
            })
            #Could refactor
            output$bend_genetics_download <- downloadHandler(
                filename = function() {
                    bend_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
                    paste('bend_genetics-data-', bend_signature(), '.csv', sep='')
                },
                content = function(file) {
                    write.csv(common_bend_genetics_wqx_data(), file, row.names = FALSE)
                }
            )
            bend_genetics_wqx_status <- eventReactive(input$bend_genetics_upload, {
                downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
                path_to_most_recent <- str_replace_all(
                    paste(
                        downloads_path,
                        "/bend_genetics-data-",
                        bend_signature(),
                        ".csv",
                        sep = ""
                    ),
                    "\\\\",
                    "/"
                )
                
                API_KEY = input$wqx_api_key
                USER_ID = input$wqx_username
                CONFIG_ID = input$wqx_config_id
                FILE_PATH = path_to_most_recent
                FILE_NAME =  paste("bend_genetics-data-", bend_signature(), ".csv", sep = "")
                
                spsComps::shinyCatch({message("sending request to CDX Web")}, position = "bottom-full-width")
                
                session <- cdx(USER_ID, API_KEY, FILE_PATH, FILE_NAME)
                file_id <- cdx_upload(session = session)
                dataset_id <-
                    cdx_import(
                        session = session,
                        file_id = file_id,
                        config_id = CONFIG_ID,
                        params = c("newOrExistingData", "0")
                    )
                
                Sys.sleep(25)
                return(cdx_get_status(session, dataset_id))
                
            })
            
            output$bend_genetics_upload_status <- renderUI({
                validate(need(bend_genetics_wqx_status(), "start upload, status of upload will be shown here after completion"))
                if (bend_genetics_wqx_status()$StatusName == "Import Failed") {
                    tags$p(tags$b("Import failed."), "Please retry upload.", style = "{color: red;}")
                } else
                {
                    tags$p(
                        tags$b("Application is importing data onto CDX."),
                        tags$br(),
                        "You may now close this document. Check email or CDX website for the final upload confirmation."
                    )
                }
            })
            
            

}