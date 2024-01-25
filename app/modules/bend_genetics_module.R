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
                             card(card_header("Raw Data"), card_body(
                                 DT::dataTableOutput(ns("bend_genetics_table")),
                                 style = "height: 900px; width: 100%;"
                                 )
                                ),
                             tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - 'O', - test passed, ', 'X' - test failed, '!' - verify manually (usually safe to ignore)"),
                             layout_column_wrap(
                                 width = 1/2,
                                 card(card_header("Range based rules"), card_body(tableOutput(ns("bend_genetics_qaqc_table")))),
                                 card(card_header("Custom rules"), card_body(tableOutput(ns("bend_genetics_custom_qaqc_table"))))
                             )
                         ),
                         tabPanel(
                             "Enter Additional Data",
                             value = "additional",
                             tags$p(class = "p-3 border rounded",
                                    "Edit the table below to enter 'Activity Depth/Height Measure', 'Activity Depth/Height Unit', and 'Result Comment'. Click 'Generate WQX Ready Data' to reformat 'Activity ID'."),
                             card(card_header("Edit Data"), card_body(
                                 DT::dataTableOutput(ns("edited_wqx_table")),
                                 style = "height: 1000px; width: 100%;"
                             )
                             ),
                             actionButton(ns("generate_formatted_df"), "Generate WQX Ready Data"),
                             textOutput(ns("check_df_message"))
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
                                 uiOutput(ns("bend_genetics_upload_status"))
                             ),
                             card(card_header("Preview Final Upload"), card_body(
                                 DT::dataTableOutput(ns("bend_genetics_wqx_formatted")),
                                 style = "height: 1500px; width: 100%;"
                             ))
                         )
                     )
                 )
             ))
}

bend_genetics_server <- function(input, output, session, account_info){
    ns <- session$ns
    uploaded_bend_genetics_data <- eventReactive(input$bend_genetics_file$datapath,{
        tryCatch({
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
            },error = function(e) {
                sendSweetAlert(
                    session = session,
                    title = "Error",
                    text = paste("An error occurred:", e$message),
                    type = "error"
                )
                return(NULL)
            })
        })

    # bend_genetics_comparison_table <- reactive({
    #             uploaded_bend_genetics_data() |>
    #                 tidyr::pivot_wider(names_from = "Target", values_from = "Result", values_fn = as.numeric) |>
    #                 rename("Microcycstin Nod" = "Microcystin/Nod.")
    #         })

    # handle data editing by the user
    # rvals <- reactiveValues(data = NULL)
    bend_comparison <- reactiveValues(data = NULL)
    bend_genetics_data <- reactiveValues(formatted_data = NULL)
    

    observe({
        bend_comparison$data <- uploaded_bend_genetics_data() |> 
            mutate(Result = ifelse(Result != "ND", as.numeric(Result), "ND")) |>
            pivot_wider(names_from = "Target", values_from = "Result")   
            
            # print(colnames(bend_comparison$data))
        
        # rvals$data <- uploaded_bend_genetics_data()
    })
    observeEvent(input$reset, {
        # rvals$data <- NULL
        bend_signature <- NULL
        bend_wqx_status <- NULL
        common_bend_genetics_wqx_data <- NULL
        bend_genetics_data$formatted_data <- NULL
        bend_comparison$data <- NULL
    })
    #
    observeEvent(input$bend_genetics_table_cell_edit, {
        bend_comparison$data <<- DT::editData(bend_comparison$data, input$bend_genetics_table_cell_edit)
    })
    #
    output$bend_genetics_table <- DT::renderDataTable({
        if (is.null(bend_comparison$data)) {
            return(NULL)
        }
        validate(need(input$bend_genetics_file, message = "Select a file to view"))
        analyte_list <- c("Anatoxin-a", "Cylindrospermopsin", "Microcystin", "Microcystin/Nod.", "Saxitoxin")
        nm1 <- intersect(analyte_list, colnames(bend_comparison$data))
        # print(nm1)
        datatable <- DT::datatable(bend_comparison$data, 
                              editable = list(target = "cell"), 
                                              # disable = list(columns = c(1,3:9, 10:11))),
                              options = list(scrollX = TRUE,
                                             pageLength = 10))
        for (analyte in nm1) {
            if(analyte == "Microcystin"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 0.8), c("#f29f99", "white", "#f29f99"))
                    )
            } else{
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 0.001), c("#f29f99", "white", "#f29f99"))
                    ) 
            }
            
        }
        return(datatable)
    
    })
            
    output$bend_genetics_qaqc_table <- renderTable({
        if (is.null(bend_comparison$data)) {
            return(NULL)
        }
        validate(need(bend_comparison$data, message = "Select a file to view qa/qc results."))
        validation_results <- validate::confront(bend_comparison$data, bend_genetics_range_rules)
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
        if (is.null(bend_comparison$data)) {
            return(NULL)
        }
        validate(need(bend_comparison$data, message = "Select a file to view custom qa/qc results."))
        validation_results <- validate::confront(bend_comparison$data, bend_genetics_custom_rules)
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
            
    observe({
        if (is.null(bend_comparison$data)) {
            return(NULL)
        }
        bend_genetics_data$formatted_data <- bend_comparison$data |> 
            pivot_longer(cols = -c("Sample ID", 
                                   "Location", 
                                   "Date Collected", 
                                   "Date Received", 
                                   "Matrix",
                                   "Preserved",
                                   "BG_ID",
                                   "Method",
                                   "Quantitation Limit",
                                   "Units",
                                   "Notes"),
                         names_to = "Target",
                         values_to = "Result") |> 
            relocate("Result", .before = "Quantitation Limit") |> 
            relocate("Target", .before = "Result") |> 
            drop_na("Result")
            
    })        
            # handle data uploads
    bend_signature <- reactiveVal(NULL)
    bend_wqx_status <- reactiveVal(NULL)
    bend_edited <- reactiveValues(wqx_data=NULL)
    common_bend_genetics_wqx_data <- reactiveValues(wqx_data=NULL)

    observe({
        if (is.null(bend_genetics_data$formatted_data)) {
            return(NULL)
        }
        bend_edited$wqx_data <- bend_genetics_to_wqx(bend_genetics_data$formatted_data)
        
    })
            
    observe({
        if (is.null(bend_genetics_data$formatted_data)){
            return(NULL)
        }
        bend_edited$wqx_data <- clean_bend_wqx(bend_edited$wqx_data)
    })
    
    output$edited_wqx_table <- DT::renderDataTable({
        
        DT::datatable(bend_edited$wqx_data,
                      editable = list(target = "cell", disable = list(columns = c(0, 2:9, 12:34))),
                      options = list(scrollX = TRUE, ordering = FALSE, pageLength = 10),
                      caption = "Additional data - please check that the 'Monitoring Location ID' matches the 'Project ID'.")
    })
            
    observeEvent(input$edited_wqx_table_cell_edit, {
        bend_edited$wqx_data <<- DT::editData(bend_edited$wqx_data, input$edited_wqx_table_cell_edit)
        })
    
 

    observeEvent(input$generate_formatted_df, {
        # common_bend_genetics_wqx_data(bend_genetics_data_wqx())
        common_bend_genetics_wqx_data$wqx_data <- bend_edited$wqx_data |>
            mutate("Activity ID (CHILD-subset)" = bend_genetics_make_activity_id(location_id = `Monitoring Location ID`,
                                                       date = `Activity Start Date`,
                                                       time = `Activity Start Time`,
                                                       activity_type = `Activity Type`,
                                                       equipment_name = `Sample Collection Equipment Name`,
                                                       depth = `Activity Depth/Height Measure`)) |> 
            relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)")
        # common_bend_genetics_wqx_data(bend_genetics_data$formatted_data)
        output$check_df_message <- renderText({
            Sys.sleep(0.5)
            "Check Formatted Data tab for generated WQX data sheet."
        })
    })
    output$bend_genetics_wqx_formatted <- DT::renderDataTable({
        
        DT::datatable(common_bend_genetics_wqx_data$wqx_data,
                      options = list(scrollX = TRUE, ordering = FALSE, pageLength = 10),
                      caption = "Preview data before download.")
    })
            
            #Could refactor
    output$bend_genetics_download <- downloadHandler(
        filename = function() {
            bend_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
            paste('bend_genetics-data-', bend_signature(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(common_bend_genetics_wqx_data$wqx_data, file, row.names = FALSE)
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
        API_KEY = account_info$selectedApiKey()
        USER_ID = account_info$selectedUsername()
        CONFIG_ID = account_info$selectedConfigId()
        FILE_PATH = path_to_most_recent
        FILE_NAME =  paste("bend_genetics-data-", bend_signature(), ".csv", sep = "")
        cat("USER_ID:", USER_ID, "\n")
        cat("API_KEY:", API_KEY, "\n")
        cat("FILE_PATH:", FILE_PATH, "\n")
        cat("FILE_NAME:", FILE_NAME, "\n")
        
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