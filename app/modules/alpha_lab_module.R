alpha_lab_ui <- function(id){
    ns <- NS(id)
    tabPanel("Alpha Lab",
             
             tags$h2("Alpha Lab Data"),
             sidebarLayout(
                 sidebarPanel(width = 3,
                              fileInput(ns("alpha_lab_file"), "Select Alpha Lab File", multiple = TRUE),
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
                                 DT::dataTableOutput(ns("alpha_lab_table")),
                                 style = "height: 1400px; width: 100%;"
                             )
                             ),
                             tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - 'O', - test passed, ', 'X' - test failed, '!' - verify manually (usually safe to ignore)"),
                             layout_column_wrap(
                                 width = 1/2,
                                 card(card_header("Range based rules"), card_body(tableOutput(ns("alpha_lab_qaqc_table")))),
                                 card(card_header("Custom rules"), card_body(tableOutput(ns("alpha_lab_custom_qaqc_table"))))
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
                                 downloadButton(ns("alpha_lab_download")),
                                 actionButton(ns("alpha_lab_upload"), label = "Upload to WQX", icon = shiny::icon("rocket")),
                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                  tags$div(HTML("<b> Starting WQX upload. Please wait 25 seconds for the upload status from CDX...</b>"),id="loadmessage")),
                                 uiOutput(ns("alpha_lab_upload_status"))
                             ),
                             card(card_header("Preview Final Upload"), card_body(
                                 DT::dataTableOutput(ns("alpha_lab_wqx_formatted")),
                                 style = "height: 1400px; width: 100%;"
                             ))
                         )
                     )
                 )
             ))
}

alpha_lab_server <- function(input, output, session, account_info){
    uploaded_alpha_lab_data <- eventReactive(input$alpha_lab_file$datapath,{
        tryCatch({
            req(input$alpha_lab_file$datapath)
            
            if (!any(endsWith(input$alpha_lab_file$datapath, c(".xls")))) {
                sendSweetAlert(
                    session = session,
                    title = "Error",
                    text = "Please upload valid Alpha Lab data files with a '.xls' extension.",
                    type = "error"
                )
                return(NULL)
            }
            purrr::map_df(input$alpha_lab_file$datapath, \(x) parse_alphalab(x))
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
    
    # handle data editing by the user
    # rvals <- reactiveValues(data = NULL)
    alpha_comparison <- reactiveValues(data = NULL)
    alpha_labs_data <- reactiveValues(formatted_data = NULL)
    
    observe({
        alpha_comparison$data <- uploaded_alpha_lab_data() |> 
            mutate(Result = ifelse(Result != "ND" & Result != "Absent" & Result != "Present", as.numeric(Result), Result)) |>
            pivot_wider(names_from = "ANALYTE", values_from = "Result")   

    })
    #
    observeEvent(input$reset, {
        alpha_signature <- NULL
        alpha_wqx_status <- NULL
        common_alpha_lab_wqx_data$wqx_data <- NULL
        # alpha_lab_data$formatted_data <- NULL
    })
    #
    observeEvent(input$alpha_lab_table_cell_edit, {
        alpha_comparison$data <<- DT::editData(alpha_comparison$data, input$alpha_lab_table_cell_edit)
    })

    output$alpha_lab_table <- DT::renderDataTable({
        if (is.null(alpha_comparison$data)) {
            return(NULL)
        }
        validate(need(input$alpha_lab_file, message = "Select a file to view"))
        analyte_list <- c(
            "Oil & Grease (HEM)", 
            "Nitrate + Nitrite as N", 
            "Phosphorus, total", 
            "Total Organic Carbon",
            "Total Kjeldahl Nitrogen",
            "Fecal Coliform",
            "Total Coliform")
        nm1 <- intersect(analyte_list, colnames(alpha_comparison$data))
        # print(nm1)
        datatable <- DT::datatable(alpha_comparison$data, 
                                   editable = list(target = "cell", 
                                                   disable = list(columns = c(1, 3:4, 6:45))),
                                   options = list(scrollX = TRUE,
                                                  pageLength = 10))
        for (analyte in nm1) {
            if(analyte == "Oil & Grease (HEM)"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 3000), c("#f29f99", "white", "#f29f99"))
                    )
            }else if(analyte == "Nitrate + Nitrite as N"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 10), c("#f29f99", "white", "#f29f99"))
                    )
            }else if(analyte == "Phosphorus, total"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 2), c("#f29f99", "white", "#f29f99"))
                    )
            }else if(analyte == "Total Organic Carbon"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 10), c("#f29f99", "white", "#f29f99"))
                    )
            }else if(analyte == "Total Kjeldahl Nitrogen"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 10), c("#f29f99", "white", "#f29f99"))
                    )
            }else if(analyte == "Total Coliform"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 300000), c("#f29f99", "white", "#f29f99"))
                    )
            }else if(analyte == "Fecal Coliform"){
                datatable <- datatable |>
                    DT::formatStyle(
                        columns = analyte,
                        target = "cell",
                        backgroundColor = DT::styleInterval(c(0, 300000), c("#f29f99", "white", "#f29f99"))
                    )
            }
        }
        return(datatable)
        
    })
    
    output$alpha_lab_qaqc_table <- renderTable({
        if (is.null(alpha_comparison)) {
            return(NULL)
        }
        validate(need(alpha_comparison$data, message = "Select a file to view qa/qc results."))
        validation_results <- validate::confront(alpha_comparison$data, alpha_lab_range_rules)
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
    
    output$alpha_lab_custom_qaqc_table <- renderTable({
        if (is.null(alpha_comparison$data)) {
            return(NULL)
        }
        validate(need(alpha_comparison$data, message = "Select a file to view custom qa/qc results."))
        validation_results <- validate::confront(alpha_comparison$data, alpha_lab_custom_rules)
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
        if (is.null(alpha_comparison$data)) {
            return(NULL)
        }
        alpha_labs_data$formatted_data <- alpha_comparison$data |>  
            pivot_longer(cols = (starts_with("ANALYTEORDER")+1):ncol(alpha_comparison$data),
                         names_to = "ANALYTE",
                         values_to = "Result") |> 
            relocate("Result", .before = "DL") |> 
            relocate("ANALYTE", .before = "CASNUMBER") |>
            drop_na("Result")
    })
    # handle data uploads
    alpha_signature <- reactiveVal(NULL)
    alpha_wqx_status <- reactiveVal(NULL)
    alpha_edited <- reactiveValues(wqx_data=NULL)
    common_alpha_lab_wqx_data <- reactiveValues(wqx_data=NULL)

    observe({
        if (is.null(alpha_comparison$data)) {
            return(NULL)
        }
        alpha_edited$wqx_data <- alpha_lab_to_wqx(alpha_labs_data$formatted_data)
    })
    
    output$edited_wqx_table <- DT::renderDataTable({
        
        DT::datatable(alpha_edited$wqx_data,
                      editable = list(target = "cell", disable = list(columns = c(0, 3:9, 12:34))),
                      options = list(scrollX = TRUE, ordering = FALSE, pageLength = 10),
                      caption = "Additional data - please check that the 'Monitoring Location ID' matches the 'Project ID'.")
    })
    
    observeEvent(input$edited_wqx_table_cell_edit, {
        alpha_edited$wqx_data <<- DT::editData(alpha_edited$wqx_data, input$edited_wqx_table_cell_edit)
    })
    
    observeEvent(input$generate_formatted_df, {
        # common_alpha_lab_wqx_data(alpha_lab_data_wqx())
        common_alpha_lab_wqx_data$wqx_data <- alpha_edited$wqx_data |>
            mutate("Activity ID (CHILD-subset)" = alpha_lab_make_activity_id(location_id = `Monitoring Location ID`,
                                                                             date = `Activity Start Date`,
                                                                             time = `Activity Start Time`,
                                                                             activity_type = `Activity Type`,
                                                                             equipment_name = `Sample Collection Equipment Name`,
                                                                             depth = `Activity Depth/Height Measure`)) |> 
            relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)")
        # common_alpha_lab_wqx_data(alpha_lab_data$formatted_data)
        output$check_df_message <- renderText({
            Sys.sleep(0.5)
            "Check Formatted Data tab for generated WQX data sheet."
        })
    })
    output$alpha_lab_wqx_formatted <- DT::renderDataTable({
        
        DT::datatable(common_alpha_lab_wqx_data$wqx_data,
                      options = list(scrollX = TRUE, ordering = FALSE, pageLength = 10),
                      caption = "Preview data before download.")
    })    
    #Could refactor
    output$alpha_lab_download <- downloadHandler(
        filename = function() {
            alpha_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
            paste('alpha_lab-data-', alpha_signature(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(common_alpha_lab_wqx_data$wqx_data, file, row.names = FALSE)
        }
    )
    alpha_lab_wqx_status <- eventReactive(input$alpha_lab_upload, {
        downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
        path_to_most_recent <- str_replace_all(
            paste(
                downloads_path,
                "/alpha_lab-data-",
                alpha_signature(),
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
        FILE_NAME =  paste("alpha_lab-data-", alpha_signature(), ".csv", sep = "")
        
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
    
    output$alpha_lab_upload_status <- renderUI({
        validate(need(alpha_lab_wqx_status(), "start upload, status of upload will be shown here after completion"))
        if (alpha_lab_wqx_status()$StatusName == "Import Failed") {
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