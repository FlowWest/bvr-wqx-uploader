alpha_lab_ui <- function(id){
    ns <- NS(id)
    tabPanel("Alpha Lab",
             tags$h4("Alpha Lab Data", class = "mb-2"),
             bslib::layout_sidebar(
                 sidebar = bslib::sidebar(
                     width = 280,
                     padding = 10,
                      fileInput(ns("alpha_lab_file"), "Select Alpha Lab File", multiple = FALSE),
                      actionButton(ns("reset"), "Reset", class = "btn-secondary btn-sm w-100")
                 ),
                 tabsetPanel(
                     id = ns("tabs"),
                     type = "pills",
                     tabPanel(
                         "QA/QC",
                         value = "qa_qc",
                         tags$p(class = "p-2 border rounded mb-2 small",
                                "This section provides view of raw data, as well as results for QA/QC checks. Verify that all validations pass, and proceed to next tab when ready. Click on 'Reset' to clear all saved data and values in application."),
                          shinycssloaders::withSpinner(DT::dataTableOutput(ns("alpha_lab_table")), type = 6, color = "#007bff")
                      ),
                      tabPanel(
                          "Enter Additional Data",
                          value = "additional",
                          tags$p(class = "p-2 border rounded mb-2 small",
                                 "Edit the table below to enter 'Activity Depth/Height Measure', 'Activity Depth/Height Unit', and 'Result Comment'. Click 'Generate WQX Ready Data' to reformat 'Activity ID'."),
                          div(class = "my-2",
                              actionButton(ns("generate_formatted_df"), "Generate WQX Ready Data", class = "btn-primary"),
                              uiOutput(ns("check_df_message"))
                          ),
                          shinycssloaders::withSpinner(DT::dataTableOutput(ns("edited_wqx_table")), type = 6, color = "#007bff")
                      ),
                      tabPanel(
                          "Formatted Data",
                          tags$p(class = "p-2 border rounded mb-2 small",
                                 "Review WQX formatted data. Download the file, then use the 'Upload to WQX' tab to submit."),
                           div(class = "mb-2",
                               actionButton(ns("alpha_lab_save"), "Save to Download Folder", class = "btn-primary btn-sm")
                           ),
                          shinycssloaders::withSpinner(DT::dataTableOutput(ns("alpha_lab_wqx_formatted")), type = 6, color = "#007bff")
                     )
                 )
             )
    )
}

alpha_lab_server <- function(input, output, session, account_info){
    
    show_error <- function(title, message, details = NULL) {
      full_msg <- message
      if (!is.null(details) && details != "") {
        full_msg <- paste0(message, "\n\nDetails: ", details)
      }
      sendSweetAlert(
        session = session,
        title = title,
        text = tags$div(
          tags$p(message),
          if (!is.null(details)) tags$small(class = "text-muted", paste("Details:", details))
        ),
        type = "error",
        html = TRUE
      )
    }
    
    show_warning <- function(title, message) {
      sendSweetAlert(
        session = session,
        title = title,
        text = message,
        type = "warning"
      )
    }
    
    show_success <- function(title, message) {
      sendSweetAlert(
        session = session,
        title = title,
        text = message,
        type = "success"
      )
    }
    
    observeEvent(input$alpha_lab_file$datapath, {
        output$check_df_message <- NULL
        if (is.null(input$alpha_lab_file$datapath)) return()
        if (!any(endsWith(input$alpha_lab_file$datapath, c(".xls", ".xlsx")))) {
            show_error(
              "Invalid File Type",
              "Please upload a valid Alpha Lab data file with a '.xls' or '.xlsx' extension.",
              paste("Received:", basename(input$alpha_lab_file$datapath))
            )
            return()
        }
    }, ignoreInit = TRUE)
    
    validate_alpha_file <- function(filepath) {
      if (!file.exists(filepath)) {
        list(valid = FALSE, error = "File not found", details = filepath)
      } else if (file.info(filepath)$size == 0) {
        list(valid = FALSE, error = "File is empty", details = filepath)
      } else {
        list(valid = TRUE)
      }
    }
    
    uploaded_alpha_lab_data <- eventReactive(input$alpha_lab_file$datapath,{
        tryCatch({
            req(input$alpha_lab_file$datapath)
            
            validation <- validate_alpha_file(input$alpha_lab_file$datapath)
            if (!validation$valid) {
              show_error("File Validation Failed", validation$error, validation$details)
              return(NULL)
            }
            
            data <- purrr::map_df(input$alpha_lab_file$datapath, \(x) parse_alphalab(x))
            
            if (nrow(data) == 0) {
              show_warning("Empty Data", "The file was parsed but contains no data rows.")
              return(NULL)
            }
            
            required_cols <- c("SAMPLENAME", "SAMPDATE", "ANALYTE", "RESULT")
            missing_cols <- required_cols[!required_cols %in% toupper(names(data))]
            if (length(missing_cols) > 0) {
              show_warning(
                "Missing Expected Columns",
                paste("Some expected columns were not found:", paste(missing_cols, collapse = ", ")),
                "The file may not be a valid Alpha Lab export."
              )
            }
            
            unknown_samples <- data$SAMPLENAME[!data$SAMPLENAME %in% names(project_id_lookup)]
            unknown_samples <- unknown_samples[!is.na(unknown_samples)]
            if (length(unknown_samples) > 0) {
              show_warning(
                "Unknown Sample Names",
                paste("Some sample names are not recognized:", paste(unique(unknown_samples), collapse = ", ")),
                "Please verify these are correct BVR monitoring locations."
              )
            }
            
            data
        }, error = function(e) {
            show_error(
              "Error Parsing File",
              "Failed to parse the Alpha Lab Excel file."
            )
            return(NULL)
        })
    })
    
    # handle data editing by the user
    # rvals <- reactiveValues(data = NULL)
    alpha_comparison <- reactiveValues(data = NULL)
    alpha_labs_data <- reactiveValues(formatted_data = NULL)
    
    observe({
        req(uploaded_alpha_lab_data())
        alpha_comparison$data <- uploaded_alpha_lab_data() |>
            mutate(RESULT = ifelse(RESULT != "ND" 
                                 & RESULT != "Absent" 
                                 & RESULT != "Present", 
                                 as.numeric(RESULT), RESULT)) |> 
            pivot_wider(names_from = "ANALYTE", values_from = "RESULT")
            
        # alpha_comparison$data <- alpha_comparison$data |> 

    })
    #
    observeEvent(input$reset, {
        alpha_signature <- NULL
        alpha_wqx_status <- NULL
        common_alpha_lab_wqx_data$wqx_data <- NULL
        alpha_comparison$data <- NULL
        alpha_labs_data$formatted_data <- NULL
    })
    #
    observeEvent(input$alpha_lab_table_cell_edit, {
        alpha_comparison$data <<- DT::editData(alpha_comparison$data, input$alpha_lab_table_cell_edit)
    })

    output$alpha_lab_table <- DT::renderDataTable({
        req(input$alpha_lab_file$datapath)
        if (is.null(alpha_comparison$data)) {
            return(NULL)
        }
        shiny::validate(shiny::need(input$alpha_lab_file, message = "Select a file to view"))
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
    
    # output$alpha_lab_qaqc_table <- renderTable({
    #     if (is.null(alpha_comparison)) {
    #         return(NULL)
    #     }
    #     validate(need(alpha_comparison$data, message = "Select a file to view qa/qc results."))
    #     validation_results <- validate::confront(alpha_comparison$data, alpha_lab_range_rules)
    #     as_tibble(summary(validation_results)) |>
    #         mutate(pass = case_when(
    #             error == TRUE ~ "!",
    #             warning == TRUE ~ "!",
    #             items == passes ~ "O",
    #             fails > 0 ~ "X",
    #             TRUE ~ "?"
    #         ),
    #         name = stringr::str_replace_all(name, "\\.", " ")) |>
    #         select(-c("nNA","items","warning","expression"))
    # })
    # 
    # output$alpha_lab_custom_qaqc_table <- renderTable({
    #     if (is.null(alpha_comparison$data)) {
    #         return(NULL)
    #     }
    #     validate(need(alpha_comparison$data, message = "Select a file to view custom qa/qc results."))
    #     validation_results <- validate::confront(alpha_comparison$data, alpha_lab_custom_rules)
    #     as_tibble(summary(validation_results)) |>
    #         mutate(pass = case_when(
    #             error == TRUE ~ "!",
    #             warning == TRUE ~ "!",
    #             items == passes ~ "O",
    #             fails > 0 ~ "X",
    #             TRUE ~ "?"
    #         ),
    #         name = stringr::str_replace_all(name, "\\.", " "))  |>
    #         select(-c("nNA","items","warning","expression"))
    # })
    
    observe({
        if (is.null(alpha_comparison$data)) {
            return(NULL)
        }
        alpha_labs_data$formatted_data <- alpha_comparison$data |>  
            pivot_longer(cols = (ncol(uploaded_alpha_lab_data())-1):ncol(alpha_comparison$data),
                         names_to = "ANALYTE",
                         values_to = "RESULT") |> 
            relocate("RESULT", .before = "DL") |> 
            relocate("ANALYTE", .before = "CASNUMBER") |>
            drop_na("RESULT")
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
        output$check_df_message <- renderUI({
            tags$div(class = "alert alert-info mt-2", role = "alert",
                    tags$strong("Important: "), "Check 'Formatted Data' tab for generated WQX data sheet.")
        })
    })
    output$alpha_lab_wqx_formatted <- DT::renderDataTable({
         
        DT::datatable(common_alpha_lab_wqx_data$wqx_data,
                      options = list(scrollX = TRUE, ordering = FALSE, pageLength = 10),
                      caption = "Preview data before download.")
    })    
    
    observeEvent(input$alpha_lab_save, {
        if (is.null(common_alpha_lab_wqx_data$wqx_data) || nrow(common_alpha_lab_wqx_data$wqx_data) == 0) {
          show_error("No Data to Save", "Please generate WQX formatted data first.")
          return()
        }
        
        download_folder <- tryCatch({
          account_info$selectedDownloadFolder()
        }, error = function(e) {
          show_error("Configuration Error", "Could not access download folder settings.", conditionMessage(e))
          return(NULL)
        })
        
        if (is.null(download_folder) || download_folder == "") {
          show_error("Missing Download Folder", "Please configure a download folder in the User Account settings.")
          return()
        }
        
        tryCatch({
          if (!dir.exists(download_folder)) {
            dir.create(download_folder, recursive = TRUE)
          }
        }, error = function(e) {
          show_error("Permission Error", "Could not create download folder.", conditionMessage(e))
          return()
        })
        
        alpha_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
        filename <- paste0('alpha_lab-data-', alpha_signature(), '-', format(lubridate::now(), "%Y%m%d_%H%M%S"), '.csv')
        file_path <- file.path(download_folder, filename)
        
        tryCatch({
          write.csv(common_alpha_lab_wqx_data$wqx_data, file_path, row.names = FALSE)
          show_success("File Saved", paste("Successfully saved to:", filename))
        }, error = function(e) {
          show_error("Save Failed", "Could not write file to disk.", conditionMessage(e))
        })
    })
}