bend_genetics_ui <- function(id){
    ns <- NS(id)
    tabPanel("Bend Genetics",
             tags$h4("Bend Genetics Data", class = "mb-2"),
             bslib::layout_sidebar(
                 sidebar = bslib::sidebar(
                     width = 280,
                     padding = 10,
                        fileInput(ns("bend_genetics_file"), "Select Bend Genetics File", multiple = FALSE),
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
                          shinycssloaders::withSpinner(DT::dataTableOutput(ns("bend_genetics_table")), type = 6, color = "#007bff")
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
                          shinycssloaders::withSpinner(DT::dataTableOutput(ns("edited_wqx_table")), type = 6, color = "#007bff"),
                      ),
                      tabPanel(
                          "Formatted Data",
                          tags$p(class = "p-2 border rounded mb-2 small",
                                 "Review WQX formatted data. Download the file, then use the 'Upload to WQX' tab to submit."),
                          div(class = "mb-2",
                              actionButton(ns("bend_genetics_save"), "Save to Download Folder", class = "btn-primary btn-sm")
                          ),
                          shinycssloaders::withSpinner(DT::dataTableOutput(ns("bend_genetics_wqx_formatted")), type = 6, color = "#007bff")
                     )
                 )
             )
    )
}

bend_genetics_server <- function(input, output, session, account_info){
    ns <- session$ns
    
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
    
    observeEvent(input$bend_genetics_file$datapath, {
        output$check_df_message <- NULL
        if (is.null(input$bend_genetics_file$datapath)) return()
        if (!any(endsWith(input$bend_genetics_file$datapath, c("xlsm", "xls", "xlsx")))) {
            show_error(
              "Invalid File Type",
              "Please upload a valid Bend Genetics data file with a '.xlsm', '.xls', or '.xlsx' extension.",
              paste("Received:", basename(input$bend_genetics_file$datapath))
            )
            return()
        }
    }, ignoreInit = TRUE)
    
    validate_bend_file <- function(filepath) {
      if (!file.exists(filepath)) {
        list(valid = FALSE, error = "File not found", details = filepath)
      } else if (file.info(filepath)$size == 0) {
        list(valid = FALSE, error = "File is empty", details = filepath)
      } else {
        tryCatch({
          sheet_names <- readxl::excel_sheets(filepath)
          if (length(sheet_names) == 0) {
            list(valid = FALSE, error = "No sheets found in Excel file", details = filepath)
          } else {
            list(valid = TRUE, sheets = sheet_names)
          }
        }, error = function(e) {
          list(valid = FALSE, error = "Cannot read Excel file", details = conditionMessage(e))
        })
      }
    }
    
    uploaded_bend_genetics_data <- eventReactive(input$bend_genetics_file$datapath,{
        tryCatch({
            req(input$bend_genetics_file$datapath)
            
            validation <- validate_bend_file(input$bend_genetics_file$datapath)
            if (!validation$valid) {
              show_error("File Validation Failed", validation$error, validation$details)
              return(NULL)
            }
            
            sheet_names <- readxl::excel_sheets(input$bend_genetics_file$datapath)
            sample_sheets <- sheet_names[str_detect(sheet_names, "^Sample")]
            if (length(sample_sheets) == 0) {
                show_error(
                  "No Sample Sheets Found",
                  "No 'Sample' sheets were found in the Excel file.",
                  paste("Available sheets:", paste(sheet_names, collapse = ", "))
                )
                return(NULL)
            }
            
            file_path_vect <- rep(input$bend_genetics_file$datapath, length(sample_sheets))
            
            all_sample_data <- tryCatch({
              purrr::map2(file_path_vect, sample_sheets, parse_bend_genetics_macro) |>
                bind_rows()
            }, error = function(e) {
              show_error(
                "Error Reading Sample Sheets",
                "Failed to parse one or more sample sheets.",
                conditionMessage(e)
              )
              return(NULL)
            })
            
            if (is.null(all_sample_data) || nrow(all_sample_data) == 0) {
              show_warning("Empty Data", "The file was parsed but contains no sample data.")
              return(NULL)
            }
            
            all_sample_data
            },error = function(e) {
                show_error(
                  "Error Parsing File",
                  "Failed to parse the Bend Genetics file."
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
        req(uploaded_bend_genetics_data())
        # if(unique(uploaded_bend_genetics_data()$bend_type) %in% c("MACRO")){
        bend_comparison$data <- uploaded_bend_genetics_data() |> 
            #     # mutate(Result = ifelse(Result != "ND", as.numeric(Result), "ND")) |>
            pivot_wider(names_from = `Analyte`, values_from = "Result") |> 
            relocate(c("Method": "Units"), .after = last_col())
            
        # }
        #     
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
        req(input$bend_genetics_file$datapath)
        if (is.null(bend_comparison$data)) {
            return(NULL)
        }
        shiny::validate(shiny::need(input$bend_genetics_file, message = "Select a file to view"))
        analyte_list <- c("Anatoxin-a", "Cylindrospermopsin", "Microcystin", "Microcystin/Nod.", "Saxitoxin")
        nm1 <- intersect(analyte_list, colnames(bend_comparison$data))
        # print(nm1)
        datatable <- DT::datatable(bend_comparison$data, 
                              editable = list(target = "cell"), 
                                              # disable = list(columns = c(1,3:9, 10:11))),
                              selection = "single",
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
            
    # output$bend_genetics_qaqc_table <- renderTable({
    #     if (is.null(bend_comparison$data)) {
    #         return(NULL)
    #     }
    #     validate(need(bend_comparison$data, message = "Select a file to view qa/qc results."))
    #     validation_results <- validate::confront(bend_comparison$data, bend_genetics_range_rules)
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
    # output$bend_genetics_custom_qaqc_table <- renderTable({
    #     if (is.null(bend_comparison$data)) {
    #         return(NULL)
    #     }
    #     validate(need(bend_comparison$data, message = "Select a file to view custom qa/qc results."))
    #     validation_results <- validate::confront(bend_comparison$data, bend_genetics_custom_rules)
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
        if (is.null(bend_comparison$data)) {
            return(NULL)
        }
        bend_genetics_data$formatted_data <- bend_comparison$data |> 
            # relocate("bend_type", .after = last_col()) |> 
            pivot_longer(
                cols = (which(names(bend_comparison$data) == "Analysis Start Date")+1):(which(names(bend_comparison$data) == "Method")-1),
                names_to = "Analyte",
                values_to = "Result")|>
            drop_na("Result")
            # relocate("Result", .before = "Quantitation Limit") |> 
            # relocate("Characteristic Name", .before = "Result") |>
            # drop_na("Result") 
            
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
        # View(bend_genetics_data$formatted_data)
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
                      editable = list(target = "cell"),
                                      # , disable = list(columns = c(0, 2:9, 12:34))),
                      selection = "single",
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
        output$check_df_message <- renderUI({
            tags$div(class = "alert alert-info mt-2", role = "alert",
                    tags$strong("Important: "), "Check 'Formatted Data' tab for generated WQX data sheet.")
        })
    })
    output$bend_genetics_wqx_formatted <- DT::renderDataTable({
        
        DT::datatable(common_bend_genetics_wqx_data$wqx_data,
                      selection = "single",
                      options = list(scrollX = TRUE, ordering = FALSE, pageLength = 10),
                      caption = "Preview data before download.")
    })
            
            observeEvent(input$bend_genetics_save, {
        if (is.null(common_bend_genetics_wqx_data$wqx_data) || nrow(common_bend_genetics_wqx_data$wqx_data) == 0) {
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
        
        bend_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
        filename <- paste0('bend_genetics-data-', bend_signature(), '.csv')
        file_path <- file.path(download_folder, filename)
        
        tryCatch({
          write.csv(common_bend_genetics_wqx_data$wqx_data, file_path, row.names = FALSE)
          update_file_status(filename, "pending")
          show_success("File Saved", paste("Successfully saved to:", filename))
        }, error = function(e) {
          show_error("Save Failed", "Could not write file to disk.", conditionMessage(e))
        })
    })
}