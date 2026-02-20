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
                         DT::dataTableOutput(ns("bend_genetics_table"))
                     ),
                     tabPanel(
                         "Enter Additional Data",
                         value = "additional",
                         tags$p(class = "p-2 border rounded mb-2 small",
                                "Edit the table below to enter 'Activity Depth/Height Measure', 'Activity Depth/Height Unit', and 'Result Comment'. Click 'Generate WQX Ready Data' to reformat 'Activity ID'."),
                         div(class = "my-2",
                             actionButton(ns("generate_formatted_df"), "Generate WQX Ready Data", class = "btn-primary"),
                             span(class = "ms-2", textOutput(ns("check_df_message"), inline = TRUE))
                         ),
                         DT::dataTableOutput(ns("edited_wqx_table")),
                     ),
                     tabPanel(
                         "Formatted Data",
                         tags$p(class = "p-2 border rounded mb-2 small",
                                "Review WQX formatted data. Download the file, then use the 'Upload to WQX' tab to submit."),
                         div(class = "mb-2",
                             downloadButton(ns("bend_genetics_download"), class = "btn-primary btn-sm")
                         ),
                         DT::dataTableOutput(ns("bend_genetics_wqx_formatted"))
                     )
                 )
             )
    )
}

bend_genetics_server <- function(input, output, session, account_info){
    ns <- session$ns
    uploaded_bend_genetics_data <- eventReactive(input$bend_genetics_file$datapath,{
        tryCatch({
            req(input$bend_genetics_file$datapath)
            
            if (!any(endsWith(input$bend_genetics_file$datapath, c("xlsm", "xls")))) {
                sendSweetAlert(
                    session = session,
                    title = "Error",
                    text = "Please upload valid Bend Genetics data files with a '.xlsm' or '.xls' extension.",
                    type = "error"
                )
                return(NULL)
            }
            # if(any(endsWith(input$bend_genetics_file$datapath, c("xlsm", "xls")))){
            sheet_names <- readxl::excel_sheets(input$bend_genetics_file$datapath)
            sample_sheets <- sheet_names[str_detect(sheet_names, "^Sample")]
            file_path_vect <- rep(input$bend_genetics_file$datapath, length(sample_sheets))
            
            all_sample_data<- purrr::map2(file_path_vect, sample_sheets, parse_bend_genetics_macro)
            bind_rows(all_sample_data)
                # return(full_data)
                # full <- full_data |>
                #     mutate(bend_type = "MACRO")
                # full <- full |>
                #     relocate(bend_type, .before = "Analysis Start Date")
                # relocate("Result", .before = "Quantitation Limit") |> 
                
                # return("MACRO")
            # }
            },error = function(e) {
                sendSweetAlert(
                    session = session,
                    title = "Error",
                    text = paste("An error occurred:", e$message),
                    type = "error"
                )
                return(NULL)
            })
            # return(all_sample_data)
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
}