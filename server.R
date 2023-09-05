function(input, output, session) {

    # hydro lab -------------------------------------------------------------------------
    uploaded_hydro_lab_data <- reactive({
        if (!any(endsWith(input$hydro_lab_file$datapath, ".csv"))) {
            sendSweetAlert(
                session = session,
                title = "Error",
                text = "at least one file is not hydrolab, please try uploading again",
                type = "error"
            )
        }
        purrr::map_df(input$hydro_lab_file$datapath, \(x) parse_hydrolab(x))

    })
    
    hydro_signature <- reactiveVal(NULL)
    hydro_wqx_status <- reactiveVal(NULL)
    
    hydro_lab_data_wqx_formatted <- reactive({
        hydro_lab_to_wqx(uploaded_hydro_lab_data())
    })
    
    hydro_path_to_most_recent_download <- reactive({
        
    })
    
    output$hydro_lab_table <- renderTable({
        
        validate(need(input$hydro_lab_file, message = "Select a file to view"))
        uploaded_hydro_lab_data()
    })
    
    output$hydro_lab_qaqc_table <- renderTable({
        validate(need(input$hydro_lab_file, message = "Select a file to view qa/qc results."))
        validation_results <- validate::confront(uploaded_hydro_lab_data(), hydro_lab_range_rules)
        as_tibble(summary(validation_results)) |>
            mutate(pass = case_when(
                error == TRUE ~ emo::ji("warning"), 
                warning == TRUE ~ emo::ji("warning"),
                items == passes ~ emo::ji("check"),
                fails > 0 ~ emo::ji("x"), 
                TRUE ~ emo::ji("question")
            ), 
            name = stringr::str_replace_all(name, "\\.", " ")) 
        # |> 
        #     select(`Test Name` = name, `Test Expression` = expression, `Test Passed` = pass)
    })
    
    output$hydro_lab_custom_qaqc_table <- renderTable({
        validate(need(input$hydro_lab_file, message = "Select a file to view custom qa/qc results."))
        validation_results <- validate::confront(uploaded_hydro_lab_data(), hydro_lab_custom_rules)
        as_tibble(summary(validation_results)) |>
            mutate(pass = case_when(
                error == TRUE ~ emo::ji("warning"), 
                warning == TRUE ~ emo::ji("warning"),
                items == passes ~ emo::ji("check"),
                fails > 0 ~ emo::ji("x"), 
                TRUE ~ emo::ji("question")
            ), 
            name = stringr::str_replace_all(name, "\\.", " ")) 
    })
    
    output$hydro_lab_wqx_formatted <- renderTable({
        req(input$hydro_lab_file)
        hydro_lab_data_wqx_formatted()
    })
    
    output$hydro_lab_download <- downloadHandler(
        filename = function() {
            hydro_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
            paste('hydro-lab-data-', hydro_signature(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(hydro_lab_data_wqx_formatted(), file)
        }
    )
    
    observeEvent(input$hydro_lab_upload, {
        donwloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
        path_to_most_recent <- str_replace_all(
            paste(
                donwloads_path,
                "/hydro-lab-data-",
                hydro_signature(),
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
        FILE_NAME =  paste("hydro-lab-data-", hydro_signature(), ".csv", sep = "")
        
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
        status <- cdx_get_status(session, dataset_id)
        
        hydro_wqx_status(status$StatusName)
    })
    
    output$hydro_upload_status <- renderUI({
        validate(need(hydro_wqx_status(), "start upload, status of upload will be shown here after completion"))
        if (hydro_wqx_status() == "Import Failed") {
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
    
    # get_path_to_most_recent_hydro_lab <- function() {
    #     path_to_download <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
    #     file_path <-
    #         str_replace_all(
    #             paste(
    #                 path_to_download,
    #                 "/hydro-lab-data-",
    #                 hydro_signature(),
    #                 ".csv",
    #                 sep = ""
    #             ),
    #             "\\\\",
    #             "/"
    #         )
    # }
    
    # alpha lab -------------------------------------------------------------------------------
    
    uploaded_alpha_lab_data <- reactive({
        purrr::map_df(input$alpha_lab_file$datapath, \(x) parse_alphalab(x))
        
    })
    
    output$alpha_lab_table <- renderTable({
        
        validate(need(input$alpha_lab_file, message = "Select a file to view"))
        uploaded_alpha_lab_data()
    })
    
    output$alpha_lab_qaqc_table <- renderTable({
        validate(need(input$alpha_lab_file, message = "Select a file to view qa/qc results."))
        data_for_validation <- alpha_lab_format_for_range_validation(uploaded_alpha_lab_data())
        print(data_for_validation)
        validation_results <- validate::confront(data_for_validation, alpha_lab_range_rules)
        as_tibble(summary(validation_results)) |>
            mutate(pass = case_when(
                error == TRUE ~ emo::ji("warning"), 
                warning == TRUE ~ emo::ji("warning"),
                items == passes ~ emo::ji("check"),
                fails > 0 ~ emo::ji("x"), 
                TRUE ~ emo::ji("question")
            ), 
            name = stringr::str_replace_all(name, "\\.", " "))  
        # |> 
        #     select(`Test Name` = name, `Test Expression` = expression, `Test Passed` = pass)
    })
    
    # output$alpha_lab_custom_qaqc_table <- renderTable({
    #     validate(need(input$alpha_lab_file, message = "Select a file to view custom qa/qc results."))
    #     validation_results <- validate::confront(uploaded_alpha_lab_data(), alpha_lab_custom_rules)
    #     as_tibble(summary(validation_results)) |>
    #         mutate(pass = case_when(
    #             error == TRUE ~ emo::ji("warning"), 
    #             warning == TRUE ~ emo::ji("warning"),
    #             items == passes ~ emo::ji("check"),
    #             fails > 0 ~ emo::ji("x"), 
    #             TRUE ~ emo::ji("question")
    #         ), 
    #         name = stringr::str_replace_all(name, "\\.", " ")) 
    #     # |> 
    #     #     select(`Test Name` = name, `Test Expression` = expression, `Test Passed` = pass)
    # })
    
    output$alpha_lab_wqx_formatted <- renderTable({
        alpha_lab_to_wqx(uploaded_alpha_lab_data())
    })

}
