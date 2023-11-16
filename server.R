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
    
    hydro_lab_data_wqx <- reactive({
        hydro_lab_to_wqx(uploaded_hydro_lab_data())
    })
    hydro_lab_data_wqx_formatted <- reactive({
        append_input_data(hydro_lab_data_wqx(), input$temperature_air, input$result_comment)
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
            write.csv(hydro_lab_data_wqx_formatted(), file, row.names = FALSE)
        }
    )
    
    hydro_wqx_status <- eventReactive(input$hydro_lab_upload, {
        downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
        path_to_most_recent <- str_replace_all(
            paste(
                downloads_path,
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
        return(cdx_get_status(session, dataset_id))
        
        # hydro_wqx_status(status$StatusName)
    })
    
    output$hydro_upload_status <- renderUI({
        validate(need(hydro_wqx_status(), "start upload, status of upload will be shown here after completion"))
        if (hydro_wqx_status()$StatusName == "Import Failed") {
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
    
    # alpha lab -------------------------------------------------------------------------------
    
    uploaded_alpha_lab_data <- reactive({
        purrr::map_df(input$alpha_lab_file$datapath, \(x) parse_alphalab(x))
        
    })
    alpha_signature <- reactiveVal(NULL)
    alpha_wqx_status <- reactiveVal(NULL)
    
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
    
    
    alpha_lab_data_wqx_formatted <- reactive({
        alpha_lab_to_wqx(uploaded_alpha_lab_data())
    })
    
    output$alpha_lab_wqx_formatted <- renderTable({
        req(input$alpha_lab_file)
        alpha_lab_data_wqx_formatted()
    })
    
    
    output$alpha_lab_download <- downloadHandler(
        filename = function() {
            alpha_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
            paste('alpha-lab-data-', alpha_signature(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(alpha_lab_data_wqx_formatted(), file, row.names = FALSE)
        }
    )
    
    alpha_wqx_status <- eventReactive(input$alpha_lab_upload, {
        downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
        path_to_most_recent <- str_replace_all(
            paste(
                downloads_path,
                "/alpha-lab-data-",
                alpha_signature(),
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
        FILE_NAME =  paste("alpha-lab-data-", hydro_signature(), ".csv", sep = "")
        
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
    
    output$alpha_upload_status <- renderUI({
        validate(need(alpha_wqx_status(), "start upload, status of upload will be shown here after completion"))
        if (alpha_wqx_status()$StatusName == "Import Failed") {
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
    
    
    #bend-genetics -------------------------------------------------------

    uploaded_bend_genetics_data <- reactive({
        if (!any(endsWith(input$bend_genetics_file$datapath, ".csv"))) {
            sendSweetAlert(
                session = session,
                title = "Error",
                text = "at least one file is not bend genetics, please try uploading again",
                type = "error"
            )
        }
        purrr::map_df(input$bend_genetics_file$datapath, \(x) parse_bend_genetics(x))
        
    })
    
    bend_genetics_comparison_table <- reactive({
        uploaded_bend_genetics_data() |> 
            tidyr::pivot_wider(names_from = "Target", values_from = "Result", values_fn = as.numeric) |> 
            rename("Microcycstin Nod" = "Microcystin/Nod.")
    })
    
    bend_signature <- reactiveVal(NULL)
    bend_wqx_status <- reactiveVal(NULL)
    
    bend_genetics_data_wqx <- reactive({
        bend_genetics_to_wqx(uploaded_bend_genetics_data())
    })
    
    bend_genetics_data_wqx_formatted <- reactive({
        clean_bend_wqx(bend_genetics_data_wqx())
    })
    bend_path_to_most_recent_download <- reactive({
        
    })
    
    output$bend_genetics_table <- renderTable({
        
        validate(need(input$bend_genetics_file, message = "Select a file to view"))
        uploaded_bend_genetics_data()
    })
    
    output$bend_genetics_qaqc_table <- renderTable({
        validate(need(input$bend_genetics_file, message = "Select a file to view qa/qc results."))
        validation_results <- validate::confront(bend_genetics_comparison_table(), bend_genetics_range_rules)
        as_tibble(summary(validation_results)) |>
            mutate(pass = case_when(
                error == TRUE ~ emo::ji("warning"), 
                warning == TRUE ~ emo::ji("warning"),
                items == passes ~ emo::ji("check"),
                fails > 0 ~ emo::ji("x"), 
                TRUE ~ emo::ji("question")
            ), 
            name = stringr::str_replace_all(name, "\\.", " "))|>
            select(-expression)
    })
    
    # output$bend_genetics_custom_qaqc_table <- renderTable({
    #     validate(need(input$bend_genetics_file, message = "Select a file to view custom qa/qc results."))
    #     validation_results <- validate::confront(uploaded_bend_genetics_data(), bend_genetics_custom_rules)
    #     as_tibble(summary(validation_results)) |>
    #         mutate(pass = case_when(
    #             error == TRUE ~ emo::ji("warning"), 
    #             warning == TRUE ~ emo::ji("warning"),
    #             items == passes ~ emo::ji("check"),
    #             fails > 0 ~ emo::ji("x"), 
    #             TRUE ~ emo::ji("question")
    #         ), 
    #         name = stringr::str_replace_all(name, "\\.", " ")) 
    # })
    
    output$bend_genetics_wqx_formatted <- renderTable({
        req(input$bend_genetics_file)
        bend_genetics_data_wqx_formatted()
    })
    
    output$bend_genetics_download <- downloadHandler(
        filename = function() {
            bend_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
            paste('bend-lab-data-', bend_signature(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(bend_genetics_data_wqx_formatted(), file, row.names = FALSE)
        }
    )
    
    bend_wqx_status <- eventReactive(input$bend_genetics_upload, {
        donwloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
        path_to_most_recent <- str_replace_all(
            paste(
                donwloads_path,
                "/bend-lab-data-",
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
        FILE_NAME =  paste("bend-lab-data-", bend_signature(), ".csv", sep = "")
        
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
    
    output$bend_upload_status <- renderUI({
        validate(need(bend_wqx_status(), "start upload, status of upload will be shown here after completion"))
        if (bend_wqx_status() == "Import Failed") {
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
    