function(input, output, session) {
    # hydro lab -------------------------------------------------------------------------
    uploaded_hydro_lab_data <- reactive({
        req(input$hydro_lab_file$datapath)
        if (!any(endsWith(input$hydro_lab_file$datapath, c(".csv", ".CSV")))) {
            sendSweetAlert(
                session = session,
                title = "Error",
                text = "Please upload valid HydroLab data files with a '.csv' extension.",
                type = "error"
            )
            return(NULL)
        }
        purrr::map_df(input$hydro_lab_file$datapath, \(x) parse_hydrolab(x))
    })
    
    # handle data editing by the user
    rvals <- reactiveValues(data = NULL)
    
    observe({
        rvals$data <- uploaded_hydro_lab_data()
    })
    
    observeEvent(input$reset, {

        rvals$data <- NULL
        hydro_signature <- NULL
        hydro_wqx_status <- NULL
        common_hydro_lab_wqx_data <- NULL
        hydro_lab_data$formatted_data <- NULL
        temp_data$filtered_data <- NULL
        wqx_data$empty_data <- NULL
    })
    
    observeEvent(input$hydro_lab_table_cell_edit, {
        rvals$data <<- DT::editData(rvals$data, input$hydro_lab_table_cell_edit)
    })
    
    output$hydro_lab_table <- DT::renderDataTable({
        if (is.null(rvals$data)) {
            return(NULL)
        }
        
        editable_cols <- rep(TRUE, 16)
        editable_cols[1:2] <- FALSE
        editable_cols[16] <- FALSE
        validate(need(input$hydro_lab_file, message = "Select a file to view"))
        DT::datatable(rvals$data, editable = list(target = "cell", disable = list(columns = c(1,2, 16))),
                      options = list(scrollX = TRUE, dom = "t", ordering = FALSE)) |> 
            DT::formatStyle(
                c("CHL"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0.03, 1000), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("Temp"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0, 30), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("Depth10"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0.01,10), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("SpCond"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0.01, 1), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("Sal"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0.01, 1), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("TDS"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0.01, 1), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("DO%"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0.01, 150), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("DO"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(0, 20), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("pH"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(5, 11), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("Turb"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(1.5, 1000), c("#f29f99", "white", "#f29f99"))
            ) |> 
            DT::formatStyle(
                c("PCY"), 
                target = "cel",
                backgroundColor = DT::styleInterval(c(100, 200000), c("#f29f99", "white", "#f29f99"))
            ) 
    })
    
    
    output$hydro_lab_qaqc_table <- renderTable({
        if (is.null(rvals$data)) {
            return(NULL)
        }
        validate(need(rvals$data, message = "Select a file to view qa/qc results."))
        validation_results <- validate::confront(rvals$data, hydro_lab_range_rules)
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
    
    output$hydro_lab_custom_qaqc_table <- renderTable({
        if (is.null(rvals$data)) {
            return(NULL)
        }
        validate(need(rvals$data, message = "Select a file to view custom qa/qc results."))
        validation_results <- validate::confront(rvals$data, hydro_lab_custom_rules)
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
    hydro_signature <- reactiveVal(NULL)
    hydro_wqx_status <- reactiveVal(NULL)
    common_hydro_lab_wqx_data <- reactiveVal(NULL)
    hydro_lab_data <- reactiveValues(formatted_data = NULL)
    
    hydro_lab_data_wqx <- reactive({
        hydro_lab_data$formatted_data <- hydro_lab_to_wqx(rvals$data)
    })

    observe({
        if (is.null(rvals$data)) {
            return(NULL)
        }
        updateSelectInput(session, "selected_location", "Select Monitoring Location:",
        choices = unique(hydro_lab_data_wqx()$`Monitoring Location ID`))
    })
    # 
    hydro_lab_dates <- reactive({
        if (is.null(rvals$data)) {
            return(NULL)
        }
        req(input$selected_location)
        hydro_lab_locations <- hydro_lab_data_wqx()  |>
            filter(`Monitoring Location ID` == input$selected_location) |>
            select(`Activity Start Date`)
        })

    observe({
        updateSelectInput(
            session,
            "selected_day",
            "Select Monitoring Day:",
            choices = unique(as.character(hydro_lab_dates()$"Activity Start Date"))
        )
    })
    
    temp_data <- reactiveValues(filtered_data = NULL)
    wqx_data <- reactiveValues(empty_data = NULL)
    
    observeEvent(input$add_result,{
        if (!is.null(hydro_lab_data$formatted_data)){
            hydro_lab_data_wqx_filtered <- hydro_lab_data$formatted_data |>
                filter(`Activity Start Date` == input$selected_day & `Monitoring Location ID` == input$selected_location)
            with_temp_data <- append_input_data(hydro_lab_data_wqx_filtered, input$temperature_air, input$result_comment)
            with_temp_data <- with_temp_data |>
                filter(`Characteristic Name` == "Temperature, Air") |> 
                tail(1)
            hydro_lab_data$formatted_data <- hydro_lab_data$formatted_data |> 
                mutate(`Result Comment` = ifelse(`Activity Start Date` == input$selected_day & `Monitoring Location ID` == input$selected_location, input$result_comment, `Result Comment`))
            hydro_lab_data$formatted_data <- rbind(hydro_lab_data$formatted_data, with_temp_data)
            temp_data$filtered_data <- rbind(temp_data$filtered_data, with_temp_data)
        }else{
            with_temp_data <- generate_empty_data(input$temperature_air, input$result_comment) |> 
                filter(`Characteristic Name` == "Temperature, Air")
            temp_data$filtered_data <- rbind(temp_data$filtered_data, with_temp_data)
            new_sheet <- generate_empty_data(input$temperature_air, input$result_comment)
            wqx_data$empty_data <- rbind(wqx_data$empty_data, new_sheet)
            
        }
    })
    
    observeEvent(input$delete_result, {
        req(input$add_result)
        temp_data$filtered_data <- temp_data$filtered_data %>%
            slice(-n())
        if (!is.null(hydro_lab_data$formatted_data)){
            last_row <- tail(hydro_lab_data$formatted_data, 1)
            last_location <- last_row$`Monitoring Location ID`
            last_date <- last_row$`Activity Start Date`

            hydro_lab_data$formatted_data <- hydro_lab_data$formatted_data %>%
                mutate(`Result Comment` = ifelse(`Activity Start Date` == last_date & `Monitoring Location ID` == last_location, "", `Result Comment`)) |> 
                slice(-n())
                
            }
        else if(!is.null(wqx_data$empty_data)){
            wqx_data$empty_data <- wqx_data$empty_data |> 
                slice_head(n = -12)
        }
    })

    
    output$temperature_data_table <- DT::renderDataTable({

        DT::datatable(temp_data$filtered_data, 
                      editable = list(target = "cell", disable = list(columns = c(2:35))),
                      options = list(scrollX = TRUE, dom = "t", ordering = FALSE),
                      caption = "Additional data - please check that the 'Monitoring Location ID' matches the 'Project ID'.")
    })
    #disable edit ML
    observeEvent(input$temperature_data_table_cell_edit, {
        if (!is.null(hydro_lab_data$formatted_data)){
            temp_data$filtered_data <<- DT::editData(temp_data$filtered_data, input$temperature_data_table_cell_edit)
            str(input$temperature_data_table_cell_edit)
            date_to_update <- temp_data$filtered_data[input$temperature_data_table_cell_edit$row, ]$`Activity Start Date`
            location_to_update <- temp_data$filtered_data[input$temperature_data_table_cell_edit$row, ]$`Monitoring Location ID`
            hydro_lab_data$formatted_data <<- hydro_lab_data$formatted_data |> 
                mutate(`Project ID` = case_when(`Activity Start Date` == date_to_update & `Monitoring Location ID` == location_to_update ~
                                                    input$temperature_data_table_cell_edit$value, TRUE ~ `Project ID`))}
             
        else{
            temp_data$filtered_data <<- DT::editData(temp_data$filtered_data, input$temperature_data_table_cell_edit)
            # wqx_data$empty_data <<- DT::editData(temp_data$empty_data, input$temperature_data_table_cell_edit)
        }
    })
    hydro_lab_data_wqx_formatted <-  eventReactive(input$generate_formatted_df, {
        req(input$hydro_lab_file$datapath)
        hydro_lab_data$formatted_data
    })
    
    hydro_lab_data_wqx_empty <- eventReactive(input$generate_df, {
        wqx_data$empty_data
    })
    
    observeEvent(input$generate_formatted_df, {
        common_hydro_lab_wqx_data(hydro_lab_data_wqx_formatted())
        output$check_df_message <- renderText({
            Sys.sleep(0.5)
            "Check Formatted Data tab for generated WQX data sheet. To delete the added data, click on 'Delete Last Added Result'." 
        })
    })
    
    observeEvent(input$generate_df, {
        common_hydro_lab_wqx_data(hydro_lab_data_wqx_empty())
        output$check_empty_df_message <- renderText({
            Sys.sleep(0.5)
           "Check 'Formatted Data' tab for generated empty data sheet. To delete the added data, click on 'Delete Last Added Result' button."
       })
    })

    
    output$hydro_lab_wqx_formatted <- renderTable({
        req(common_hydro_lab_wqx_data())
        common_hydro_lab_wqx_data()
    })
    

    
    output$hydro_lab_download <- downloadHandler(
        filename = function() {
            hydro_signature(format(lubridate::now(), "%Y%m%d_%H%M%S"))
            paste('hydro-lab-data-', hydro_signature(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(common_hydro_lab_wqx_data(), file, row.names = FALSE)
        }
    )

    output$error_message <- renderUI({
        if (!file_info$file_exists) {
            tagList(
                HTML("<p style='color: red;'> Click 'Generate File' to create cdx-account-info.csv' in 'Documents/CDX_Account'. Fill in credentials, then click 'Load Credential'. Check manual for instructions to obtain credentials.</p>"),
                actionButton("generate_button", "Generate File")
            )
        } 
    })
    
    observeEvent(input$generate_button, {
        template <- data.frame(WQX_API_KEY = character(0), USER_ID = character(0), CONFIG_ID = character(0))
        dir.create(cdx_account_path, recursive = TRUE)
        write_csv(template, cdx_account_file)
        

        # Update the error message
        output$error_message <- renderUI({
            u_name <- Sys.getenv("USERNAME")
            tagList(
                    tags$p("Credential file successfully generated! Please enter information in CSV located at", style='color: green;'), 
                    tags$p(paste0("C:\\Users\\", u_name, "\\Documents\\CDX_Account\\cdx-account-info.csv"), style='color: green;')
            )
        })
        
    })
    
    observeEvent(input$load_credential,{
        cdx_account <- check_file(cdx_account_file)
        updateSelectInput(session, "wqx_username", "Username", choices = cdx_account$USER_ID)
        updateSelectInput(session, "wqx_api_key", "API Key", choices = cdx_account$WQX_API_KEY)
        updateSelectInput(session, "wqx_config_id", "Config ID", choices = cdx_account$CONFIG_ID)
    })
    # observeEvent(input$add_credential, {
    #     req(input$update_api_key)
    #     req(input$update_user_name)
    #     req(input$update_config_id)
    #     
    #     
    #     new_row <- data.frame(WQX_API_KEY = input$update_api_key,
    #                           USER_ID = input$update_user_name,
    #                           CONFIG_ID = input$update_config_id)
    #     
    #     cdx_account <- rbind(cdx_account, new_row)
    #     write_csv(cdx_account, cdx_account_file)
    #     updateSelectInput(session, "wqx_username", "Username", 
    #                                         choices = cdx_account$USER_ID)
    #     updateSelectInput(session, "wqx_api_key", "API Key", 
    #                                         choices = cdx_account$WQX_API_KEY)
    #     updateSelectInput(session, "wqx_config_id", "Config ID",
    #                                         choices = cdx_account$CONFIG_ID)
    # })
    
    
    
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
    # session$onSessionEnded(function() {
    #     stopApp()
    # })
}
    