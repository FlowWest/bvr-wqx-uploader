function(input, output, session) {

    # hydro lab -------------------------------------------------------------------------
    uploaded_hydro_lab_data <- reactive({
        purrr::map_df(input$hydro_lab_file$datapath, \(x) parse_hydrolab(x))

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
        hydro_lab_to_wqx(uploaded_hydro_lab_data())
    })
    
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
