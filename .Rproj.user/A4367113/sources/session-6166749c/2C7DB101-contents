function(input, output, session) {

    # hydro lab 
    
    uploaded_data <- reactive({
        purrr::map_df(input$hydro_lab_file$datapath, \(x) parse_hydrolab(x))

    })
    
    
    observe({
        print(dput(uploaded_data()))
    })
    
    output$hydro_lab_table <- renderTable({
        
        validate(need(input$hydro_lab_file, message = "Select a file to view"))
        uploaded_data()
    })
    
    output$hydro_lab_qaqc_table <- renderTable({
        validate(need(input$hydro_lab_file, message = "Select a file to view qa/qc results."))
        validation_results <- validate::confront(uploaded_data(), hydro_lab_range_rules)
        as_tibble(summary(validation_results)) |>
            mutate(pass = ifelse(fails == 0, emo::ji("check"), emo::ji("x")), 
                   name = stringr::str_replace_all(name, "\\.", " ")) |> 
            select(`Test Name` = name, `Test Expression` = expression, `Test Passed` = pass)
    })
    
    output$hydro_lab_custom_qaqc_table <- renderTable({
        validate(need(input$hydro_lab_file, message = "Select a file to view custom qa/qc results."))
        validation_results <- validate::confront(uploaded_data(), hydro_lab_custom_rules)
        as_tibble(summary(validation_results)) |>
            mutate(pass = ifelse(fails == 0, emo::ji("check"), emo::ji("x")), 
                   name = stringr::str_replace_all(name, "\\.", " ")) |> 
            select(`Test Name` = name, `Test Expression` = expression, `Test Passed` = pass)
    })

}
