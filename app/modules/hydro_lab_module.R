hydro_lab_ui <- function(id){
 ns <- NS(id)
 tabPanel("Hydro Lab",
          
          tags$h2("Hydro Lab Data"),
          sidebarLayout(
              sidebarPanel(width = 3,
                           fileInput(ns("hydro_lab_file"), "Select Hydro Lab File", multiple = TRUE),
                           actionButton(ns("reset"), "Reset"),
                           conditionalPanel(condition="input.tabs == 'additional'",
                               selectInput(ns("selected_location"), "Select Monitoring Location:", choices = NULL),
                               selectInput(ns("selected_day"), "Select Monitoring Day:", choices = NULL),
                               numericInput(ns("temperature_air"), "Enter Air Temperature Measurement", value = ""),
                               textAreaInput(ns("result_comment"), "Enter Result Comment", rows = 2),
                               actionButton(ns("add_result"), "Add Result"),
                               actionButton(ns("delete_result"), "Delete Last Added Result")
                               )
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
                                card(card_header("Raw Data"), card_body(DT::dataTableOutput(ns("hydro_lab_table")))),
                                tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - 'O', - test passed, ', 'X' - test failed, '!' - verify manually (usually safe to ignore)"),
                                layout_column_wrap(
                                    width = 1/2,
                                    card(card_header("Range based rules"), card_body(tableOutput(ns("hydro_lab_qaqc_table")))),
                                    card(card_header("Custom rules"), card_body(tableOutput(ns("hydro_lab_custom_qaqc_table"))))
                                )
                            ),
                            tabPanel(
                                "Enter Additional Data",
                                value = "additional",
                                tags$p(class = "p-3 border rounded",
                                       "Enter additional AccuWeather 'Temperature, Air' measurement and 'Result Comment' for each date and location in the sidebar panel."),
                                DT::dataTableOutput(ns("temperature_data_table")),
                                actionButton(ns("generate_formatted_df"), "Generate WQX Ready Data"),
                                textOutput(ns("check_df_message")),
                                tags$p(class = "p-3 border rounded",
                                       "If water body is too shallow, click on button below to generate empty dataframe after inputing air temperature and result comment."),
                                actionButton(ns("generate_df"), "Generate Empty WQX Data Sheet"),
                                textOutput(ns("check_empty_df_message")),
                            ),
                            tabPanel(
                                "Formatted Data",
                                tags$p(class = "p-3 border rounded",
                                       "Review WQX formatted data. Click 'Download' and then 'Upload to WQX' when ready."),
                                bslib::layout_columns(
                                    col_widths = c(2, 2, 2),
                                    downloadButton(ns("hydro_lab_download")),
                                    actionButton(ns("hydro_lab_upload"), label = "Upload to WQX", icon = shiny::icon("rocket")),
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     tags$div(HTML("<b> Starting WQX upload. Please wait 25 seconds for the upload status from CDX...</b>"),id="loadmessage")),
                                    uiOutput(ns("hydro_upload_status"))
                                ),
                                # DT::dataTableOutput("hydro_lab_wqx_formatted")
                                tableOutput(ns("hydro_lab_wqx_formatted"))
                                )
                            )
                        )
              ))
}

hydro_lab_server <- function(input, output, session){
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
                                  options = list(scrollX = TRUE, ordering = FALSE, pageLength = 25)) |>
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
}