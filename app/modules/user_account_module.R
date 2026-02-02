user_account_ui <- function(id){
    ns <- NS(id)
    tabPanel(
        "User Account",
        div(
            class = "container-fluid py-4",
            div(
                class = "row justify-content-center",
                div(
                    class = "col-lg-8 col-xl-6",
                    card(
                        card_header(
                            class = "bg-primary text-white",
                            tags$h4(class = "mb-0", icon("user-cog"), " Manage WQX Credentials")
                        ),
                        card_body(
                            class = "p-4",
                            # Credential inputs
                            div(
                                class = "mb-4",
                                selectInput(
                                    ns("wqx_username"),
                                    label = tags$span(icon("user"), " Username"),
                                    choices = cdx_account$USER_ID,
                                    width = "100%"
                                )
                            ),
                            div(
                                class = "mb-4",
                                selectInput(
                                    ns("wqx_api_key"),
                                    label = tags$span(icon("key"), " API Key"),
                                    choices = cdx_account$WQX_API_KEY,
                                    width = "100%"
                                )
                            ),
                            div(
                                class = "mb-4",
                                selectInput(
                                    ns("wqx_config_id"),
                                    label = tags$span(icon("cog"), " Config ID"),
                                    choices = cdx_account$CONFIG_ID,
                                    width = "100%"
                                )
                            ),
                            # Action button
                            div(
                                class = "d-grid gap-2 mt-4",
                                actionButton(
                                    ns("load_credential"),
                                    label = tagList(icon("sync"), " Reload Credentials"),
                                    class = "btn-primary btn-lg"
                                )
                            ),
                            # Status/error messages
                            div(
                                class = "mt-4",
                                uiOutput(ns("error_message"))
                            )
                        )
                    )
                )
            )
        )
    )
}

user_account_server <- function(input, output, session){
    ns <- session$ns
    
    output$error_message <- renderUI({
        if (!file_info$file_exists) {
            div(
                class = "alert alert-warning",
                role = "alert",
                div(
                    class = "d-flex align-items-center mb-3",
                    icon("exclamation-triangle", class = "me-2"),
                    tags$strong("Credentials file not found")
                ),
                tags$p(
                    class = "mb-3",
                    "Click 'Generate File' to create the credentials file. ",
                    "Fill in your CDX credentials, then click 'Reload Credentials'."
                ),
                actionButton(
                    ns("generate_button"),
                    label = tagList(icon("file-alt"), " Generate File"),
                    class = "btn-warning"
                )
            )
        }
    })
    
    observeEvent(input$generate_button, {
        template <- data.frame(
            WQX_API_KEY = character(0),
            USER_ID = character(0),
            CONFIG_ID = character(0)
        )
        dir.create(cdx_account_path, recursive = TRUE)
        write_csv(template, cdx_account_file)
        
        output$error_message <- renderUI({
            div(
                class = "alert alert-success",
                role = "alert",
                div(
                    class = "d-flex align-items-center mb-2",
                    icon("check-circle", class = "me-2"),
                    tags$strong("Credentials file created successfully!")
                ),
                tags$p(
                    class = "mb-1",
                    "Please enter your credentials in the CSV file located at:"
                ),
                tags$code(
                    class = "d-block p-2 bg-light rounded",
                    cdx_account_file
                )
            )
        })
    })
    
    observeEvent(input$load_credential, {
        cdx_account <- check_file(cdx_account_file)
        updateSelectInput(session, "wqx_username", "Username", choices = cdx_account$USER_ID)
        updateSelectInput(session, "wqx_api_key", "API Key", choices = cdx_account$WQX_API_KEY)
        updateSelectInput(session, "wqx_config_id", "Config ID", choices = cdx_account$CONFIG_ID)
    })
    
    selectedUsername <- reactiveVal(NULL)
    selectedApiKey <- reactiveVal(NULL)
    selectedConfigId <- reactiveVal(NULL)
    
    observeEvent(input$wqx_api_key, {
        selectedUsername(input$wqx_username)
    })
    
    observeEvent(input$wqx_api_key, {
        selectedApiKey(input$wqx_api_key)
    })
    
    observeEvent(input$wqx_config_id, {
        selectedConfigId(input$wqx_config_id)
    })
    
    list(
        selectedApiKey = selectedApiKey,
        selectedUsername = selectedUsername,
        selectedConfigId = selectedConfigId
    )
}