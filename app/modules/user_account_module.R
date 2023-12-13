user_account_ui <- function(id){
    ns <- NS(id)    
    tabPanel("User Account",
                 tags$h2("Manage WQX Credentials"),
                 sidebarLayout(
                     sidebarPanel(width = 4,
                                  selectInput(ns("wqx_username"), "Username", choices = cdx_account$USER_ID),
                                  selectInput(ns("wqx_api_key"), "API Key", choices = cdx_account$WQX_API_KEY),
                                  selectInput(ns("wqx_config_id"), "Config ID", choices = cdx_account$CONFIG_ID),
                                  ),
                mainPanel(
                    uiOutput(ns("error_message")),
                    br(),
                    actionButton(ns("load_credential"), "Load Credential")
    
                )
            )
    
        )
}

user_account_server <- function(input, output, session){
            ns <- session$ns
            output$error_message <- renderUI({
                if (!file_info$file_exists) {
                    tagList(
                        HTML("<p style='color: red;'> Click 'Generate File' to create cdx-account-info.csv' in 'Documents/CDX_Account'. Fill in credentials, then click 'Load Credential'. Check manual for instructions to obtain credentials.</p>"),
                        actionButton(ns("generate_button"), "Generate File")
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
}