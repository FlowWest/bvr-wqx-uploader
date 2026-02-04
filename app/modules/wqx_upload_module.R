wqx_upload_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Upload to WQX",
        div(
            class = "container-fluid py-2",
            tags$h4("Upload to WQX", class = "mb-2"),
            div(
                class = "row",
                div(
                    class = "col-md-6",
                    tags$p(class = "small text-muted mb-2", "Select a previously downloaded WQX-formatted CSV file to upload to CDX."),
                    fileInput(ns("wqx_file"), label = "Select CSV File", accept = ".csv", width = "100%"),
                    actionButton(ns("upload_to_wqx"), label = "Upload to WQX", icon = icon("rocket"), class = "btn-success btn-sm"),
                    conditionalPanel(
                        condition = "$('html').hasClass('shiny-busy')",
                        ns = ns,
                        div(class = "alert alert-info mt-2 small", "Uploading to WQX. Please wait 25 seconds for the status from CDX...")
                    ),
                    div(class = "mt-2", uiOutput(ns("upload_status")))
                )
            ),
            div(class = "mt-2", DT::dataTableOutput(ns("file_preview")))
        )
    )
}

wqx_upload_server <- function(input, output, session, account_info) {
    ns <- session$ns
    
    # Preview uploaded file
    output$file_preview <- DT::renderDataTable({
        req(input$wqx_file)
        df <- read_csv(input$wqx_file$datapath, show_col_types = FALSE)
        DT::datatable(df, options = list(pageLength = -1, scrollX = TRUE, searching = FALSE, lengthChange = FALSE, paging = FALSE, info = FALSE))
    })
    
    # Upload to WQX
    upload_result <- eventReactive(input$upload_to_wqx, {
        req(input$wqx_file)
        
        uploaded_file <- input$wqx_file
        FILE_PATH <- uploaded_file$datapath
        FILE_NAME <- uploaded_file$name
        
        API_KEY <- account_info$selectedApiKey()
        USER_ID <- account_info$selectedUsername()
        CONFIG_ID <- account_info$selectedConfigId()
        
        cat("WQX UPLOAD - USER_ID:", USER_ID, "\n")
        cat("WQX UPLOAD - FILE_PATH:", FILE_PATH, "\n")
        cat("WQX UPLOAD - FILE_NAME:", FILE_NAME, "\n")
        cat("WQX UPLOAD - CONFIG_ID:", CONFIG_ID, "\n")
        
        spsComps::shinyCatch({message("Sending request to CDX Web...")}, position = "bottom-full-width")
        
        cdx_session <- cdx(USER_ID, API_KEY, FILE_PATH, FILE_NAME)
        file_id <- cdx_upload(session = cdx_session)
        dataset_id <- cdx_import(
            session = cdx_session,
            file_id = file_id,
            config_id = CONFIG_ID,
            params = c("newOrExistingData", "0")
        )
        
        Sys.sleep(25)
        return(cdx_get_status(cdx_session, dataset_id))
    })
    
    output$upload_status <- renderUI({
        shiny::validate(shiny::need(upload_result(), "Select a file and click 'Upload to WQX' to begin."))
        
        if (upload_result()$StatusName == "Import Failed") {
            div(
                class = "alert alert-danger",
                tags$b("Import failed."), " Please check your file and retry."
            )
        } else {
            div(
                class = "alert alert-success",
                tags$b("Upload successful!"),
                tags$br(),
                "Your data is being imported to CDX. Check your email or the CDX website for final confirmation."
            )
        }
    })
}
