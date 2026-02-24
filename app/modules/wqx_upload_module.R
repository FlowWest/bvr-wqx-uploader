wqx_upload_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Upload to WQX",
        div(
            class = "container-fluid py-2",
            tags$h4("Upload to WQX", class = "mb-2"),
            div(
                class = "row mb-3",
                div(
                    class = "col-12",
                    tags$h5("Files to Upload", class = "d-inline-block mb-2"),
                    actionButton(ns("refresh_files"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-secondary mb-2 ms-2"),
                    DT::dataTableOutput(ns("pending_files_table")),
                    tags$p(class = "small text-muted mt-1", "Files in your download folder with status 'downloaded'. Once uploaded successfully, status changes to 'uploaded'.")
                )
            ),
            div(
                class = "row mb-3",
                div(
                    class = "col-12",
                    tags$h5("Uploaded Files", class = "d-inline-block mb-2"),
                    actionButton(ns("refresh_uploaded"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-secondary mb-2 ms-2"),
                    DT::dataTableOutput(ns("uploaded_files_table")),
                    tags$p(class = "small text-muted mt-1", "Files that have been successfully uploaded to WQX.")
                )
            ),
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
    
    upload_tracking_file <- file.path(cdx_account_path, "file_tracking.csv")
    
    load_file_tracking <- function() {
        if (file.exists(upload_tracking_file)) {
            read_csv(upload_tracking_file, show_col_types = FALSE)
        } else {
            data.frame(
                filename = character(),
                lab_type = character(),
                status = character(),
                date_added = character(),
                upload_date = character(),
                stringsAsFactors = FALSE
            )
        }
    }
    
    save_file_tracking <- function(df) {
        dir.create(cdx_account_path, recursive = TRUE, showWarnings = FALSE)
        write_csv(df, upload_tracking_file)
    }
    
    update_file_status <- function(filename, new_status) {
        df <- load_file_tracking()
        idx <- which(df$filename == filename)
        if (length(idx) > 0) {
            df$status[idx] <- new_status
            if (new_status == "uploaded") {
                df$upload_date[idx] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            }
        } else {
            new_row <- data.frame(
                filename = filename,
                lab_type = get_lab_type(filename),
                status = new_status,
                date_added = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                upload_date = if (new_status == "uploaded") format(Sys.time(), "%Y-%m-%d %H:%M:%S") else NA,
                stringsAsFactors = FALSE
            )
            df <- rbind(df, new_row)
        }
        save_file_tracking(df)
        return(df)
    }
    
    get_lab_type <- function(filename) {
        if (grepl("^hydro-lab-data-", filename)) {
            "Hydro Lab"
        } else if (grepl("^alpha_lab-data-", filename)) {
            "Alpha Lab"
        } else if (grepl("^bend_genetics-data-", filename)) {
            "Bend Genetics"
        } else {
            "Unknown"
        }
    }
    
    file_tracking_df <- reactiveVal(load_file_tracking())
    pending_files_refresh <- reactiveVal(0)
    uploaded_files_refresh <- reactiveVal(0)
    
    observeEvent(input$refresh_files, {
        pending_files_refresh(pending_files_refresh() + 1)
    })
    
    observeEvent(input$refresh_uploaded, {
        uploaded_files_refresh(uploaded_files_refresh() + 1)
    })
    
    observeEvent(input$refresh_uploaded, {
        file_tracking_df(load_file_tracking())
    })
    
    pending_files <- reactive({
        pending_files_refresh()
        req(account_info$selectedDownloadFolder())
        download_folder <- normalizePath(account_info$selectedDownloadFolder(), mustWork = FALSE)
        
        if (!dir.exists(download_folder)) {
            return(data.frame(
                filename = character(),
                lab_type = character(),
                stringsAsFactors = FALSE
            ))
        }
        
        all_files <- list.files(download_folder, pattern = "\\.csv$", full.names = TRUE)
        file_names <- basename(all_files)
        
        pattern <- "hydro-lab-data-|alpha_lab-data-|bend_genetics-data-"
        wqx_files <- file_names[grepl(pattern, file_names)]
        
        tracking_df <- file_tracking_df()
        uploaded_files <- tracking_df$filename[tracking_df$status == "uploaded"]
        
        pending <- wqx_files[!wqx_files %in% uploaded_files]
        
        new_files <- pending[!pending %in% tracking_df$filename]
        if (length(new_files) > 0) {
            for (f in new_files) {
                update_file_status(f, "downloaded")
            }
            file_tracking_df(load_file_tracking())
            tracking_df <- file_tracking_df()
        }
        
        pending_df <- tracking_df[tracking_df$status == "downloaded", ]
        
        if (nrow(pending_df) == 0) {
            return(data.frame(
                filename = character(),
                lab_type = character(),
                status = character(),
                date_added = character(),
                stringsAsFactors = FALSE
            ))
        }
        
        pending_df[, c("filename", "lab_type", "status", "date_added")]
    })
    
    output$pending_files_table <- DT::renderDataTable({
        df <- pending_files()
        if (nrow(df) == 0) {
            DT::datatable(
                data.frame(message = "No pending files to upload"),
                options = list(dom = "t", ordering = FALSE, searching = FALSE)
            )
        } else {
            DT::datatable(
                df,
                rownames = FALSE,
                colnames = c("Filename", "Lab Type", "Status", "Date Added"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
            )
        }
    })
    
    output$uploaded_files_table <- DT::renderDataTable({
        uploaded_files_refresh()
        df <- file_tracking_df()
        df <- df[df$status == "uploaded", ]
        if (nrow(df) == 0) {
            DT::datatable(
                data.frame(message = "No files have been uploaded yet"),
                options = list(dom = "t", ordering = FALSE, searching = FALSE)
            )
        } else {
            DT::datatable(
                df[, c("filename", "lab_type", "upload_date")],
                rownames = FALSE,
                colnames = c("Filename", "Lab Type", "Upload Date"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
            )
        }
    })
    
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
    
    observeEvent(upload_result(), {
        req(upload_result())
        req(input$wqx_file)
        
        if (upload_result()$StatusName != "Import Failed") {
            update_file_status(input$wqx_file$name, "uploaded")
            file_tracking_df(load_file_tracking())
        }
    })
}
