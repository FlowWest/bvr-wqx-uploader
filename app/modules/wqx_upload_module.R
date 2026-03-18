wqx_upload_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Upload to WQX",
        div(
            class = "container-fluid py-3",
            tags$h4("Upload to WQX", class = "mb-3"),
            div(
                class = "row g-3",
                div(
                    class = "col-lg-6",
                    card(
                        full_screen = TRUE,
                        card_header(
                            class = "d-flex justify-content-between align-items-center",
                            tags$span(tags$i(class = "fa fa-clock-o me-2"), "Pending Files"),
                            actionButton(ns("refresh_files"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-outline-secondary")
                        ),
                        card_body(
                            DT::dataTableOutput(ns("pending_files_table")),
                            tags$p(class = "small text-muted mt-2 mb-0", "Files in your download folder with status 'downloaded'. Click a row to preview and upload.")
                        )
                    )
                ),
                div(
                    class = "col-lg-6",
                    card(
                        full_screen = TRUE,
                        card_header(
                            class = "d-flex justify-content-between align-items-center",
                            tags$span(tags$i(class = "fa fa-check-circle me-2"), "Uploaded Files"),
                            actionButton(ns("refresh_uploaded"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-outline-secondary")
                        ),
                        card_body(
                            DT::dataTableOutput(ns("uploaded_files_table")),
                            tags$p(class = "small text-muted mt-2 mb-0", "Files that have been successfully uploaded to WQX.")
                        )
                    )
                )
            ),
            div(
                class = "row mt-3",
                div(
                    class = "col-12",
                    card(
                        full_screen = TRUE,
                        card_header(tags$span(tags$i(class = "fa fa-table me-2"), "File Preview")),
                        card_body(
                            uiOutput(ns("selected_file_display")),
                            DT::dataTableOutput(ns("file_preview"))
                        )
                    )
                )
            ),
            div(
                class = "row mt-3",
                div(
                    class = "col-12",
                    card(
                        full_screen = TRUE,
                        card_header(
                            tags$span(tags$i(class = "fa fa-cloud-upload me-2"), "Upload to CDX/WQX")
                        ),
                        card_body(
                            div(
                                class = "row g-3 align-items-center",
                                div(
                                    class = "col-md-6",
                                    uiOutput(ns("download_folder_display"))
                                ),
                                div(
                                    class = "col-md-6 d-flex align-items-end",
                                    actionButton(ns("upload_to_wqx"), label = "Upload to WQX", icon = icon("rocket"), class = "btn-success btn-lg"),
                                    conditionalPanel(
                                        condition = "$('html').hasClass('shiny-busy')",
                                        ns = ns,
                                        div(class = "alert alert-info mt-3 mb-0", tags$i(class = "fa fa-spinner fa-spin me-2"), "Uploading to WQX. Please wait 25 seconds for the status from CDX...")
                                    )
                                )
                            ),
                            div(class = "mt-3", uiOutput(ns("upload_status")))
                        )
                    )
                )
            )
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
    
    output$download_folder_display <- renderUI({
        req(account_info$selectedDownloadFolder())
        download_folder <- account_info$selectedDownloadFolder()
        tagList(
            tags$p(class = "small text-muted mb-1", "Download folder: ", download_folder,
                tags$a(href = "#", onclick = paste0("Shiny.setInputValue('", ns("open_folder"), "', '", download_folder, "', {priority: 'event'}); return false;"), 
                       "(open)")),
            tags$small(class = "text-muted", "Files saved from Format Data tabs appear here"))
    })
    
    observeEvent(input$open_folder, {
        if (dir.exists(input$open_folder)) {
            shell.exec(normalizePath(input$open_folder))
        }
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
                selection = "single",
                colnames = c("Filename", "Lab Type", "Status", "Date Added"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
            )
        }
    })
    
    selected_file <- reactiveVal(NULL)
    
    observeEvent(input$pending_files_table_rows_selected, {
        row <- input$pending_files_table_rows_selected
        df <- pending_files()
        if (!is.null(row) && row <= nrow(df)) {
            download_folder <- normalizePath(account_info$selectedDownloadFolder(), mustWork = FALSE)
            file_path <- file.path(download_folder, df$filename[row])
            selected_file(list(name = df$filename[row], path = file_path))
        }
    })
    
    output$selected_file_display <- renderUI({
        if (is.null(selected_file())) {
            tags$p(class = "small text-muted mb-2", "Click a file in the Pending Files table to select it for upload.")
        } else {
            tags$p(class = "small mb-2", tags$i(class = "fa fa-file me-1"), tags$b(selected_file()$name))
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
                df[, c("filename", "lab_type", "status", "upload_date")],
                rownames = FALSE,
                colnames = c("Filename", "Lab Type", "Status", "Upload Date"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
            )
        }
    })
    
    output$file_preview <- DT::renderDataTable({
        req(selected_file())
        req(file.exists(selected_file()$path))
        df <- read_csv(selected_file()$path, show_col_types = FALSE)
        DT::datatable(df, options = list(pageLength = -1, scrollX = TRUE, searching = FALSE, lengthChange = FALSE, paging = FALSE, info = FALSE))
    })
    
    # Upload to WQX
    upload_result <- eventReactive(input$upload_to_wqx, {
        req(selected_file())
        
        FILE_PATH <- selected_file()$path
        FILE_NAME <- selected_file()$name
        
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
        req(selected_file())
        
        if (upload_result()$StatusName != "Import Failed") {
            update_file_status(selected_file()$name, "uploaded")
            file_tracking_df(load_file_tracking())
        }
    })
}
