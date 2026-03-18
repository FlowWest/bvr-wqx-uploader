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
                    class = "col-lg-6 d-flex flex-column",
                    div(
                        class = "flex-grow-1",
                        card(
                            full_screen = TRUE,
                            style = "height: 100%;",
                            card_header(
                                class = "d-flex justify-content-between align-items-center",
                                tags$span(tags$i(class = "fa fa-clock-o me-2"), "Pending Upload"),
                                actionButton(ns("refresh_files"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-outline-secondary")
                            ),
                            card_body(
                                shinycssloaders::withSpinner(DT::dataTableOutput(ns("pending_files_table"))),
                                tags$p(class = "small text-muted mt-2 mb-0", "Click a row to preview and upload.")
                            )
                        )
                    )
                ),
                div(
                    class = "col-lg-6 d-flex flex-column",
                    div(
                        class = "mb-3 flex-grow-1",
                        card(
                            full_screen = TRUE,
                            style = "background-color: #d4edda; height: 100%;",
                            card_header(
                                class = "d-flex justify-content-between align-items-center",
                                tags$span(tags$i(class = "fa fa-check-circle me-2"), "Successful Uploads"),
                                actionButton(ns("refresh_uploaded"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-outline-secondary")
                            ),
                            card_body(
                                shinycssloaders::withSpinner(DT::dataTableOutput(ns("uploaded_files_table"))),
                                tags$p(class = "small text-muted mt-2 mb-0", "Files successfully uploaded to WQX.")
                            )
                        )
                    ),
                    div(
                        class = "flex-grow-1",
                        card(
                            full_screen = TRUE,
                            style = "background-color: #f8d7da; height: 100%;",
                            card_header(
                                class = "d-flex justify-content-between align-items-center",
                                tags$span(tags$i(class = "fa fa-exclamation-circle me-2"), "Failed Uploads"),
                                actionButton(ns("refresh_failed"), label = "Refresh", icon = icon("refresh"), class = "btn-xs btn-outline-secondary")
                            ),
                            card_body(
                                shinycssloaders::withSpinner(DT::dataTableOutput(ns("failed_files_table"))),
                                tags$p(class = "small text-muted mt-2 mb-0", "Files that failed to upload.")
                            )
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
                                    class = "col-md-8",
                                    uiOutput(ns("download_folder_display"))
                                ),
                                div(
                                    class = "col-md-4",
                                    div(
                                        actionButton(ns("upload_to_wqx"), label = "Upload to WQX", icon = icon("rocket"), class = "btn-success btn-lg w-100 mb-2")
                                    ),
                                    conditionalPanel(
                                        condition = "$('html').hasClass('shiny-busy')",
                                        ns = ns,
                                        div(class = "alert alert-info mb-0", tags$i(class = "fa fa-spinner fa-spin me-2"), "Uploading... please wait...")
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
    
    read_csv_cached <- memoise(function(path) {
        read_csv(path, show_col_types = FALSE)
    })
    
    upload_tracking_file <- file.path(cdx_account_path, "file_tracking.csv")
    
    load_file_tracking <- function() {
        if (file.exists(upload_tracking_file)) {
            df <- read_csv(upload_tracking_file, show_col_types = FALSE)
            if ("uploaded_by" %in% names(df)) {
                df <- df |> rename(username = uploaded_by)
            }
            if (!"username" %in% names(df)) {
                df$username <- NA_character_
            }
            df
        } else {
            data.frame(
                filename = character(),
                lab_type = character(),
                status = character(),
                date_added = character(),
                upload_date = character(),
                username = character(),
                stringsAsFactors = FALSE
            )
        }
    }
    
    save_file_tracking <- function(df) {
        dir.create(cdx_account_path, recursive = TRUE, showWarnings = FALSE)
        write_csv(df, upload_tracking_file)
    }
    
    update_file_status <- function(filename, new_status, username = NULL) {
        df <- load_file_tracking()
        if (!"username" %in% names(df)) {
            df$username <- NA_character_
        }
        idx <- which(df$filename == filename)
        if (length(idx) > 0) {
            df$status[idx] <- new_status
            if (new_status == "uploaded") {
                df$upload_date[idx] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            }
            if (!is.null(username)) {
                df$username[idx] <- username
            }
        } else {
            new_row <- data.frame(
                filename = filename,
                lab_type = get_lab_type(filename),
                status = new_status,
                date_added = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                upload_date = if (new_status == "uploaded") format(Sys.time(), "%Y-%m-%d %H:%M:%S") else NA,
                username = if (!is.null(username)) username else NA,
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
    failed_files_refresh <- reactiveVal(0)
    
    observeEvent(input$refresh_files, {
        pending_files_refresh(pending_files_refresh() + 1)
    })
    
    observeEvent(input$refresh_uploaded, {
        uploaded_files_refresh(uploaded_files_refresh() + 1)
    })
    
    observeEvent(input$refresh_failed, {
        failed_files_refresh(failed_files_refresh() + 1)
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
        failed_files <- tracking_df$filename[tracking_df$status == "upload_failed"]
        exclude_files <- c(uploaded_files, failed_files)
        
        pending <- wqx_files[!wqx_files %in% exclude_files]
        
        new_files <- pending[!pending %in% tracking_df$filename]
        if (length(new_files) > 0) {
            username <- account_info$selectedUsername()
            for (f in new_files) {
                update_file_status(f, "downloaded", username)
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
                username = character(),
                stringsAsFactors = FALSE
            ))
        }
        
        pending_df[, c("filename", "lab_type", "status", "date_added", "username")]
    })
    
    output$pending_files_table <- DT::renderDataTable({
        df <- pending_files()
        if (nrow(df) == 0) {
            DT::datatable(
                data.frame(message = "No pending files to upload"),
                options = list(dom = "t", ordering = FALSE, searching = FALSE)
            )
        } else {
            df$date_added <- format(
                lubridate::with_tz(df$date_added, tzone = "America/Los_Angeles"),
                "%Y-%m-%d %I:%M %p PST"
            )
            DT::datatable(
                df,
                rownames = FALSE,
                selection = "single",
                colnames = c("Filename", "Lab Type", "Status", "Date Added", "Username"),
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
            df$upload_date <- format(
                lubridate::with_tz(df$upload_date, tzone = "America/Los_Angeles"),
                "%Y-%m-%d %I:%M %p PST"
            )
            DT::datatable(
                df[, c("filename", "lab_type", "status", "upload_date", "username")],
                rownames = FALSE,
                colnames = c("Filename", "Lab Type", "Status", "Upload Date", "Username"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
            )
        }
    })
    
    output$failed_files_table <- DT::renderDataTable({
        failed_files_refresh()
        df <- file_tracking_df()
        df <- df[df$status == "upload_failed", ]
        if (nrow(df) == 0) {
            DT::datatable(
                data.frame(message = "No failed uploads"),
                options = list(dom = "t", ordering = FALSE, searching = FALSE)
            )
        } else {
            df$date_added <- format(
                lubridate::with_tz(df$date_added, tzone = "America/Los_Angeles"),
                "%Y-%m-%d %I:%M %p PST"
            )
            DT::datatable(
                df[, c("filename", "lab_type", "status", "date_added", "username")],
                rownames = FALSE,
                colnames = c("Filename", "Lab Type", "Status", "Date", "Username"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
            )
        }
    })
    
    output$file_preview <- DT::renderDataTable({
        req(selected_file())
        req(file.exists(selected_file()$path))
        df <- read_csv_cached(selected_file()$path)
        DT::datatable(df, options = list(pageLength = -1, scrollX = TRUE, searching = FALSE, lengthChange = FALSE, paging = FALSE, info = FALSE))
    })
    
    upload_result <- reactiveVal(NULL)
    
    observeEvent(input$upload_to_wqx, {
        req(selected_file())
        
        upload_result(NULL)
        
        FILE_PATH <- selected_file()$path
        FILE_NAME <- selected_file()$name
        
        API_KEY <- account_info$selectedApiKey()
        USER_ID <- account_info$selectedUsername()
        CONFIG_ID <- account_info$selectedConfigId()
        
        cat("WQX UPLOAD - USER_ID:", USER_ID, "\n")
        cat("WQX UPLOAD - FILE_PATH:", FILE_PATH, "\n")
        cat("WQX UPLOAD - FILE_NAME:", FILE_NAME, "\n")
        cat("WQX UPLOAD - CONFIG_ID:", CONFIG_ID, "\n")
        
        tryCatch({
            spsComps::shinyCatch({message("Connecting to EPA's WQX server...")}, position = "bottom-full-width")
            
            cdx_session <- cdx(USER_ID, API_KEY, FILE_PATH, FILE_NAME)
            file_id <- cdx_upload(session = cdx_session)
            dataset_id <- cdx_import(
                session = cdx_session,
                file_id = file_id,
                config_id = CONFIG_ID,
                params = c("newOrExistingData", "0")
            )
            
            spsComps::shinyCatch({message("Checking import status...")}, position = "bottom-full-width")
            
            status <- NULL
            start_time <- Sys.time()
            poll_interval <- 5
            timeout_seconds <- 45
            
            while (TRUE) {
                if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) >= timeout_seconds) {
                    status <- list(StatusName = "Import Failed", error_message = "Status check timed out after 45 seconds")
                    break
                }
                
                status <- cdx_get_status(cdx_session, dataset_id)
                upload_result(status)
                
                status_display <- if (!is.null(status) && !is.null(status$StatusName)) status$StatusName else "Checking..."
                spsComps::shinyCatch({message("Status: ", status_display)}, position = "bottom-full-width")
                
                if (!is.null(status) && !is.null(status$StatusName)) {
                    status_upper <- toupper(status$StatusName)
                    if (grepl("
SUCCESS", status_upper) || grepl("FAILED", status_upper)) {
                        break
                    }
                }
                
                Sys.sleep(poll_interval)
            }
            
            upload_result(status)
            
            if (status$StatusName == "Import Failed") {
                username <- account_info$selectedUsername()
                update_file_status(FILE_NAME, "upload_failed", username)
                file_tracking_df(load_file_tracking())
            }
            
            cat("Upload result StatusName:", status$StatusName, "\n")
        }, error = function(e) {
            cat("Upload error:", conditionMessage(e), "\n")
            username <- account_info$selectedUsername()
            update_file_status(FILE_NAME, "upload_failed", username)
            file_tracking_df(load_file_tracking())
            upload_result(list(StatusName = "Import Failed", error_message = conditionMessage(e)))
        })
    })
    
    output$upload_status <- renderUI({
        result <- upload_result()
        if (is.null(result)) return(NULL)
        
        status_name <- result$StatusName
        err_msg <- result$error_message
        
        extra_content <- if (status_name != "Import Failed") {
            list(tags$br(), "Check your email or the CDX website for final confirmation.")
        } else {
            NULL
        }
        
        do.call(div, c(list(
            class = if (status_name == "Import Failed") "alert alert-danger" else "alert alert-info",
            tags$b("Status: ", status_name),
            if (!is.null(err_msg)) tags$br(),
            if (!is.null(err_msg)) tags$small(err_msg)
        ), extra_content))
    })
    
    observeEvent(upload_result(), {
        req(upload_result())
        req(selected_file())
        
        status_name <- upload_result()$StatusName
        username <- account_info$selectedUsername()
        
        if (status_name == "Import Failed") {
            update_file_status(selected_file()$name, "upload_failed", username)
        } else if (grepl("Success", status_name, ignore.case = TRUE)) {
            update_file_status(selected_file()$name, "uploaded", username)
        } else {
            update_file_status(selected_file()$name, "pending_review", username)
        }
        file_tracking_df(load_file_tracking())
    })
}
