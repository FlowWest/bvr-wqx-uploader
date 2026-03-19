tracking_db_path <- file.path(cdx_account_path, "file_tracking.db")
tracking_csv_path <- file.path(cdx_account_path, "file_tracking.csv")

get_tracking_con <- function() {
    dir.create(cdx_account_path, recursive = TRUE, showWarnings = FALSE)
    con <- dbConnect(RSQLite::SQLite(), tracking_db_path)
    dbExecute(con, "
        CREATE TABLE IF NOT EXISTS file_tracking (
            filename TEXT PRIMARY KEY,
            lab_type TEXT,
            status TEXT,
            date_added TEXT,
            upload_date TEXT,
            username TEXT
        )
    ")
    con
}

# One-time migration from CSV to SQLite
if (file.exists(tracking_csv_path) && !file.exists(tracking_db_path)) {
    old_df <- read_csv(tracking_csv_path, show_col_types = FALSE)
    if ("uploaded_by" %in% names(old_df)) {
        old_df <- old_df |> rename(username = uploaded_by)
    }
    if (!"username" %in% names(old_df)) {
        old_df$username <- NA_character_
    }
    for (col in c("date_added", "upload_date")) {
        if (col %in% names(old_df) && inherits(old_df[[col]], "POSIXct")) {
            old_df[[col]] <- format(old_df[[col]], "%Y-%m-%d %H:%M:%S")
        }
    }
    con <- get_tracking_con()
    if (nrow(old_df) > 0) {
        dbWriteTable(con, "file_tracking", old_df, append = TRUE)
    }
    dbDisconnect(con)
    file.rename(tracking_csv_path, paste0(tracking_csv_path, ".bak"))
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

load_file_tracking <- function() {
    con <- get_tracking_con()
    on.exit(dbDisconnect(con))
    df <- dbReadTable(con, "file_tracking")
    if (nrow(df) == 0) {
        return(data.frame(
            filename = character(),
            lab_type = character(),
            status = character(),
            date_added = character(),
            upload_date = character(),
            username = character(),
            stringsAsFactors = FALSE
        ))
    }
    df
}

update_file_status <- function(filename, new_status, username = NULL) {
    con <- get_tracking_con()
    on.exit(dbDisconnect(con))
    
    existing <- dbGetQuery(con, "SELECT * FROM file_tracking WHERE filename = ?", list(filename))
    
    if (nrow(existing) > 0) {
        upload_date <- existing$upload_date
        if (new_status == "uploaded") {
            upload_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        }
        uname <- if (!is.null(username)) username else existing$username
        dbExecute(con, "
            UPDATE file_tracking SET status = ?, upload_date = ?, username = ?
            WHERE filename = ?
        ", list(new_status, upload_date, uname, filename))
    } else {
        dbExecute(con, "
            INSERT INTO file_tracking (filename, lab_type, status, date_added, upload_date, username)
            VALUES (?, ?, ?, ?, ?, ?)
        ", list(
            filename,
            get_lab_type(filename),
            new_status,
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            if (new_status == "uploaded") format(Sys.time(), "%Y-%m-%d %H:%M:%S") else NA,
            if (!is.null(username)) username else NA
        ))
    }
    
    load_file_tracking()
}
