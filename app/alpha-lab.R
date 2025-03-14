convert_to_datetime <- function(string_date_time_column) {
    numeric_date_time_column <- as.numeric(string_date_time_column)  # Ensure it's numeric
    
    if (any(!is.na(numeric_date_time_column))) {
        date_time_column <- as.POSIXct(numeric_date_time_column * (60*60*24), origin = "1899-12-30", tz = "UTC")
    }else{
        date_time_column <- mdy_hms(string_date_time_column)
    }
    
    return(date_time_column)
}

parse_alphalab <- function(filepath) {
    col_names <- names(read_excel(filepath, n_max = 0))
    samp_date_index <- which(col_names == "SAMPDATE")
    ana_date_index <- which(col_names == "ANADATE")
    col_types <- rep("guess", length(col_names))
    col_types[samp_date_index] <- "text"
    col_types[ana_date_index] <- "text"
    # col_types
    file <- read_excel(filepath, col_types = col_types)
    file[["SAMPDATE"]] <- convert_to_datetime(file[["SAMPDATE"]])
    file[["ANADATE"]] <- convert_to_datetime(file[["ANADATE"]]) 
    names(file) <- toupper(names(file))
    
    # file <- readxl::read_excel(filepath, , col_types = col_types)
    file <- file |>
        tidyr::separate(SAMPLENAME,
                        into = c("sample1", "sample2"),
                        sep = " ") |> # TODO this looks a little too hard-coded
        dplyr::mutate(SAMPLENAME = ifelse(sample1 %in% names(project_id_lookup), sample1, sample2)) |>
        dplyr::select(-sample1, -sample2) |>
        dplyr::relocate(SAMPLENAME, .before = LABSAMPID) |>
        mutate(
            SAMPLENAME = ifelse(SAMPLENAME == "BVSWDI", "BVSWD1", SAMPLENAME),
            SAMPLENAME = ifelse(SAMPLENAME == "BVRTCI", "BVRTC1", SAMPLENAME),
            SAMPLENAME = ifelse(SAMPLENAME == "BVCLI", "BVCL1", SAMPLENAME)
        ) 
}

alpha_lab_make_activity_id <- function(location_id,
                                       date,
                                       activity_type,
                                       equipment_name,
                                       depth = NULL,
                                       time = NULL,
                                       equipment_comment = NULL) {
    YYYYMMDD <- gsub('/', '', date)
    activity <- ifelse(activity_type == "Sample-Routine", "SR", "FM")
    equipment <- case_when(
        equipment_name == "Probe/Sensor" ~ "PS",
        equipment_name == "Water Bottle" ~ "WB",
        TRUE ~ ""
    )
    hhmm <- gsub(':', '', time)
    equipment_comment <- case_when(
        equipment_comment == "Hydrolab Surveyor DS5 Multiprobe" ~ "Hydro",
        equipment_comment == "AlgaeChek Ultra Fluorometer" ~ "Algae",
        TRUE ~ ""
    )
    depth <- ifelse(is.na(depth), "", depth)
    paste(location_id,
          YYYYMMDD,
          hhmm,
          activity,
          equipment,
          depth,
          equipment_comment,
          sep = ":")
}

alpha_lab_format_for_range_validation <- function(data) {
    data |>
        select(PROJECT, ANALYTE, RESULT) |>
        pivot_wider(names_from = "ANALYTE", values_from = "RESULT")
}

alpha_lab_to_wqx <- function(data) {
    # date_columns <- c("SAMPDATE", "ANADATE")
    # for (col in date_columns) {
    #     if (is.character(data[[col]])) {
    #         data[[col]] <- mdy_hms(data[[col]])
    #     }
    # }
    
    data |>
        mutate(
            "Project ID" = project_id_lookup[SAMPLENAME],
            "Monitoring Location ID" = SAMPLENAME,
            "Activity ID User Supplied (PARENTs)" = "",
            "Activity Type" = "Sample-Routine",
            "Activity Media Name" = MATRIX,
            "Activity Start Date" = format(SAMPDATE, "%m/%d/%Y"),
            "Activity Start Time" = format(SAMPDATE, "%H:%S"),
            "Activity Start Time Zone" = "PST",
            "Activity Depth/Height Measure" = "0.152",
            "Activity Depth/Height Unit" = "m",
            "Sample Collection Method ID" = "BVR SWQAPP",
            "Sample Collection Method Context" = "CA_BVR",
            "Sample Collection Equipment Name" = "Water Bottle",
            "Sample Collection Equipment Comment" = "",
            "Characteristic Name" = ifelse(
                ANALYTE %in% names(characteristic_lookup),
                characteristic_lookup[ANALYTE],
                ANALYTE
            ),
            "Characteristic Name User Supplied" = "",
            "Method Speciation" = ifelse(
                ANALYTE %in% names(method_speciation_lookup),
                method_speciation_lookup[ANALYTE],
                ""
            ),
            "Result Detection Condition" = case_when(
                RESULT == "ND" ~ "Not Detected",
                RESULT == "Absent" ~ "Not Present",
                RESULT == "Present" ~ "Detected Not Quantified",
                TRUE ~ ""
            ),
            "Result Value" = ifelse(
                RESULT == "Absent" |
                    RESULT == "ND" | RESULT == "Present",
                "",
                RESULT
            ),
            "Result Unit" = case_when(
                UNITS == "." ~ "",
                RESULT == "Absent" ~ "",
                RESULT == "Present" ~ "",
                RESULT == "ND" ~ "",
                TRUE ~ UNITS
            ),
            "Result Measure Qualifier" = "",
            "Result Sample Fraction" = "Total",
            "Result Status ID" = "Final",
            "ResultTemperatureBasis" = "",
            "Statistical Base Code" = "",
            "ResultTimeBasis" = "",
            "Result Value Type" = ifelse(is.na(RESULT), "", "Actual"),
            "Result Analytical Method ID" = ifelse(is.na(METHODNAME), "", method_id_lookup[METHODNAME]),
            "Activity ID (CHILD-subset)" = alpha_lab_make_activity_id(
                location_id = `Monitoring Location ID`,
                date = `Activity Start Date`,
                time = `Activity Start Time`,
                activity_type = `Activity Type`,
                equipment_name = `Sample Collection Equipment Name`,
                depth = `Activity Depth/Height Measure`
            ),
            "Result Analytical Method Context" = method_context_lookup[METHODNAME],
            "Analysis Start Date" = format(ANADATE, "%m/%d/%Y"),
            "Result Detection/Quantitation Limit Type" = ifelse(is.na(DL), "", "Lower Reporting Limit"),
            "Result Detection/Quantitation Limit Measure" = ifelse(is.na(DL), "", DL),
            "Result Detection/Quantitation Limit Unit" = ifelse(UNITS == ".", "", UNITS),
            "Result Comment" = ""
            
        ) %>%
        select(-c(0:(which(
            names(.) == "Project ID"
        ) - 1))) %>%
        relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)") |>
        mutate(
            "Result Analytical Method Context" = ifelse(
                is.na(`Result Analytical Method ID`),
                "",
                `Result Analytical Method Context`
            ),
            "Result Unit" = ifelse(is.na(`Result Value`), "", `Result Unit`)
        ) |>
        relocate("Result Analytical Method Context", .before = "Analysis Start Date") |>
        select("Project ID":"Result Comment")
}