make_activity_id <- function(location_id, date, activity_type, equipment_name, depth = NULL, time = NULL) {
    YYYYMMDD <- gsub('/', '', date)
    activity <- ifelse(activity_type == "Sample-Routine", "SR", "FM")
    equipment <- case_when(
        equipment_name == "Probe/Sensor" ~ "PS",
        equipment_name == "Water Bottle" ~ "WB",
        TRUE ~ NA_character_)
    hhmm <- gsub(':', '', time)
    equipment_comment <- case_when(
        equipment_name == "Hydrolab Surveyor DS5 Multiprobe" ~ "Hydro",
        equipment_name == "AlgaeChek Ultra Fluorometer" ~ "Algae", 
        TRUE ~ "")
    depth <- ifelse(is.na(depth), "", depth)
    paste(location_id, YYYYMMDD, hhmm,activity, equipment, depth, equipment_comment, sep = ":")
}


parse_bend_genetics_macro <- function(file_path, sheet_name){
    # file_path <- "data-raw/bend/20241004_BV_LIMS_report.xlsm"
    # sheet_name <- "Sample1"
    left_metadata_raw <- read_excel(file_path, sheet = sheet_name, range = "A3:B8", col_names = c("key", "value"))
    right_metadata_raw <- read_excel(file_path, sheet = sheet_name, range = "D3:E8", col_names = c("key", "value"))
    
    #process metadata
    left_metadata <- map2_dfc(left_metadata_raw$key, left_metadata_raw$value, function(x, y) {
        if (!is.na(x)) {
            
            if (x == "Received:") {
                as.Date(as.numeric(y) - 2, origin = "1900-01-01")
            } else if (x == "Time:") {
                format(as.POSIXct(as.numeric(y) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
            } else if (x %in% c("Matrix:", "BG ID:")) {
                y
            } else if (x == "Reported:") {
                as.POSIXct((as.numeric(y) - 2) * 86400, origin = "1900-01-01")
            }
        }
    })
    
    right_metadata <- map2_dfc(right_metadata_raw$key, right_metadata_raw$value, function(x, y) {
        if (!is.na(x)) {
            
            if (x == "Customer:") {
                y
            } else if (x == "Project:") {
                y
            } else if (x == "Location:"){
                y
            } else if (x == "Collected:") {
                as.POSIXct((as.numeric(y) - 2) * 86400, origin = "1900-01-01")
            } else if (x == "Sample ID:") {
                y
            }
        }
    })
    
    left_col_names <- stringr::str_replace(left_metadata_raw$key[!is.na(left_metadata_raw$key)], ":", "")
    colnames(left_metadata) <- left_col_names
    right_col_names <- stringr::str_replace(right_metadata_raw$key[!is.na(right_metadata_raw$key)], ":", "")
    colnames(right_metadata) <- right_col_names
    
    full_metadata <- cross_join(right_metadata, left_metadata) 
    full_metadata_formatted <- full_metadata |> 
        mutate(
            Matrix = ifelse(Matrix == "Water Grab", "Water", "SPATT"),
            activity_start_date = format(as_date(Collected), "%m/%d/%Y"),
            activity_start_time = format(Collected, "%H:%M"))
    
    bend_results <- read_excel(file_path, sheet = sheet_name, skip = 10) |>
        filter(!is.na(`Method`)) |>
        mutate(
            "Project ID" = project_id_lookup[full_metadata_formatted$Location],
            "Monitoring Location ID" = full_metadata_formatted$Location,
            "Activity Start Date" = full_metadata_formatted$activity_start_date,
            "Activity Start Time" = full_metadata_formatted$activity_start_time,
            "Sample Collection Equipment Name" = case_when(full_metadata_formatted$Matrix == "SPATT" ~ "SPATT Bags",
                                                           full_metadata_formatted$Matrix == "Water" ~ "Water Bottle",
                                                           .default = ""),
            "Analysis Start Date" = format(as_date(full_metadata_formatted$Received),"%m/%d/%Y")
        ) |>
        select(-c("Qualifiers", "Batch"))
    return(bend_results)
}


bend_genetics_to_wqx <- function(data) {
    bend_wqx_results <- data |>
        mutate(
            # "Project ID" = project_id_lookup[location],
            # "Monitoring Location ID" = location,
            "Activity ID User Supplied (PARENTs)" = "",
            "Activity Type" = "Sample-Routine",
            "Activity Media Name" = "Water",
            # "Activity Start Date" = activity_date,
            # "Activity Start Time" = activity_time,
            "Activity Start Time Zone" = "PST",
            # Need activity depth/height, unit
            "Activity Depth/Height Measure" = "0.151",
            "Activity Depth/Height Unit" = "m",
            # Confirm Sample Collection method id is BVR SWQAPP
            "Sample Collection Method ID" = "BVR SWQAPP",
            "Sample Collection Method Context" = "CA_BVR",
            # Confirm Equipment for bend is Water Bottle
            # "Sample Collection Equipment Name" = case_when(`Matrix` == "SPATT" ~ "SPATT Bags",
            # `Matrix` == "Water" ~ "Water Bottle",
            # .default = ""),
            "Sample Collection Equipment Comment" = "",
            "Characteristic Name" = ifelse(Analyte == "Microcystin/Nod.", "Microcystin/nodularin genes mcyE/ndaF", Analyte),
            "Characteristic Name User Supplied" = "",
            "Method Speciation" = "",
            "Result Detection Condition" = ifelse(Result == "ND", "Not Detected", ""),
            "Result Value" = ifelse(Result == "ND", "", gsub(",", "", Result)),
            "Result Unit" = ifelse(Result == "ND", "", Units),
            "Result Measure Qualifier" = "",
            "Result Sample Fraction" = "Total",
            "Result Status ID" = "Final",
            "ResultTemperatureBasis" = "",
            "Statistical Base Code" = "",
            "ResultTimeBasis" = "",
            "Result Value Type" = "Actual",
            # method_lookup is in the lookup table. It points the methods to their IDs.
            "Result Analytical Method ID" = case_when(
                `Method` == "ELISA" ~ abraxis_id_lookup[Analyte],
                .default = method_id_lookup[Method]),
            # method_context_lookup is in the lookup table. It points the method to their method context.
            "Result Analytical Method Context" = method_context_lookup[Method],
            # "Analysis Start Date" = formatted_received_date,
            "Result Detection/Quantitation Limit Type" = "Practical Quantitation Limit",
            "Result Detection/Quantitation Limit Measure" = `Reporting Limit`,
            "Result Detection/Quantitation Limit Unit" = Units,
            "Result Comment" = "",
            "Activity ID (CHILD-subset)" = make_activity_id(location_id = `Monitoring Location ID`,
                                                                          date = `Activity Start Date`,
                                                                          time = `Activity Start Time`,
                                                                          activity_type = `Activity Type`,
                                                                          equipment_name = `Sample Collection Equipment Name`,
                                                                          depth = `Activity Depth/Height Measure`)
        ) |> 
        select(-c("Method", "Analyte", "Result", "Reporting Limit", "Units"))
        # relocate("Activity Start Date", .after = "Activity Media Name") |> 
        # relocate("Activity Start Time", .after = "Activity Start Date") |> 
        # relocate("Analysis Start Date", .before = "Result Analytical Method Context") |> 
    bend_wqx_results <- bend_wqx_results[, c(
        "Project ID", 
        "Monitoring Location ID", 
        "Activity ID (CHILD-subset)", 
        "Activity ID User Supplied (PARENTs)",
        "Activity Type", 
         "Activity Media Name", 
        "Activity Start Date", 
        "Activity Start Time", 
        "Activity Start Time Zone", 
        "Activity Depth/Height Measure", 
        "Activity Depth/Height Unit", 
        "Sample Collection Method ID", 
        "Sample Collection Method Context",	
        "Sample Collection Equipment Name",	
        "Sample Collection Equipment Comment",	
        "Characteristic Name",	
        "Characteristic Name User Supplied",
        "Method Speciation",	
        "Result Detection Condition",
        "Result Value",	
        "Result Unit",	
        "Result Measure Qualifier",	
        "Result Sample Fraction",	
        "Result Status ID",	
        "ResultTemperatureBasis",	
        "Statistical Base Code",	
        "ResultTimeBasis",	
        "Result Value Type",	
        "Result Analytical Method ID",	
        "Result Analytical Method Context",	
        "Analysis Start Date",	
        "Result Detection/Quantitation Limit Type",	
        "Result Detection/Quantitation Limit Measure",	
        "Result Detection/Quantitation Limit Unit",
        "Result Comment"
    )]
    return(bend_wqx_results)
}
clean_bend_wqx <- function(data) {
    data |> 
        mutate(
            'Result Unit' = ifelse(
                data$"Result Unit" == "μg/L",
                "ug/L",
                data$"Result Unit"
            ),
            'Result Detection/Quantitation Limit Unit' = ifelse(
                data$'Result Detection/Quantitation Limit Unit' == "μg/L",
                "ug/L",
                data$'Result Detection/Quantitation Limit Unit'
            )
        )
}
