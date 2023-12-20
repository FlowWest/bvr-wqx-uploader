parse_bend_genetics <- function(file_path) {
    bend_raw <- read_csv(file_path, skip = 7) |>
        select(-starts_with("..."))
    
    analytical_report_for_samples <- bend_raw[1:which(is.na(bend_raw$`Sample ID`))[1] - 1,]
    
    bend_raw_na <- bend_raw |>
        filter(!if_all(everything(), is.na))
    
    sample_results <- bend_raw_na[(which(is.na(bend_raw_na$`Sample ID`))[1:4][4] + 3):(which(is.na(bend_raw_na$`Sample ID`))[5] - 1),] |>
        rename(
            Method = Location,
            Target = `Date Collected`,
            Result = `Date Received`,
            `Quantitation Limit` = Matrix,
            Units = Preserved,
            Notes = BG_ID
        ) |> 
        mutate_if(is.character, utf8::utf8_encode) |> 
        mutate(
            Units = ifelse(Units == "\\xb5g/L", "ug/L", Units)
        )
    
    bend_full_df <- left_join(analytical_report_for_samples, sample_results) |> 
        filter(!is.na(Target))
    
    return(bend_full_df)
}
# parse_bend_genetics <- function(filepath) {
#     raw_bend_data <- read_csv(filepath, skip = 7)  
#     raw_bend_data <- raw_bend_data |> select(-starts_with("..."))
#     raw_bend_data <- raw_bend_data[!is.na(raw_bend_data$Location), ]
#     
#     # Find the index of the first occurrence of "Bend Genetics, LLC" in the "Location" column
#     df_cutoff_1 <- min(which(raw_bend_data$Location == "Bend Genetics, LLC")) - 1
#     
#     # Find the index of the second occurrence of "Bend Genetics, LLC" in the "Location" column
#     df_cutoff_2 <- max(which(raw_bend_data$Location == "Bend Genetics, LLC")) - 1
#     
#     # Create the analytical_report_for_samples dataframe
#     analytical_report_for_samples <- raw_bend_data[1:df_cutoff_1, ]
#     
#     # Find the index of the row where "Sample ID" appears in the "Sample ID" column
#     sample_id_row_index <- min(which(raw_bend_data$`Sample ID` == "Sample ID"))
#     
#     # Create the sample_results dataframe
#     sample_results <- raw_bend_data[(sample_id_row_index + 1):df_cutoff_2, ]  
#     
#     sample_results <- sample_results|>
#         rename(
#             Method = Location,
#             Target = `Date Collected`,
#             Result = `Date Received`,
#             `Quantitation Limit` = Matrix,
#             Units = Preserved,
#             Notes = BG_ID
#         ) |> 
#         mutate_if(is.character, utf8::utf8_encode) |> 
#         mutate(
#             Units = ifelse(Units== "\\xb5g/L", "ug/L", Units),
#             Result = readr::parse_number(Result) 
#         )
#     
#     bend_full_df <-
#         left_join(analytical_report_for_samples, sample_results) |> 
#         filter(!is.na(Target))
# 
#              }

bend_genetics_make_activity_id <-
    function(location_id,
             date,
             activity_type,
             equipment_name,
             depth = NULL,
             time = NULL,
             equipment_comment = NULL) {
        YYYYMMDD <- gsub('/', '', date)
        activity <- ifelse(activity_type == "Sample-Routine", "SR", "FM")
        equipment <- ifelse(equipment_name == "Probe/Sensor", "PS", "")
        hhmm <- gsub(':', '', time)
        equipment_comment <- case_when(
            equipment_comment == "Hydrolab Surveyor DS5 Multiprobe" ~ "Hydro",
            equipment_comment == "AlgaeChek Ultra Fluorometer" ~ "Algae",
            TRUE ~ NA_character_
        )
        paste(location_id,
              YYYYMMDD,
              hhmm,
              activity,
              equipment,
              depth,
              equipment_comment,
              sep = ":")
    }

bend_genetics_to_wqx <- function(data) {
    data |>        
        mutate("Project ID" = project_id_lookup[Location],
               "Monitoring Location ID" = Location,
               "Activity ID User Supplied (PARENTs)" = "",
               "Activity Type" = "Sample-Routine",
               "Activity Media Name" = "Water",
               # use the lubridate package function mdy_hm()to format date in m/d/y
               "Activity Start Date" = format(mdy_hm(`Date Collected`), "%m/%d/%Y"),
               # use the lubridate package function mdy_hm() to formate time in HH:MM
               "Activity Start Time" = format(mdy_hm(`Date Collected`), "%H:%M"),
               "Activity Start Time Zone" = "PST",
               # Need activity depth/height, unit
               "Activity Depth/Height Measure" = "0.151",
               "Activity Depth/Height Unit" = "m",
               # Confirm Sample Collection method id is BVR SWQAPP
               "Sample Collection Method ID" = "BVR SWQAPP",
               "Sample Collection Method Context" = "CA_BVR",
               # Confirm Equipment for bend is Water Bottle
               "Sample Collection Equipment Name" = case_when(`Matrix` == "SPATT" ~ "SPATT Bags",
                                                              `Matrix` == "Water" ~ "Water Bottle",
                                                              .default = ""),
               "Sample Collection Equipment Comment" = "",
               "Characteristic Name" = ifelse(Target == "Microcystin/Nod.", "Microcystin/nodularin genes mcyE/ndaF", Target),
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
                   `Method` == "ELISA" ~ abraxis_id_lookup[Target],
                   .default = method_id_lookup[Method]),
               # method_context_lookup is in the lookup table. It points the method to their method context.
               "Result Analytical Method Context" = method_context_lookup[Method],
               "Analysis Start Date" = format(mdy_hm(`Date Received`), "%m/%d/%Y"),
               "Result Detection/Quantitation Limit Type" = "Practical Quantitation Limit",
               "Result Detection/Quantitation Limit Measure" = `Quantitation Limit`,
               "Result Detection/Quantitation Limit Unit" = Units,
               "Result Comment" = ifelse(is.na(Notes), "", Notes),
               "Activity ID (CHILD-subset)" = bend_genetics_make_activity_id(location_id = Location,
                                                               date = `Activity Start Date`,
                                                               time = `Activity Start Time`,
                                                               activity_type = `Activity Type`,
                                                               equipment_name = `Sample Collection Equipment Name`,
                                                               depth = `Activity Depth/Height Measure`)
        ) |>
        relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)") |>
        select(-c(0:13))
}

clean_bend_wqx <- function(data) {
    data |> 
        mutate(
            "Result Unit" = ifelse(
                data$`Result Unit` == "µg/L",
                "ug/L",
                data$`Result Unit`
            ),
            "Result Detection/Quantitation Limit Unit" = ifelse(
                data$`Result Detection/Quantitation Limit Unit` == "µg/L",
                "ug/L",
                data$`Result Detection/Quantitation Limit Unit`
            )
        )
}
