library(dplyr)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)
library(readr)
library(purrr)
# load("lookup_objects.rdata")
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


file_path <- "data-raw/bend/20241004_BV_LIMS_report.xlsm"
parse_bend_genetics_macro <- function(file_path, sheet_name){
    bend_meta_data <- read_excel(file_path, sheet = sheet_name) 
    activity_start_date <- ymd_hms("1899-12-30 00:00:00") + days(floor(as.numeric(bend_meta_data[5,5]))) 
    activity_fractional_day <- as.numeric(bend_meta_data[5,5]) %% 1
    activity_hours_part <- floor(activity_fractional_day * 24)
    activity_minutes_part <- floor((activity_fractional_day * 24 - activity_hours_part) * 60)
    
    reported_datetime <- date + hours(activity_hours_part) + minutes(activity_minutes_part)
    
    activity_date <- format(as_date(activity_start_date), "%m/%d/%Y")
    activity_time <- format(reported_datetime, "%H:%M")
    
    received_date <- ymd_hms("1899-12-30 00:00:00") + days(floor(as.numeric(bend_meta_data[2,2])))
    formatted_received_date <- format(as_date(received_date), "%m/%d/%Y")
    location <- unlist(bend_meta_data[4,5])
    Matrix <- unlist(ifelse(bend_meta_data[4,2] == "Water Grab", "Water", "SPATT"))
    bend_results <- read_excel(file_path, sheet = sheet_name, skip = 10) |>
        filter(!is.na(`Method`)) |>
        mutate("Project ID" = project_id_lookup[location],
               "Monitoring Location ID" = location,
               "Activity ID User Supplied (PARENTs)" = "",
               "Activity Type" = "Sample-Routine",
               "Activity Media Name" = "Water",
               "Activity Start Date" = activity_date,
               "Activity Start Time" = activity_time,
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
               "Analysis Start Date" = formatted_received_date,
               "Result Detection/Quantitation Limit Type" = "Practical Quantitation Limit",
               "Result Detection/Quantitation Limit Measure" = `Reporting Limit`,
               "Result Detection/Quantitation Limit Unit" = Units,
               "Result Comment" = "",
               "Activity ID (CHILD-subset)" = bend_genetics_make_activity_id(location_id = location,
                                                                             date = `Activity Start Date`,
                                                                             time = `Activity Start Time`,
                                                                             activity_type = `Activity Type`,
                                                                             equipment_name = `Sample Collection Equipment Name`,
                                                                             depth = `Activity Depth/Height Measure`)
        ) |> 
        select(-c("Method", "Analyte", "Result", "Reporting Limit", "Units", "Qualifiers", "Batch")) |> 
        relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)")
    
    }

sheet_names <- excel_sheets(file_path)
sample_sheets <- sheet_names[str_detect(sheet_names, "^Sample")]
file_path_vect <- rep(file_path, length(sample_sheets))
all_sample_data <- map2(file_path_vect, sample_sheets, parse_bend_genetics_macro)
# x <- parse_bend_genetics_macro(file_path)
bind_rows(all_sample_data) |> View()
