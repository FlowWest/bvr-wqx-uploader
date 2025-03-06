library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)
library(lubridate)
load("lookup_objects.rdata")


# calwatch - wqx submittal required?
# ------------------------------------------------------------------------------
make_activity_id <-
  function(location_id,
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
      TRUE ~ NA_character_
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

parse_alphalab <- function(filepath) {
    readxl::read_excel(filepath) |> 
        tidyr::separate(SAMPLENAME, into=c("sample1", "sample2"), sep = " ") |> # TODO this looks a little too hard-coded 
        dplyr::mutate(SAMPLENAME = ifelse(sample1 %in% names(project_id_lookup), sample1, sample2)) |>
        dplyr::select(-sample1, -sample2) |> 
        dplyr::relocate(SAMPLENAME, .before = LABSAMPID) |> 
        mutate(SAMPLENAME = ifelse(SAMPLENAME == "BVSWDI", "BVSWD1", SAMPLENAME),
               SAMPLENAME = ifelse(SAMPLENAME == "BVRTCI", "BVRTC1", SAMPLENAME),
               SAMPLENAME = ifelse(SAMPLENAME == "BVCLI", "BVCL1", SAMPLENAME))
}
alpha_lab_to_wqx <- function(data) {
    data |> 
        mutate(
            "Project ID" = project_id_lookup[SAMPLENAME],
            "Monitoring Location ID" = SAMPLENAME,
            "Activity ID User Supplied (PARENTs)" = "",
            "Activity Type" = "Sample-Routine",
            "Activity Media Name" = MATRIX,
            "Activity Start Date" = case_when(
                class(SAMPDATE)[1] == "character" ~ format(mdy_hms(SAMPDATE), "%m/%d/%Y"),
                class(SAMPDATE)[1] == "POSIXct" ~ format(`SAMPDATE`,"%m/%d/%Y"),
                TRUE ~ ""),
            "Activity Start Time" = case_when(
                class(SAMPDATE)[1] == "character" ~ format(mdy_hms(SAMPDATE), "%H:%M"),
                class(SAMPDATE)[1] == "POSIXct" ~ format(`SAMPDATE`,"%H:%M"),
                TRUE ~ ""),
            "Activity Start Time Zone" = "PST",
            "Activity Depth/Height Measure" = "0.152",
            "Activity Depth/Height Unit" = "m",
            "Sample Collection Method ID" = "BVR SWQAPP",
            "Sample Collection Method Context" = "CA_BVR",
            "Sample Collection Equipment Name" = "Water Bottle",
            "Sample Collection Equipment Comment" = "",
            "Characteristic Name" = ifelse(
                ANALYTE %in% names(characteristic_lookup), characteristic_lookup[ANALYTE], ANALYTE),
            "Characteristic Name User Supplied" = "",
            "Method Speciation" = ifelse(
                ANALYTE %in% names(method_speciation_lookup), method_speciation_lookup[ANALYTE], ""),
            "Result Detection Condition" = case_when(
                Result == "ND" ~ "Not Detected",
                Result == "Absent" ~ "Not Present",
                Result == "Present" ~ "Detected Not Quantified",
                TRUE ~ ""
            ),
            "Result Value" = ifelse(
                Result == "Absent" |
                    Result == "ND" | Result == "Present",
                "",
                Result
            ),
            "Result Unit" = case_when(UNITS == "." ~ "",
                                      Result == "Absent" ~ "",
                                      Result == "Present" ~ "",
                                      Result == "ND" ~ "", 
                                      TRUE ~ UNITS),
            "Result Measure Qualifier" = "",
            "Result Sample Fraction" = "Total",
            "Result Status ID" = "Final",
            "ResultTemperatureBasis" = "",
            "Statistical Base Code" = "",
            "ResultTimeBasis" = "",
            "Result Value Type" = ifelse(is.na(Result), "", "Actual"),
            "Result Analytical Method ID" = ifelse(
                is.na(METHODNAME), "", method_id_lookup[METHODNAME]),
            "Activity ID (CHILD-subset)" = alpha_lab_make_activity_id(
                location_id = `Monitoring Location ID`,
                date = `Activity Start Date`,
                time = `Activity Start Time`,
                activity_type = `Activity Type`,
                equipment_name = `Sample Collection Equipment Name`,
                depth = `Activity Depth/Height Measure`
            ),
            "Result Analytical Method Context" = method_context_lookup[METHODNAME],
            "Analysis Start Date" = case_when(
                class(ANADATE)[1] == "character" ~ format(mdy_hms(ANADATE), "%m/%d/%Y"),
                class(ANADATE)[1] == "POSIXct" ~ format(`ANADATE`,"%m/%d/%Y"),
                TRUE ~ ""),
            "Result Detection/Quantitation Limit Type" = ifelse(is.na(DL), "", "Lower Reporting Limit"),
            "Result Detection/Quantitation Limit Measure" = ifelse(is.na(DL), "", DL),
            "Result Detection/Quantitation Limit Unit" = ifelse(UNITS == ".", "", UNITS),
            "Result Comment" = ""
            
        ) %>%
        select(-c(0:48)) %>%
        relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)") |> 
        mutate(
            "Result Analytical Method Context" = ifelse(
                is.na(`Result Analytical Method ID`),
                "",
                `Result Analytical Method Context`
            ),
            "Result Unit" = ifelse(
                is.na(`Result Value`),
                "",
                `Result Unit`
            )
        ) |>
        relocate("Result Analytical Method Context", .before = "Analysis Start Date") |> 
        select("Project ID":"Result Comment")
}

# ------------------------------------------------------------------------------

# raw_alpha_lab <-
#   list.files("data-raw/alpha-lab",
#              full.names = T) %>% 
#   map_df(~read_excel(.))

# ------------------------------------------------------------

file_path <-
  "data-raw/alpha-lab/07.12.2024 Tule Boat BVCL6.xls"
# file_path <- "data-raw/alpha-lab/22G2998 FINAL EXCEL 09 Aug 22 1018.xls"
# 
raw_alpha_lab <- read_excel(file_path) 
x <- parse_alphalab(file_path)
alpha_lab_to_wqx(x)


for (row in 1:nrow(raw_alpha_lab)) {
  for (n in 1:length(strsplit(raw_alpha_lab$SAMPLENAME, " ")[[row]])) {
    if (strsplit(raw_alpha_lab$SAMPLENAME, " ")[[row]][n] %in% locations) {
      raw_alpha_lab$SAMPLENAME[row] = strsplit(raw_alpha_lab$SAMPLENAME, " ")[[row]][n]
    }
  }
    
}

if (class(raw_alpha_lab$ANADATE)[1] == "character"){
    print("Character")
} else if (class(raw_alpha_lab$ANADATE)[1] == "POSIXct"){
    print("Date")
}
test <- raw_alpha_lab |> 
    mutate("SAMPDATE" = format(`SAMPDATE`,"%m/%d/%Y"))

  
  # (strsplit(raw_alpha_lab$SAMPLENAME, " ")[[row]][1] %in% locations | strsplit(raw_alpha_lab$SAMPLENAME, " ")[[row]][2] %in% locations){
  

  # }
  # for (word in strsplit(raw_alpha_lab$SAMPLENAME, " ")[[row]]){
    # print(word)
  # }
# }

locations <- c("HSP", "BVSWD1", "RSTCC")
# appending_vector <- c()
# 
# for (x in strsplit(raw_alpha_lab$SAMPLENAME, " ")){
#   for (word in x){
#     if(word %in% locations){
#       appending_vector <- append(appending_vector, word)
#     }else{
#       appending_vector <- append(appending_vector, NA)
#     }
#   }
# }


clean_alpha_lab <- raw_alpha_lab %>%
  # clean_names() %>%
  # Are we using calwatch as project ID?
  mutate(
    "Project ID" = "SW",
    # No location ID or sampleIDin these alpha lab data
    "Monitoring Location ID" = SAMPLENAME,
    "Activity ID User Supplied (PARENTs)" = NA,
    "Activity Type" = "Sample-Routine",
    "Activity Media Name" = MATRIX,
    "Activity Start Date" = format(mdy_hms(SAMPDATE), "%m/%d/%Y"),
    "Activity Start Time" = format(mdy_hms(SAMPDATE), "%H:%M"),
    "Activity Start Time Zone" = "PST",
    # not depth or height
    "Activity Depth/Height Measure" = "0.152",
    "Activity Depth/Height Unit" = "m",
    "Sample Collection Method ID" = "BVR SWQAPP",
    "Sample Collection Method Context" = "CA_BVR",
    "Sample Collection Equipment Name" = "Water Bottle",
    "Sample Collection Equipment Comment" = NA,
    "Characteristic Name" = case_when(
      ANALYTE == "E. Coli" ~ "Escherichia coli",
      ANALYTE == "Oil & Grease (HEM)" ~ "Oil and Grease",
      ANALYTE == "Phosphorus, total" ~ "Phosphorus",
      ANALYTE == "Total Organic Carbon" ~ "Organic carbon",
      TRUE ~ ANALYTE),
    "Characteristic Name User Supplied" = NA,
    "Method Speciation" = case_when(
      ANALYTE == "Nitrate as N" ~ "as N",
      ANALYTE == "Phosphorus, total" ~ "as P",
      ANALYTE == "Nitrate + Nitrite as N" ~ "as N",
      TRUE ~ NA_character_),
    "Result Detection Condition" = case_when(
      Result == "ND" ~ "Not Reported",
      Result == "Absent" ~ "Not Present",
      Result == "Present" ~ "Detected Not Quantified",
      TRUE ~ NA_character_),
    "Result Value" = ifelse(Result == "Absent" | Result == "ND" | Result == "Present", NA_character_, Result),
    "Result Unit" = ifelse(UNITS == ".", NA_character_, UNITS),
    "Result Measure Qualifier" = NA,
    "Result Sample Fraction" = "Total",
    "Result Status ID" = "Final",
    "ResultTemperatureBasis" = NA,
    "Statistical Base Code" = NA,
    "ResultTimeBasis" = NA,
    "Result Value Type" = ifelse(is.na(Result), NA_character_, "Actual"),
    "Result Analytical Method ID" = ifelse(is.na(METHODNAME), NA_character_, method_id_lookup[METHODNAME]),
    "Activity ID (CHILD-subset)" = make_activity_id(
      location_id = `Monitoring Location ID`,
      date = `Activity Start Date`,
      time = `Activity Start Time`,
      activity_type = `Activity Type`,
      equipment_name = `Sample Collection Equipment Name`,
      depth = `Activity Depth/Height Measure`
    ),
    # Casnumber? Context APHA? 
    "Result Analytical Method Context" = method_context_lookup[METHODNAME],
    "Analysis Start Date" = format(mdy_hms(ANADATE), "%m/%d/%Y"),
    "Result Detection/Quantitation Limit Type" = ifelse(DL == "NA", NA_character_, "Method Detection Level"),
    "Result Detection/Quantitation Limit Measure" = ifelse(DL == "NA", NA_character_, DL),
    "Result Detection/Quantitation Limit Unit" = ifelse(UNITS == ".", NA_character_, UNITS),
    "Result Comment" = NA
    
  ) %>%
  select(-c(0:48)) %>% 
  relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)")

clean_alpha_lab <- clean_alpha_lab %>% 
  mutate("Result Analytical Method Context" = ifelse(is.na(clean_alpha_lab$`Result Analytical Method ID`), NA_character_, clean_alpha_lab$`Result Analytical Method Context`),
         "Result Unit" = ifelse(is.na(clean_alpha_lab$`Result Value`), NA_character_, clean_alpha_lab$`Result Unit`)) %>% 
  relocate("Result Analytical Method Context", .before = "Analysis Start Date")

write_csv(clean_alpha_lab, "data/alpha_lab_wqx.csv", na = "")

# **NA and Unknown Values**
#   
#   * `r round(sum(is.na(combined_lab_data$`Activity Depth/Height Measure`))/nrow(combined_lab_data), 3) * 100` % of values in the `Activity Depth/Height Measure` column are NA.
# 
# * `r round(sum(is.na(combined_lab_data$`Result Value`))/nrow(combined_lab_data), 3) * 100` % of values in the `Result Value` column are NA.
# 
# * `r round(sum(is.na(combined_lab_data$`Result Detection/Quantitation Limit Measure`))/nrow(combined_lab_data), 3) * 100` % of values in the `Result Detection/Quantitation Limit Measure` column are NA.

