library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(hms)
load("data-raw/lookup_objects.rdata")

# read data from csv into environment
# raw_data <- read_csv('data-raw/hydro-lab/LPTNT.csv', skip = 5, col_types = "c")
# View(raw_data)
# glimpse(raw_data)

raw_file_list <-
  list.files("data-raw/hydro-lab",
             full.names = T) 


# |> 
#   map_df(~read_csv(.,skip = 5, col_types = "c"
#                    ))

# ------------------------------------------------------------------------------
#Note: two locations M1 and BVCL6 are used in two projects (BVSHORE) 

# raw_header <- 
#   list.files("data-raw/hydro-lab",
#              full.names = T) |> 
#   map_df(~read_csv(.,col_names = FALSE, n_max = 1
#   ))
# 
# raw_header <- c(raw_header$X1)
# 
# regex_pattern <- "\\w+$"
# location_id <- unlist(
#   str_extract_all(raw_header,
#     regex_pattern))
# ------------------------------------------------------------------------------
datalist = list()

for (file in raw_file_list){
  raw_header <- read_csv(file, col_names = FALSE, n_max = 1)
  raw_header <- c(raw_header$X1)
  regex_pattern <- "\\w+$"
  location_id <- unlist(
    str_extract_all(raw_header,
                    regex_pattern))
  print(location_id)
  raw_data <- read_csv(file, skip = 5, col_types = "c") |> 
    mutate("location_id" = location_id)
  datalist[[file]] <- raw_data
}
all_raw_data <- do.call(rbind, datalist)

# -----------------------------------------------------------------------------
make_activity_id <- function(location_id, date, activity_type, equipment_name, depth = NULL, time = NULL, equipment_comment = NULL) {
  YYYYMMDD <- gsub('/', '', date)
  activity <- ifelse(activity_type == "Sample-Routine", "SR", "FM")
  equipment <- ifelse(equipment_name == "Probe/Sensor", "PS", NA)
  hhmm <- gsub(':', '', time)
  equipment_comment <- case_when(
    equipment_comment == "Hydrolab Surveyor DS5 Multiprobe" ~ "Hydro",
    equipment_comment == "AlgaeChek Ultra Fluorometer" ~ "Algae", 
    TRUE ~ NA_character_)
  paste(location_id, YYYYMMDD, hhmm,activity, equipment, depth, equipment_comment, sep = ":")
}
# ------------------------------------------------------------------------------
hydrolab_formatted_for_wqx <- all_raw_data |> 
  select(-c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)) |> 
  clean_names() |>
  filter(str_count(date, "\\d+") > 2) |> 
  select(-c("ibv_svr4", "chl", "pcy")) |>
  rename("Temperature, water" = "temp",
         "Specific conductance" = "sp_cond",
         "Resistivity" = "res",
         "Salinity" = "sal",
         "Total dissolved solids" = "tds",
         "Dissolved oxygen saturation" = "do_percent",
         "Dissolved oxygen (DO)" = "do",
         "pH" = "p_h",
         "Turbidity" = "turb",
         "Monitoring Location ID" = location_id) |>
    pivot_longer(!c(date, time, depth10, `Monitoring Location ID`), names_to = "Characteristic Name", "values_to" = "Result Value") |> 
  mutate("Project ID" = project_id_lookup[`Monitoring Location ID`],
         # "Monitoring Location ID" = location_id,
         # "Activity ID (CHILD-subset)" = make_activity_id(location_id, date, activity_type, equipment_name, time = NULL), 
         "Activity ID User Supplied(PARENTs)" = "",
         "Activity Type" = "Field Msr/Obs",
         "Activity Media Name" = "Water",
         "Activity Start Date" = format(mdy(date), "%m/%d/%Y"),
         "Activity Start Time" = format(parse_date_time(time, c('HMS', 'HM')), "%H:%M"),
         "Activity Start Time Zone" = "PST",
         "Activity Depth/Height Measure" = as.numeric(depth10),
         "Activity Depth/Height Unit" = "m",
         "Sample Collection Method ID" = "BVR SWQAPP",
         "Sample Collection Method Context" = "CA_BVR",
         "Sample Collection Equipment Name" = "Probe/Sensor",
         "Sample Collection Equipment Comment" = "Hydrolab Surveyor DS5 Multiprobe",
         "Characteristic Name" = `Characteristic Name`,
         "Result Unit" = unit_lookup[`Characteristic Name`],
         "Characteristic Name User Supplied" = "",
         "Method Speciation" = "",
         "Result Detection Condition" = "",
         "Result Value" = `Result Value`,
         "Result Unit" = `Result Unit`,
         "Result Measure Qualifier" = "",
         "Result Sample Fraction" = "",
         "Result Status ID" = "Final",
         "ResultTemperatureBasis" = "",
         "Statistical Base Code" = "",
         "ResultTimeBasis" = "",
         "Result Value Type" = "Actual",
         "Activity ID (CHILD-subset)" = make_activity_id(location_id = `Monitoring Location ID`,
                                          date = `Activity Start Date`,
                                          time = `Activity Start Time`,
                                          activity_type = `Activity Type`,
                                          equipment_name = `Sample Collection Equipment Name`,
                                          depth = `Activity Depth/Height Measure`,
                                          equipment_comment = `Sample Collection Equipment Comment`),
         "Result Analytical Method ID" = "",
         "Result Analytical Method Context" = "",
         "Analysis Start Date" = "",
         "Result Detection/Quantitation Limit Type" = "",
         "Result Detection/Quantitation Limit Measure" = "",
         "Result Detection/Quantitation Limit Unit" = "",
         "Result Comment" = ""
         
         ) |> 
  select(-c(date, depth10, time)) |> 
  relocate("Project ID",
           "Monitoring Location ID",
           "Activity ID (CHILD-subset)",
           "Activity ID User Supplied(PARENTs)",
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
           "Result Detection/Quantitation Limit Measure",
           "Result Detection/Quantitation Limit Unit",
           "Result Comment"
           )

write_csv(hydrolab_formatted_for_wqx, "data/hydrolab_wqx.csv")
v
  
