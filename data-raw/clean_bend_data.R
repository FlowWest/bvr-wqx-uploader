library(dplyr)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)
library(readr)
load("lookup_objects.rdata")

# ------------------------------------------------------------------------------
# Parse data based on sample_id
read_bend <- function(file) {
  vect_file_name <- c(file)
  # print(vect_file_name)
  if (stringr::str_detect(vect_file_name, "xlsx$") == TRUE) {
    bend_raw <- read_excel(file, skip = 5)|>  
      clean_names() |> 
      mutate(sample_id = as.numeric(sample_id)) |> 
      select(-x8)  
    first_df <- bend_raw[1:which(is.na(bend_raw$sample_id))[1] - 1, ]
    second_df <- bend_raw[(which(is.na(bend_raw$sample_id))[2]) : (which(is.na   (bend_raw$sample_id))[3] - 1), ] |>
      row_to_names(row_number = 1) |>
      clean_names() |> 
      rename(sample_id = na)
  }else{
    header_chunk <- readr::read_lines(file, n_max = 100)
    skip_n <- which(stringr::str_detect(header_chunk, "^SAMPLE RESULTS"))
    bend_raw <- read_csv(file, skip = skip_n)
    bend_raw <- bend_raw|> 
      filter(is.na(bend_raw$`Sample ID`) == FALSE)|>  
      clean_names() |> 
      # mutate(sample_id = as.numeric(sample_id)) |> 
      select(-x8)
    first_df <- bend_raw[1:which(bend_raw$sample_id == "QUALITY CONTROL") - 1, ]
    second_df <- bend_raw[which(bend_raw$sample_id == "QUALITY CONTROL") + 1 : which(bend_new$sample_id == "QUALIFIERS/COMMENTS/NOTES"), ]|>
      row_to_names(row_number = 1) |>
      clean_names() 
      # rename(sample_id = na)
    }
  bend_full_df <-   left_join(first_df, second_df)
}

bend_raw <- read_bend("data-raw/bend/20220913_BV_Results.csv")
bend_raw_xlsx <- read_bend("data-raw/bend/BendHAB.xlsx")

# ------------------------------------------------------------------------------
bend_new <- read_csv("data-raw/bend/20220913_BV_Results.csv", skip = 52)
bend_new <- bend_new|> 
  filter(is.na(bend_new$`Sample ID`) == FALSE)|>  
  clean_names()  |> 
  # mutate(sample_id = as.numeric(sample_id)) |> 
  select(-x8)

first_df <- bend_new[1:which(bend_new$sample_id == "QUALITY CONTROL") - 1, ]
second_df <- bend_new[which(bend_new$sample_id == "QUALITY CONTROL") + 1 : which(bend_new$sample_id == "QUALIFIERS/COMMENTS/NOTES"), ]|>
  row_to_names(row_number = 1) |>
  clean_names() |> glimpse()
# header_chunk <- readr::read_lines("data-raw/bend/20220913/_BV_Results.csv", n_max = 100)
# header_chunk <- readr::read_lines("data-raw/bend/BendHAB.xlsx", n_max = 100)

skip_n <- which(stringr::str_detect(header_chunk, "^SAMPLE RESULTS"))
# x <- read_csv("data-raw/bend/20220913_BV_Results.csv", skip = skip_n)

# file <- c("data-raw/bend/BendHAB.xlsx")
# stringr::str_detect(file, "xlsx$/")
# ------------------------------------------------------------------------------


# bend_raw <- read_excel("data-raw/bend/BendHAB.xlsx") |> 
  # as_tibble() |> 
  # slice(100) |> glimpse()
  # unlist(., use.names= FALSE)

bend_raw <- read_excel("data-raw/bend/BendHAB.xlsx", skip = 5)  |>  
  clean_names() |> 
  mutate(sample_id = as.numeric(sample_id)) |> 
  select(-x8)

first_df <- bend_raw[1:which(is.na(bend_raw$sample_id))[1] - 1, ]
second_df <- bend_raw[(which(is.na(bend_raw$sample_id))[2]) : (which(is.na(bend_raw$sample_id))[3] - 1), ] |>
  row_to_names(row_number = 1) |>
  clean_names() |> 
  rename(sample_id = na) |> glimpse()

# -----------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
bend_full_df <-   left_join(first_df, second_df) 
bend_wqx <- bend_full_df |> 
  mutate("Project ID" = "HAB",
         # Not all locations are recorded in master monitoring plan:
        # Benthic LUC01, OA04, LA03
         "Monitoring Location ID" = location,
         "Activity ID User Supplied (PARENTs)" = NA,
         "Activity Type" = "Sample-Routine", 
         "Activity Media Name" = matrix,
         "Activity Start Date" = format(mdy_hm(date_collected), "%m/%d/%Y"),
         "Activity Start Time" = format(mdy_hm(date_collected), "%H:%M"),
         "Activity Start Time Zone" = "PST",
        # Need activity depth/height, unit
         "Activity Depth/Height Measure" = "0.152",
         "Activity Depth/Height Unit" = "m",
        # Confirm Sample Collection method id is BVR SWQAPP
         "Sample Collection Method ID" = "BVR SWQAPP",
         "Sample Collection Method Context" = "CA_BVR",
        # Confirm Equipment for bend is Water Bottle
         "Sample Collection Equipment Name" = "Water Bottle",
         "Sample Collection Equipment Comment" = NA,
        #Should we use microcystin/nod. or just microcystin
         "Characteristic Name" = ifelse(target == "Microcystin/Nod.", "Microcystin/nodularin genes mcyE/ndaF", target),
         "Characteristic Name User Supplied" = NA,
         "Method Speciation" = NA,
         "Result Detection Condition" = ifelse(result == "ND", "Not Detected", NA),
         "Result Value" = ifelse(result == "ND", NA, result),
        # Do we use copies/mL? do not see it in previous wqx upload
         "Result Unit" = ifelse(result == "ND", NA, units),
         "Result Measure Qualifier" = NA,
         "Result Sample Fraction" = "Total",
         "Result Status ID" = "Final",
         "ResultTemperatureBasis" = NA,
         "Statistical Base Code" = NA,
         "ResultTimeBasis" = NA,
         "Result Value Type" = "Actual",
        # Seems like we are only using ELSA analysis not QPCR?
         "Result Analytical Method ID" = method_lookup[method],
         "Result Analytical Method Context" = method_context_lookup[method],
         "Analysis Start Date" = format(mdy_hm(date_received), "%m/%d/%Y"),
         "Result Detection/Quantitation Limit Type" = "Practical Quantitation Limit",
         "Result Detection/Quantitation Limit Measure" = quantitation_limit,
         "Result Detection/Quantitation Limit Unit" = units,
         "Result Comment" = notes,
         "Activity ID (CHILD-subset)" = make_activity_id(location_id = location,
                                                        date = `Activity Start Date`,
                                                        time = `Activity Start Time`,
                                                        activity_type = `Activity Type`,
                                                        equipment_name = `Sample Collection Equipment Name`,
                                                        depth = `Activity Depth/Height Measure`)
  ) |> 
  relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)") |> 
  select(-c(0:13))

bend_wqx <- bend_wqx |> 
  mutate("Result Unit" = ifelse(bend_wqx$`Result Unit` == "µg/L", "ug/L", bend_wqx$`Result Unit`),
         "Result Detection/Quantitation Limit Unit" = ifelse(bend_wqx$`Result Detection/Quantitation Limit Unit` == "µg/L", "ug/L", bend_wqx$`Result Detection/Quantitation Limit Unit`))
# ------------------------------------------------------------------------------
write_csv(bend_wqx, "data/bend_wqx.csv", na = "")
# write_csv(bend_wqx, "data/bend_wqx.csv", na = "")

         