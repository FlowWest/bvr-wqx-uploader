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

parse_bend_genetics <- function(file_path) {
    bend_full_df <- read_csv(file_path) |>
        mutate(across(`Anatoxin-a (ug/L)`:`Pheophytin-a (ug/L)`, as.character)) |> 
        pivot_longer(cols = `Anatoxin-a (ug/L)`:`Pheophytin-a (ug/L)`,
                           names_to = "Target",
                           values_to = "Result",
                           values_ptypes = character()) |>
        mutate("Project ID" = project_id_lookup[Location],
               "Characteristic Name" = str_trim(str_extract(Target, "^[^(]+")),
               "Result Unit" = str_replace_all(str_extract(Target, "\\(([^)]+)\\)"), "[()]", "")) |> 
        filter(!`Result Unit` %in% c("copies/g", "toxins/g","toxin/g")) |> 
        mutate("Method" = case_when(`Result Unit` == "copies/mL" ~ "QPCR",
                                    # `Result Unit` == "copies/g"~ "QPCR",
                                    `Result Unit` == "ug/L" ~ "ELISA",
                                    # `Result Unit` == "toxins/g" ~ "ELISA",
                                    # `Result Unit` == "toxin/g" ~ "ELISA",
                                    .default = "")) |> 
        mutate("Characteristic Name" = case_when(`Characteristic Name` == "Microcystin/Nod." ~ "Microcystin/nodularin genes mcyE/ndaF",
                                                 `Characteristic Name` == "qPCR-anaC" ~ "Anatoxin-a",
                                                 `Characteristic Name` == "qPCR-mcyE" ~ "Microcystin",
                                                 `Characteristic Name` == "qPCR-sxtA" ~ "Saxitoxin",
                                                 `Characteristic Name` == "qPCR-cyrA" ~ "Cylindrospermopsin",
                                                 `Characteristic Name` == "Pheophytin-a" ~ "Pheophytin a",
                                                 `Characteristic Name` == "Chloropyhll-a" ~ "Chlorophyll a",    
                                                 .default = `Characteristic Name`),
               "Method" = case_when(`Characteristic Name` == "Pheophytin a" ~ "EPA 455.0",
                                    `Characteristic Name` == "Chlorophyll a" ~ "EPA 455.0",
                                    .default = `Method`)) |> 
        select(-c(`Target`, `Project`)) |> 
        relocate("Project ID", .before = "Location")
    # View(bend_full_df)
    
    return(bend_full_df)
}
    
bend_genetics_to_wqx <- function(data) {
    data |> 
        # pivot_longer(cols = `Anatoxin-a (ug/L)`:`Pheophytin-a (ug/L)`,
        #                    names_to = "Target",
        #                    values_to = "Result",
        #                    values_ptypes = character()) |> 
        # mutate("Characteristic Name" = str_trim(str_extract(Target, "^[^(]+")),
        #        "Result Unit" = str_replace_all(str_extract(Target, "\\(([^)]+)\\)"), "[()]", "")) |> 
        # filter(!`Result Unit` %in% c("copies/g", "toxins/g","toxin/g")) |> 
        # mutate("Method" = case_when(`Result Unit` == "copies/mL" ~ "QPCR",
        #                             # `Result Unit` == "copies/g"~ "QPCR",
        #                             `Result Unit` == "ug/L" ~ "ELISA",
        #                             # `Result Unit` == "toxins/g" ~ "ELISA",
        #                             # `Result Unit` == "toxin/g" ~ "ELISA",
        #                             .default = "")) |> 
        # mutate("Characteristic Name" = case_when(`Characteristic Name` == "Microcystin/Nod." ~ "Microcystin/nodularin genes mcyE/ndaF",
        #                                          `Characteristic Name` == "qPCR-anaC" ~ "Anatoxin-a",
        #                                          `Characteristic Name` == "qPCR-mcyE" ~ "Microcystin",
        #                                          `Characteristic Name` == "qPCR-sxtA" ~ "Saxitoxin",
        #                                          `Characteristic Name` == "qPCR-cyrA" ~ "Cylindrospermopsin",
        #                                          `Characteristic Name` == "Pheophytin-a" ~ "Pheophytin a",
        #                                          `Characteristic Name` == "Chloropyhll-a" ~ "Chlorophyll a",    
        #                                          .default = `Characteristic Name`),
        #        "Method" = case_when(`Characteristic Name` == "Pheophytin a" ~ "EPA 455.0",
        #                             `Characteristic Name` == "Chlorophyll a" ~ "EPA 455.0",
        #                             .default = `Method`)) |>
        mutate(
            # "Project ID" = project_id_lookup[Location],
            # "Project ID" = "HAB",
            "Monitoring Location ID" = Location,
            "Activity ID User Supplied (PARENTs)" = "",
            "Activity Type" = "Sample-Routine",
            "Activity Media Name" = "Water",
            "Activity Start Date" = format(mdy(`Collected`), "%m/%d/%Y"),
            # use the lubridate package function mdy_hm() to formate time in HH:MM
            "Activity Start Time" = format(`Time...7`, "%H:%M"),
            "Activity Start Time Zone" = "PST",
            # Need activity depth/height, unit
            "Activity Depth/Height Measure" = "0.151",
            "Activity Depth/Height Unit" = "m",
            # Confirm Sample Collection method id is BVR SWQAPP
            "Sample Collection Method ID" = "BVR SWQAPP",
            "Sample Collection Method Context" = "CA_BVR",
            # Confirm Equipment for bend is Water Bottle
            "Sample Collection Equipment Name" = case_when(`Sample Type` == "SPATT" ~ "SPATT Bags",
                                                           `Sample Type` == "Water Grab" ~ "Water Bottle",
                                                           .default = ""),
            "Sample Collection Equipment Comment" = "",
            "Characteristic Name User Supplied" = "",
            "Method Speciation" = "",
            "Result Detection Condition" = ifelse(Result == "ND", "Not Detected", ""),
            "Result Value" = ifelse(Result == "ND", "", gsub(",", "", Result)),
            "Result Unit" = ifelse(Result == "ND", "", `Result Unit`),
            "Result Measure Qualifier" = "",
            "Result Sample Fraction" = "Total",
            "Result Status ID" = "Final",
            "ResultTemperatureBasis" = "",
            "Statistical Base Code" = "",
            "ResultTimeBasis" = "",
            "Result Value Type" = "Actual",
            # method_lookup is in the lookup table. It points the methods to their IDs.
            "Result Analytical Method ID" = case_when(
                `Method` == "ELISA" ~ abraxis_id_lookup[`Characteristic Name`],
                # `Method` == "QPCR" ~ method_id_lookup[Method],
                .default = method_id_lookup[Method]),
            # method_context_lookup is in the lookup table. It points the method to their method context.
            "Result Analytical Method Context" = method_context_lookup[Method],
            "Analysis Start Date" = format(mdy(`Received`), "%m/%d/%Y"),
            "Result Detection/Quantitation Limit Type" = "Practical Quantitation Limit",
            "Result Detection/Quantitation Limit Measure" = case_when(
                `Method` == "QPCR" ~ "100",
                `Method` == "ELISA" ~ elisa_quantitation_limit_lookup[`Characteristic Name`],
                .default = ""
            ),
            "Result Detection/Quantitation Limit Unit" = case_when(
                `Method` == "QPCR" ~ "copies/mL",
                `Method` == "ELISA" ~ "ug/L",
                .default = ""
            ),
            "Result Comment" = ifelse(is.na(Notes), "", Notes),
            "Activity ID (CHILD-subset)" = bend_genetics_make_activity_id(location_id = Location,
                                                                          date = `Activity Start Date`,
                                                                          time = `Activity Start Time`,
                                                                          activity_type = `Activity Type`,
                                                                          equipment_name = `Sample Collection Equipment Name`,
                                                                          depth = `Activity Depth/Height Measure`)
        ) |> 
        relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)") |> 
        relocate("Characteristic Name", .before = "Characteristic Name User Supplied") |> 
        relocate("Result Unit", .before = "Result Measure Qualifier") |>
        relocate("Project ID", .before = "Monitoring Location ID") |> 
        select("Project ID":"Result Comment")
        # relocate("Result", .before = "Result Unit") |> 
        # select(-c(0:13)) |> 
        # select(-c(`Result`, `Method`)) |> View()
        # select(-c(`Method`))
}