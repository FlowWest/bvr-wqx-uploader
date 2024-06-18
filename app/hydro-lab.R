parse_hydrolab <- function(filepath) {
    regex_pattern <- "\\w+"
    raw_name <- readLines(filepath, 1)
    location_for_selected_hydrolab <- unlist(str_extract_all(raw_name, regex_pattern))[4]
    read_csv(filepath,
             skip = 5,
             col_types = "c",
             col_select = c(1:28)) |>
        mutate_if(is.character, utf8::utf8_encode) |>
        select(-starts_with("...")) |>
        mutate("location_id" = location_for_selected_hydrolab) |> 
        filter(grepl("^[0-9]", Date))
}

hydro_lab_make_activity_id <-
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
            equipment_comment == "AccuWeather" ~ "AccuW",
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

hydro_lab_to_wqx <- function(data) {
    data |> 
        select(-c("IBVSvr4")) |>
        rename("Temperature, water" = "Temp",
               "Specific conductance" = "SpCond",
               "Resistivity" = "Res",
               "Salinity" = "Sal",
               "Total dissolved solids" = "TDS",
               "Dissolved oxygen saturation" = "DO%",
               "Dissolved oxygen (DO)" = "DO",
               "pH" = "pH",
               "Turbidity" = "Turb",
               "Chlorophyll a" = "CHL",
               "Phycocyanin" = "PCY",
               "Monitoring Location ID" = location_id)|> 
        pivot_longer(
            !c(Date, Time, Depth10, `Monitoring Location ID`),
            names_to = "Characteristic Name",
            "values_to" = "Result Value"
        ) |>
        mutate("Project ID" = project_id_lookup[`Monitoring Location ID`],
               "Activity ID User Supplied(PARENTs)" = "",
               "Activity Type" = "Field Msr/Obs",
               "Activity Media Name" = "Water",
               "Activity Start Date" = format(mdy(Date), "%m/%d/%Y"),
               "Activity Start Time" = format(parse_date_time(Time, c('HMS', 'HM')), "%H:%M"),
               "Activity Start Time Zone" = "PST",
               "Activity Depth/Height Measure" = as.numeric(Depth10),
               "Activity Depth/Height Unit" = "m",
               "Sample Collection Method ID" = "BVR SWQAPP",
               "Sample Collection Method Context" = "CA_BVR",
               "Sample Collection Equipment Name" = "Probe/Sensor",
               "Sample Collection Equipment Comment" = "Hydrolab Surveyor DS5 Multiprobe",
               "Characteristic Name" = `Characteristic Name`,
               "Result Unit" = hydro_unit_lookup[`Characteristic Name`],
               "Characteristic Name User Supplied" = "",
               "Method Speciation" = "",
               "Result Detection Condition" = "",
               "Result Value" = if_else(`Result Value`== 999999, "", `Result Value`),
               "Result Unit" = `Result Unit`,
               "Result Measure Qualifier" = "",
               "Result Sample Fraction" = "",
               "Result Status ID" = "Final",
               "ResultTemperatureBasis" = "",
               "Statistical Base Code" = "",
               "ResultTimeBasis" = "",
               "Result Value Type" = "Actual",
               "Activity ID (CHILD-subset)" = hydro_lab_make_activity_id(location_id = `Monitoring Location ID`,
                                                               date = `Activity Start Date`,
                                                               time = `Activity Start Time`,
                                                               activity_type = `Activity Type`,
                                                               equipment_name = `Sample Collection Equipment Name`,
                                                               depth = `Activity Depth/Height Measure`,
                                                               equipment_comment = `Sample Collection Equipment Comment`),
               "Result Analytical Method ID" = if_else(`Characteristic Name`== "Chlorophyll a", "Probe_C", ""),
               "Result Analytical Method Context" = if_else(`Characteristic Name`== "Chlorophyll a", "CA_BVR", ""),
               "Analysis Start Date" = "",
               "Result Detection/Quantitation Limit Type" = "",
               "Result Detection/Quantitation Limit Measure" = "",
               "Result Detection/Quantitation Limit Unit" = "",
               "Result Comment" = ""

        ) |> 
        select(-c(Date, Depth10, Time)) |>
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
                 "Result Detection/Quantitation Limit Type",
                 "Result Detection/Quantitation Limit Measure",
                 "Result Detection/Quantitation Limit Unit",
                 "Result Comment"
        )
}

append_input_data <- function(data, temperature_air, result_comment){
   last_row_source <- tail(data, 1)
    last_row_source$`Activity Depth/Height Measure` <- ""
    last_row_source$`Activity Depth/Height Unit` <- ""
    last_row_source$`Sample Collection Equipment Name` <- "Miscellaneous (Other)"
    last_row_source$`Sample Collection Equipment Comment` <- "AccuWeather"
    last_row_source$`Characteristic Name` <- "Temperature, Air"
    last_row_source$`Result Value` <- temperature_air
    last_row_source$`Result Unit`<- "deg F"
    last_row_source$`Result Analytical Method ID` <- ""
    last_row_source$`Result Analytical Method Context` <- ""
    wqx_df <- rbind(data, last_row_source)
    wqx_df <- wqx_df |> 
        mutate(
            "Activity ID (CHILD-subset)" = hydro_lab_make_activity_id(location_id = `Monitoring Location ID`,
                                                                      date = `Activity Start Date`,
                                                                      time = `Activity Start Time`,
                                                                      activity_type = `Activity Type`,
                                                                      equipment_name = `Sample Collection Equipment Name`,
                                                                      depth = `Activity Depth/Height Measure`,
                                                                      equipment_comment = `Sample Collection Equipment Comment`),
            "Result Comment" = result_comment) |> 
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
                 "Result Detection/Quantitation Limit Type",
                 "Result Detection/Quantitation Limit Measure",
                 "Result Detection/Quantitation Limit Unit",
                 "Result Comment"
        )
    return(wqx_df)
}

generate_empty_data <- function(temperature_air = NULL, result_comment = NULL){
    empty_df <- data.frame(
        "Project ID" =  c(""),
        "Monitoring Location ID" = c(""),
        "Activity ID (CHILD-subset)" = c(""),
        "Activity ID User Supplied(PARENTs)" = c(""),
        "Activity Type" = c("Field Msr/Obs"),
        "Activity Media Name" = c("Water"),
        "Activity Start Date" = c(""),
        "Activity Start Time" = c(""),
        "Activity Start Time Zone" = c("PDT"),
        "Activity Depth/Height Measure" = c(""),
        "Activity Depth/Height Unit" = c(""),
        "Sample Collection Method ID" = c("BVR SWQAPP"),
        "Sample Collection Method Context" = c("CA_BVR"),
        "Sample Collection Equipment Name" = c("Probe/Sensor"),
        "Sample Collection Equipment Comment" = c(""),
        "Characteristic Name" = c("Temperature, Water", "Specific Conductance", "Resistivity", "Salinity", "Total dissolved solids", "Dissolved oxygen saturation", "Dissolved oxygen (DO)", "pH", "Turbidity", "Chlorophyll a", "Phycocyanin"),
        "Characteristic Name User Supplied" = c(""),
        "Method Speciation" = c(""),
        "Result Detection Condition" = c(""),
        "Result Value" = c(""),
        "Result Unit" = c(""),
        "Result Measure Qualifier" = c(""),
        "Result Sample Fraction" = c(""),
        "Result Status ID" = c("Final"),
        "ResultTemperatureBasis" = c(""),
        "Statistical Base Code" = c(""),
        "ResultTimeBasis" = c(""),
        "Result Value Type" = c(""),
        "Result Analytical Method ID" = c(""),
        "Result Analytical Method Context" = c(""),
        "Analysis Start Date" = c(""),
        "Result Detection/Quantitation Limit Type" = c(""),
        "Result Detection/Quantitation Limit Measure" = c(""), 
        "Result Detection/Quantitation Limit Unit" = c(""),
        "Result Comment" = ifelse(!is.null(result_comment), result_comment, c(""))
    )
    air_df <- data.frame(
        "Project ID" =  c(""),
        "Monitoring Location ID" = c(""),
        "Activity ID (CHILD-subset)" = c(""),
        "Activity ID User Supplied(PARENTs)" = c(""),
        "Activity Type" = c("Field Msr/Obs"),
        "Activity Media Name" = c("Water"),
        "Activity Start Date" = c(""),
        "Activity Start Time" = c(""),
        "Activity Start Time Zone" = c("PDT"),
        "Activity Depth/Height Measure" = c(""),
        "Activity Depth/Height Unit" = c(""),
        "Sample Collection Method ID" = c("BVR SWQAPP"),
        "Sample Collection Method Context" = c("CA_BVR"),
        "Sample Collection Equipment Name" = "Miscellaneous(Other)",
        "Sample Collection Equipment Comment" = "AccuWeather",
        "Characteristic Name" = "Temperature, Air",
        "Characteristic Name User Supplied" = c(""),
        "Method Speciation" = c(""),
        "Result Detection Condition" = c(""),
        "Result Value" = ifelse(!is.null(temperature_air), temperature_air, c("")),
        "Result Unit" = "deg F",
        "Result Measure Qualifier" = c(""),
        "Result Sample Fraction" = c(""),
        "Result Status ID" = c("Final"),
        "ResultTemperatureBasis" = c(""),
        "Statistical Base Code" = c(""),
        "ResultTimeBasis" = c(""),
        "Result Value Type" = c(""),
        "Result Analytical Method ID" = c(""),
        "Result Analytical Method Context" = c(""),
        "Analysis Start Date" = c(""),
        "Result Detection/Quantitation Limit Type" = c(""),
        "Result Detection/Quantitation Limit Measure" = c(""), 
        "Result Detection/Quantitation Limit Unit" = c(""),
        "Result Comment" = ifelse(!is.null(result_comment), result_comment, c(""))
        )

    wqx_df <- rbind(empty_df, air_df)
    colnames(wqx_df) <- gsub("\\.", " ", colnames(wqx_df)) 
    return(wqx_df)
    
}