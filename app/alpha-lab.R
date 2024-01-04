parse_alphalab <- function(filepath) {
    readxl::read_excel(filepath) |> 
        tidyr::separate(SAMPLENAME, into=c("sample1", "sample2"), sep = " ") |> # TODO this looks a little too hard-coded 
        dplyr::mutate(SAMPLENAME = ifelse(sample1 %in% names(project_id_lookup), sample1, sample2)) |>
        dplyr::select(-sample1, -sample2) |> 
        dplyr::relocate(SAMPLENAME, .before = LABSAMPID)
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
        select(PROJECT, ANALYTE, Result) |> 
        pivot_wider(names_from = "ANALYTE", values_from = "Result")
}

alpha_lab_to_wqx <- function(data) {
    data |> 
        mutate(
            "Project ID" = project_id_lookup[SAMPLENAME],
            "Monitoring Location ID" = SAMPLENAME,
            "Activity ID User Supplied (PARENTs)" = "",
            "Activity Type" = "Sample-Routine",
            "Activity Media Name" = MATRIX,
            "Activity Start Date" = format(mdy_hms(SAMPDATE), "%m/%d/%Y"),
            "Activity Start Time" = format(mdy_hms(SAMPDATE), "%H:%M"),
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
            "Analysis Start Date" = format(mdy_hms(ANADATE), "%m/%d/%Y"),
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
        relocate("Result Analytical Method Context", .before = "Analysis Start Date")
}