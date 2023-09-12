
project_id_lookup <- c(
    "M1" = "MS",
    "M2" = "MS",
    "M3" = "MS",
    "M4" = "MS",
    "HSP" = "SW",
    "BVSWD1" = "SW",
    "NBPRSC" = "SW",
    "RSTCC" = "SW",
    "BVCL1" = "SW",
    "BVRTC1" = "SW",
    "BVRTCC" = "SW",
    "BVSWDRV" = "SW",#not in cdx
    "RVSI1" = "SW",#not in cdx
    "RVSI2" = "SW",#not in cdx
    "BVCL2" = "CLM",	
    "BVCL3" = "CLM",	
    "BVCL5" = "CLM",	
    "BVCL6" = "CLM",	
    "BVCL11" = "CLM",	
    "BVCL12" = "CLM",	
    "BVCL13" = "CLM",	
    "BVCL14" = "CLM",	
    "BVCL15" = "CLM",	
    "BVCL16" = "CLM",	
    "BVCL17" = "CLM",	
    "BVCL18" = "CLM",	
    "BVCL19" = "CLM",	
    "BVCL20" = "CLM",
    "FC1" = "CS", 
    "FC2" = "CS",
    "FC3" = "CS",#not in cdx
    "MC1" = "CS",
    "MC2" = "CS",
    "TC1" = "CS",
    "AC1" = "CS",
    "AC2" = "CS",
    "AC3" = "CS",
    "AC4" = "CS",
    "MCC1" = "CS",
    "KC1" = "CS",
    "CC1" = "CS",
    "SC1" = "CS",
    "SC2" = "CS",#not in cdx
    "SHC2"= "CS",#not in cdx
    "CC2" = "CS",#not in cdx
    "SIEG01" = "CS",#not in cdx
    "COOP01" = "CS",#not in cdx
    "CLOV01" = "CS",#not in cdx
    "DRY01" = "CS",#not in cdx
    "COY01" = "CS",#not in cdx
    "SCOT01"= "CS",#not in cdx
    "AND01" = "CS",#not in cdx
    "NFORK01" = "CS",#not in cdx
    "LONG01" = "CS", #not in cdx
    "PUT01" = "CS", #not in cdx
    "MID01"= "CS", #not in cdx
    "KC5"= "CS", #not in cdx
    "AP01" = "HAB",
    "BP" = "HAB",
    "CLOAKS01" = "HAB",
    "CLV7" = "HAB",
    "CP" = "HAB",
    "ELEM01" = "HAB",
    "GH" = "HAB",
    "HB" = "HAB",
    "JB" = "HAB",
    "KEYS01" = "HAB",
    "KEYS03" = "HAB",
    "KP01" = "HAB",
    "LC01" = "HAB",
    "LPTNT" = "HAB",
    "LS" = "HAB",
    "LS2" = "HAB",
    "LUC01" = "HAB",
    "RED01" = "HAB",
    "RODS" = "HAB",
    "SBMMEL01" = "HAB",
    "SHADY01" = "HAB",
    "UBL" = "HAB",
    "CL-1" = "HAB",
    "CL-3" = "HAB",
    "CL-4" = "HAB",
    "CL-5" = "HAB",
    "LA-03" = "HAB",
    "NR-02" = "HAB",
    "OA-04" = "HAB",
    "UA-01" = "HAB",
    "UA-06" = "HAB",
    "UA-07" = "HAB",
    "UA-08" = "HAB",
    "PILLS01" = "HAB", #not in cdx
    "LAKEPILS01" = "HAB" #not in cdx
)

unit_lookup <- c(
    "Temperature, water" = "deg C", 
    "Specific conductance" = "mS/cm", 
    "Resistivity" = "KOhm-cm", 
    "Salinity" = "ppt", 
    "Total dissolved solids" = "g/L", 
    "Dissolved oxygen saturation" = "%",
    "Dissolved oxygen (DO)" = "mg/L", 
    "pH" = "None", 
    "Turbidity" = "NTU",
    "Oil & Grease (HEM)" = "mg/L",
    "Nitrate + Nitrite as N" = "mg/L",
    "Phosphorus, total" = "mg/L",
    "Total Organic Carbon" = "mg/L")

method_id_lookup <- c(
    "SM9223B" = "9223-B",
    "EPA 300.0" = "300.0",
    "ELISA" = "520060",
    "QPCR" = "1611",
    "EPA 1664A" = "1664A",
    "SM4500-NO3 E" = "4500-NO3(E)",
    "SM4500-P F" = "4500-P-F",
    "SM5310C" = "5310-C"
)

method_context_lookup <- c(
    "SM9223B" = "APHA",
    "EPA 300.0" = "USEPA",
    "ELISA" = "ABRAXIS LLC",
    "QPCR" = "USEPA",
    "EPA 1664A" = "USEPA",
    "SM4500-NO3 E" = "APHA",
    "SM4500-P F" = "APHA",
    "SM5310C" = "APHA"
    
)

characteristic_lookup <- c(
    "Oil & Grease (HEM)" = "Oil and Grease",
    "Phosphorus, total" = "Phosphorus",
    "Total Organic Carbon" = "Organic carbon",
    "E. Coli" = "Escherichia coli",
    "Nitrate + Nitrite as N" = "Nitrate + Nitrite",
    "Total Kjeldahl Nitrogen" = "Kjeldahol Nitrogen"
)

method_speciation_lookup <- c(
    "Nitrate + Nitrite as N" = "as N",
    "Nitrate as N" = "as N",
    "Nitrite as N" = "as N",
    "Phosphorus, total" = "as P",
    "Total Nitrogen" = "as N",
    "Total Kjeldahl Nitrogen" = "as N",
    "Orthophosphate" = "as P"
)


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
        TRUE ~ NA_character_
    )
    hhmm <- gsub(':', '', time)
    equipment_comment <- case_when(
        equipment_comment == "Hydrolab Surveyor DS5 Multiprobe" ~ "Hydro",
        equipment_comment == "AlgaeChek Ultra Fluorometer" ~ "Algae",
        TRUE ~ NA_character_
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
            "Activity ID User Supplied (PARENTs)" = NA,
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
            "Sample Collection Equipment Comment" = NA,
            "Characteristic Name" = ifelse(
                ANALYTE %in% names(characteristic_lookup), characteristic_lookup[ANALYTE], ANALYTE),
            "Characteristic Name User Supplied" = NA,
            "Method Speciation" = ifelse(
                ANALYTE %in% names(method_speciation_lookup), method_speciation_lookup[ANALYTE], NA),
            "Result Detection Condition" = case_when(
                Result == "ND" ~ "Not Detected",
                Result == "Absent" ~ "Not Present",
                Result == "Present" ~ "Detected Not Quantified",
                TRUE ~ NA_character_
            ),
            "Result Value" = ifelse(
                Result == "Absent" |
                    Result == "ND" | Result == "Present",
                NA_character_,
                Result
            ),
            "Result Unit" = ifelse(UNITS == ".", NA_character_, UNITS),
            "Result Measure Qualifier" = NA,
            "Result Sample Fraction" = "Total",
            "Result Status ID" = "Final",
            "ResultTemperatureBasis" = NA,
            "Statistical Base Code" = NA,
            "ResultTimeBasis" = NA,
            "Result Value Type" = ifelse(is.na(Result), NA_character_, "Actual"),
            "Result Analytical Method ID" = ifelse(
                is.na(METHODNAME), NA_character_, method_id_lookup[METHODNAME]),
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
            "Result Detection/Quantitation Limit Type" = ifelse(DL == "NA", NA_character_, "Method Detection Level"),
            "Result Detection/Quantitation Limit Measure" = ifelse(DL == "NA", NA_character_, DL),
            "Result Detection/Quantitation Limit Unit" = ifelse(UNITS == ".", NA_character_, UNITS),
            "Result Comment" = NA
            
        ) %>%
        select(-c(0:48)) %>%
        relocate("Activity ID (CHILD-subset)", .before = "Activity ID User Supplied (PARENTs)") |> 
        mutate(
            "Result Analytical Method Context" = ifelse(
                is.na(`Result Analytical Method ID`),
                NA_character_,
                `Result Analytical Method Context`
            ),
            "Result Unit" = ifelse(
                is.na(`Result Value`),
                NA_character_,
                `Result Unit`
            )
        ) |>
        relocate("Result Analytical Method Context", .before = "Analysis Start Date")
}