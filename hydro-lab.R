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
        filter(str_count(Date, "\\d+") > 2) # TODO a little sus
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
        equipment <- ifelse(equipment_name == "Probe/Sensor", "PS", NA)
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
               "Sample Collection Method ID" = "BVR Tribal SWQAPP",
               "Sample Collection Method Context" = "CA_BVR",
               "Sample Collection Equipment Name" = "Probe/Sensor",
               "Sample Collection Equipment Comment" = "Hydrolab Surveyor DS5 Multiprobe",
               "Characteristic Name" = `Characteristic Name`,
               "Result Unit" = hydro_unit_lookup[`Characteristic Name`],
               "Characteristic Name User Supplied" = "",
               "Method Speciation" = "",
               "Result Detection Condition" = "",
               "Result Value" = if_else(`Result Value`== 999999, NA, `Result Value`),
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
               "Result Analytical Method ID" = "",
               "Result Analytical Method Context" = "",
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
                 "Result Detection/Quantitation Limit Measure",
                 "Result Detection/Quantitation Limit Unit",
                 "Result Comment"
        )
}



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

hydro_unit_lookup <- c(
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
    "Total Organic Carbon" = "mg/L",
    "Chlorophyll a" = "ug/L",
    "Phycocyanin" = "#/mL")

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
    "Nitrate + Nitrite as N" = "Nitrate + Nitrite"
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

