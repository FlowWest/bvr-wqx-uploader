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

parse_bend_genetics <- function(filepath) {
    raw_bend_data <- read_csv(filepath, skip = 7)  
    raw_bend_data <- raw_bend_data |> select(-starts_with("..."))
    raw_bend_data <- raw_bend_data[!is.na(raw_bend_data$Location), ]
    
    # Find the index of the first occurrence of "Bend Genetics, LLC" in the "Location" column
    df_cutoff_1 <- min(which(raw_bend_data$Location == "Bend Genetics, LLC")) - 1
    
    # Find the index of the second occurrence of "Bend Genetics, LLC" in the "Location" column
    df_cutoff_2 <- max(which(raw_bend_data$Location == "Bend Genetics, LLC")) - 1
    
    # Create the analytical_report_for_samples dataframe
    analytical_report_for_samples <- raw_bend_data[1:df_cutoff_1, ]
    
    # Find the index of the row where "Sample ID" appears in the "Sample ID" column
    sample_id_row_index <- min(which(raw_bend_data$`Sample ID` == "Sample ID"))
    
    # Create the sample_results dataframe
    sample_results <- raw_bend_data[(sample_id_row_index + 1):df_cutoff_2, ]  
    
    sample_results <- sample_results|>
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
            Units = ifelse(Units== "\\xb5g/L", "ug/L", Units)
        )
    
    bend_full_df <-
        left_join(analytical_report_for_samples, sample_results) |> 
        filter(!is.na(Target))
    
    return(bend_full_df) 
             })
