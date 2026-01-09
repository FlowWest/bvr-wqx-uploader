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
    "JC1" = "CS",
    "KC3-E" = "CS",
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
    "LAKEPILS01" = "HAB",
    "HILL01" = "CS",
    "AND01 (up)" = "CalWatch",
    "AND01 (down)" = "CalWatch",
    "SHC01" = "CalWatch",
    "MIDSCOT01" = "CalWatch"
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
    "SM5310C" = "5310-C",
    "SM9221C,E" = "9221-E",
    "SM9221B,C" = "9221-B",
    "EPA 351.2" = "351.2",
    "EPA 455.0" = "455.0"
)

abraxis_id_lookup = c(
    "Anatoxin-a" = "520060",
    "Cylindrospermopsin" = "522011",
    "Microcystin/Nod." = "520011",
    "Saxitoxin" = "52255B",
    "Microcystin/nodularin genes mcyE/ndaF" = "520011"
)

method_context_lookup <- c(
    "SM9223B" = "APHA",
    "EPA 300.0" = "USEPA",
    "ELISA" = "ABRAXIS LLC",
    "QPCR" = "USEPA",
    "EPA 1664A" = "USEPA",
    "SM4500-NO3 E" = "APHA",
    "SM4500-P F" = "APHA",
    "SM5310C" = "APHA",
    "SM9221C,E" = "APHA",
    "SM9221B,C" = "APHA",
    "EPA 351.2" = "USEPA",
    "EPA 455.0" = "USEPA",
    "SM2310B" = "APHA_SM20ED",
    "SM2320B" = "APHA_SM21ED",
    "SM4500NH3C" = "SM",
    "EPA600" = "USEPA",
    "100.2" = "USEPA",
    "EPA821-R-02-01" = "USEPA",
    "SM5210B" = "SM",
    "SM5220D" = "SM",
    "SM1020" = "SM",
    "SM4500-CLF" = "SM",
    "SM9230" = "SM",
    "Enterolert" = "Enterolert",
    "SM9221" = "SM",
    "SM9223" = "SM",
    "SM9215B" = "SM",
    "SM2510B" = "SM",
    "SM4500-OG" = "SM",
    "1623" = "USEPA",
    "SM5540C" = "SM",
    "SM4500 H+B" = "SM",
    "SM4500-PE" = "SM",
    "SM4500" = "SM",
    "SM2540F" = "SM",
    "SM2540C" = "SM",
    "SM2540B" = "SM",
    "SM2540D" = "SM",
    "SM2340B" = "SM",
    "EPA245.1" = "USEPA",
    "EPA200.7" = "USEPA",
    "EPA200.8" = "USEPA",
    "EPA300.1" = "USEPA",
    "EPA900.0" = "USEPA",
    "SM3500" = "SM",
    "Quickchem 10-204-00-1-X" = "Quickchem",
    "SM4500-CRB" = "SM",
    "SM4500-CNG" = "SM",
    "SM5310C" = "SM",
    "EPA300.0" = "USEPA",
    "EPA218.6" = "USEPA",
    "EPA8315" = "USEPA",
    "EPA1664" = "USEPA",
    "EPA420.1" = "USEPA",
    "EPA8015" = "USEPA",
    "EPA624.1" = "USEPA",
    "EPA8260" = "USEPA",
    "EPA8270" = "USEPA",
    "EPA314.0" = "USEPA",
    "EPA8081/8082" = "USEPA",
    "EPA610" = "USEPA",
    "EPA614" = "USEPA",
    "EPA8141" = "USEPA",
    "EPA8151" = "USEPA",
    "EPA1620" = "USEPA",
    "EPA1666(Purge/Trap)" = "USEPA",
    "EPA1666(Dir. Inject)" = "USEPA",
    "EPA1010" = "USEPA",
    "EPA625.1" = "USEPA",
    "EPA1630E" = "USEPA",
    "EPA1631" = "USEPA",
    "EPA625.1SIM" = "USEPA",
    "EPA908" = "USEPA",
    "EPA903" = "USEPA",
    "EPA904" = "USEPA",
    "EPA906" = "USEPA",
    "DOD QSM V5.3" = "DOD",
    "Battelle" = "Battelle",
    "SM4500SiO2 D" = "SM",
    "EPA531.1" = "USEPA"
    
    
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

# elisa_quantitation_limit_lookup <- c(
#     "Microcystin/nodularin genes mcyE/ndaF" = "0.15",
#     "Anatoxin-a" = "0.15",
#     "Saxitoxin" = "0.02",
#     "Cylindrospermopsin" = ""
# )

save_objects <- function() {
    save(
        project_id_lookup,
        hydro_unit_lookup,
        method_id_lookup,
        method_context_lookup,
        characteristic_lookup,
        method_speciation_lookup,
        abraxis_id_lookup,
        file = "data/lookup_objects.rdata"
    )
    
}

save_objects()
