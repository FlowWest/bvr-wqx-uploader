project_sites <- c(
    "M1",
    "M2",
    "M3",
    "M4",
    "HSP",
    "BVSWD1",
    "NBPRSC",
    "RSTCC",
    "BVCL1",
    "BVRTC1",
    "BVRTCC",
    "BVSWDRV",#not in cdx
    "RVSI1",#not in cdx
    "RVSI2",#not in cdx
    "BVCL2",	
    "BVCL3",	
    "BVCL5",	
    "BVCL6",	
    "BVCL11",	
    "BVCL12",	
    "BVCL13",	
    "BVCL14",	
    "BVCL15",	
    "BVCL16",	
    "BVCL17",	
    "BVCL18",	
    "BVCL19",	
    "BVCL20",
    "FC1",
    "FC2",
    "FC3",#not in cdx
    "MC1",
    "MC2",
    "TC1",
    "AC1",
    "AC2",
    "AC3",
    "AC4",
    "MCC1",
    "KC1",
    "CC1",
    "SC1",
    "SC2",#not in cdx
    "SHC2",#not in cdx
    "CC2",#not in cdx
    "SIEG01",#not in cdx
    "COOP01",#not in cdx
    "CLOV01",#not in cdx
    "DRY01",#not in cdx
    "COY01",#not in cdx
    "SCOT01",#not in cdx
    "AND01",#not in cdx
    "NFORK01",#not in cdx
    "LONG01", #not in cdx
    "PUT01", #not in cdx
    "MID01", #not in cdx
    "KC5", #not in cdx
    "AP01",
    "BP",
    "CLOAKS01",
    "CLV7",
    "CP",
    "ELEM01",
    "GH",
    "HB",
    "JB",
    "KEYS01",
    "KEYS03",
    "KP01",
    "LC01",
    "LPTNT",
    "LS",
    "LS2",
    "LUC01",
    "RED01",
    "RODS",
    "SBMMEL01",
    "SHADY01",
    "UBL",
    "CL-1",
    "CL-3",
    "CL-4",
    "CL-5",
    "LA-03",
    "NR-02",
    "OA-04",
    "UA-01",
    "UA-06",
    "UA-07",
    "UA-08",
    "PILLS01", #not in cdx
    "LAKEPILS01"  #not in cdx
)

tibble(project_id = project_sites) |> mutate(id = row_number()) |> select(id, project_id)
