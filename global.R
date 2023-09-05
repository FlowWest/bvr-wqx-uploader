library(shiny)
library(tidyverse)
library(bslib)
library(validate)
library(wqxWeb)
library(reticulate)
library(spsComps)

reticulate::use_virtualenv("r-reticulate")

source("hydro-lab.R")
source("alpha-lab.R")

cdx_account <- read_csv(paste0(getwd(), "/cdx-account-info.csv"))


# Hydro Lab Rules -------------------------------------------------------------
hydro_lab_range_rules <- validator(
    "Temperature in valid range" = in_range(as.numeric(Temp), -10, 50), 
    "Depth in valid range" = in_range(as.numeric(Depth10), 0, 10), 
    "Specific Conductivity in valid range" = in_range(as.numeric(SpCond), 0, 500), 
    "Resistivity in valid range" = in_range(as.numeric(Res), 0, 50),
    "Salinity is positive" = in_range(as.numeric(Sal), 0, 5), 
    "Total Dissolved Solids in valid range" = in_range(as.numeric(TDS), 0, 3),
    "Dissolved Oxygen saturation in valid range" = in_range(as.numeric(`DO%`), 0, 200), 
    "Dissolved Oxygen in valid range" = in_range(as.numeric(DO), 0, 20), 
    "pH in valid range" = in_range(as.numeric(pH), 5, 10), 
    "Turbidity in valid range" = in_range(as.numeric(Turb), 0, 1000),
    "Chlorophyl in valid range" = in_range(as.numeric(CHL), 0, 10000)
)

hydro_lab_custom_rules <- validator(
    "Location ID is Valid" = location_id %in% c("FC1", "LPTNT"), 
    "Resistivity decimals values is less then 3" = number_format(as.numeric(Res), "d.dd")
)


# Alpha Labs Rules ----------------------------------------------------------

alpha_lab_range_rules <- validator(
    "Nitrate + Nitrite in valid range" = in_range(as.numeric(`Nitrate as N`), 0, 10) || `Nitrate as N` == "Absent", 
    "Oil & Grease (HEM) in valid range" = in_range(`Oil & Grease (HEM)`, 0, 3000),
    "Phosphorus, total in valid range" = in_range(`Phosphorus, total`, 0, 2),
    "Total Organic Carbon in valid range" = in_range(`Total Organic Carbon`, 0, 10),
    "Total Coliform in valid range" = in_range(as.numeric(`Total Coliform`), 0, 300000) || `Total Coliform` == "Absent",
    "Fecal Coliform in valid range" = in_range(`Fecal Coliform`, 0, 300000), 
    "Total Kjeldahl Nitrogen in valid range" = in_range(`Total Kjeldahl Nitrogen`, 0, 10)
    
)



raw_data <- parse_alphalab("data-raw/alpha-lab/22G2998 FINAL EXCEL 09 Aug 22 1018.xls")
alpha_lab_to_wqx(raw_data)
