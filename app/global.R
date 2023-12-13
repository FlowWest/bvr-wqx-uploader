library(shiny)
library(tidyverse)
library(bslib)
library(validate)
library(wqxWeb)
library(reticulate)
library(spsComps)
library(shinyWidgets)
library(shinycssloaders)

# change this based on who is using the app
# reticulate::use_miniconda("wqxUpload")
reticulate::use_virtualenv("wqxUpload")


source("modules/hydro_lab_module.R")
source("modules/bend_genetics_module.R")
# source('modules/alpha_lab_module.R')
source('modules/user_account_module.R')

source("hydro-lab.R")
source("alpha-lab.R")
source("bend-genetics.R")

file_info <- reactiveValues(file_exists = TRUE)

# Function to check if the file exists
check_file <- function(path) {
    if (!file.exists(path)) {
        file_info$file_exists <- FALSE
        return(invisible())
    }
    file_info$file_exists <- TRUE
    cdx_account <- read_csv(cdx_account_file)
    return(cdx_account)
    
}

# Load the account information
cdx_account_path <- file.path(Sys.getenv("USERPROFILE"), "Documents/CDX_Account")
cdx_account_file <- paste0(cdx_account_path, "/cdx-account-info.csv")
cdx_account <- check_file(cdx_account_file)



# Hydro Lab Rules -------------------------------------------------------------
hydro_lab_range_rules <- validator(
    "Temperature in valid range" = in_range(as.numeric(Temp), 0, 30), 
    "Depth in valid range" = in_range(as.numeric(Depth10), 0.01, 10), 
    "Specific Conductivity in valid range" = in_range(as.numeric(SpCond), 0.01, 1), 
    "Resistivity in valid range" = in_range(as.numeric(Res), 0.1, 5),
    "Salinity in valid range" = in_range(as.numeric(Sal), 0.01, 1), 
    "Total Dissolved Solids in valid range" = in_range(as.numeric(TDS), 0.01, 1),
    "Dissolved Oxygen saturation in valid range" = in_range(as.numeric(`DO%`), 0.01, 150), 
    "Dissolved Oxygen in valid range" = in_range(as.numeric(DO), 0, 20), 
    "pH in valid range" = in_range(as.numeric(pH), 5, 11), 
    "Turbidity in valid range" = in_range(as.numeric(Turb), 1.5, 1000),
    "Chlorophyl in valid range" = in_range(as.numeric(CHL), 0.03, 50),
    "Phycocyanin in valid range" = in_range(as.numeric(PCY), 100, 200000)
)

hydro_lab_custom_rules <- validator(
    "Location ID is Valid" = location_id %in% names(project_id_lookup),
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

#Bend Genetics Rules ----------------------------------------------------
bend_genetics_range_rules <- validator(
    "Microcystin/Nod. in valid range" = in_range(as.numeric(na.omit(`Microcycstin Nod`)), 0, 1000),
    "Microcystin in valid range" = in_range(as.numeric(na.omit(`Microcystin`)), 0, 300000),
    "Anatoxin-a in valid range" = in_range(as.numeric(na.omit(`Anatoxin-a`)), 0, 1000),
    "Cylindrospermopsin in valid range" = in_range(as.numeric(na.omit(`Cylindrospermopsin`)), 0, 1000),
    "Saxitoxin in valid range" = in_range(as.numeric(na.omit(`Saxitoxin`)), 0, 1000)
    
)
