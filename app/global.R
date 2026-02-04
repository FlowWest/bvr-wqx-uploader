library(shiny)
library(tidyverse)
library(bslib)
library(spsComps)
library(shinyWidgets)
library(shinycssloaders)
library(readxl)
library(httr2)
library(digest)
library(base64enc)
library(jsonlite)

load("../data/lookup_objects.rdata")

# CDX API client (pure R, no Python dependency)
source("cdx-api.R")

source("modules/hydro_lab_module.R")
source("modules/bend_genetics_module.R")
source('modules/alpha_lab_module.R')
source('modules/user_account_module.R')
source('modules/wqx_upload_module.R')
source('modules/definitions_module.R')

source("hydro-lab.R")
source("alpha-lab.R")
# source("bend-genetics.R")
# source("new-bend-genetics.R")
source('bend-genetics-macro.R')

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

# Load the account information (cross-platform: works on Windows and macOS)
home_dir <- Sys.getenv("HOME")
if (home_dir == "") home_dir <- Sys.getenv("USERPROFILE")
if (home_dir == "") home_dir <- path.expand("~")
cdx_account_path <- file.path(home_dir, "Documents", "CDX_Account")
cdx_account_file <- file.path(cdx_account_path, "cdx-account-info.csv")
cdx_account <- check_file(cdx_account_file)

# Default download folder (used if not specified in config)
default_download_folder <- file.path(home_dir, "Downloads")

