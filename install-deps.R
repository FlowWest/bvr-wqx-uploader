library(shiny)
library(tidyverse)
library(bslib)
library(validate)
library(wqxWeb)
library(reticulate)
library(spsComps)
library(shinyWidgets)
library(shinycssloaders)


message("checking to see what needs to be installed....")
deps <- c("shiny", "tidyverse", "bslib", "validate", "reticulate", "spsComps","shinyWidgets", "shinycssloaders")
installed_deps <- installed.packages()

for (dep in deps){
  if (dep %in% installed_deps){
    next
  }else {
    message(paste("installing:", dep))
    install.packages(dep, repos='https://cloud.r-project.org')
  }
}