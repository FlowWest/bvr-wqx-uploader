message("checking to see what needs to be installed....")
#check for loaded packages via session_info()
deps <- c("shinycssloaders", 
          "shinyWidgets",
          "spsComps",
          "validate",
          "bslib",
          "lubridate",
          "forcats",
          "stringr",
          "dplyr",
          "purrr",
          "readr",
          "tidyr",
          "tibble",
          "ggplot2",
          "tidyverse",
          "shiny",
          "remotes",
          "DT",
          "emo",
          "httr2",
          "digest",
          "base64enc",
          "jsonlite",
          "readxl"
          )

installed_deps <- installed.packages()

for (dep in deps){
  if (dep %in% installed_deps){
    next
  }else {
      if(dep == "emo"){
          message(paste("installing:", dep))
          remotes::install_github("hadley/emo")
      }else{
          message(paste("installing:", dep))
          install.packages(dep, repos='https://cloud.r-project.org')  
      }
  }
}
