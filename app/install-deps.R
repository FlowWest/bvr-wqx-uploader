message("checking to see what needs to be installed....")
#check for loaded packages via session_info()
deps <- c("shinycssloaders", 
          "shinyWidgets",
          "spsComps",
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
      if (dep == "httr2") {
          current_version <- packageVersion("httr2")
          if (current_version != "1.2.2") {
              message("Reinstalling httr version 1.2.2 (current: ", current_version, ")")
              install.packages(
                  "httr2",
                  repos = "https://cloud.r-project.org"
              )
          }
      }
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
