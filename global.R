library(shiny)
library(tidyverse)
library(bslib)
library(validate)


parse_hydrolab <- function(filepath) {
    raw_name <- readLines(filepath, 1)
    location_for_selected_hydrolab <- trimws(str_replace(str_extract(str_replace_all(raw_name, '"', ""), ": .+"), ":", ""))
    read_csv(filepath,
             skip = 5,
             col_types = "c",
             col_select = c(1:28)) |>
        mutate_if(is.character, utf8::utf8_encode) |>
        select(-starts_with("...")) |>
        mutate("location_id" = location_for_selected_hydrolab) |>
        filter(str_count(Date, "\\d+") > 2) # TODO a little sus
}


hydro_lab_range_rules <- validator(
    "Temperature in valid range" = in_range(Temp, -10, 50), 
    "Depth in valid range" = in_range(Depth10, 0, 10), 
    "Specific Conductivity in valid range" = in_range(SpCond, 0, 500), 
    "Resistivity in valid range" = in_range(Res, 0, 50),
    "Salinity is positive" = in_range(Sal, 0, 5), 
    "Total Dissolved Solids in valid range" = in_range(TDS, 0, 3),
    "Dissolved Oxygen saturation in valid range" = in_range(`DO%`, 0, 200), 
    "Dissolved Oxygen in valid range" = in_range(DO, 0, 20), 
    "pH in valid range" = in_range(pH, 5, 10), 
    "Turbidity in valid range" = in_range(Turb, 0, 1000),
    "Chlorophyl in valid range" = in_range(CHL, 0, 10000)
)

hydro_lab_custom_rules <- validator(
    "Location ID is Valid" = location_id %in% c("FC1", "LPTNT")
)