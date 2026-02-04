definitions_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Parameter Definitions"),
        tags$p(class = "p-3 border rounded mb-4",
               "This page displays the valid ranges for each parameter used in QA/QC validation. 
               Values outside these ranges will be highlighted in the data tables."),
        
        tags$h4("Hydro Lab Parameters"),
        tableOutput(ns("hydro_lab_ranges")),
        
        tags$h4(class = "mt-4", "Alpha Lab Parameters"),
        tableOutput(ns("alpha_lab_ranges")),
        
        tags$h4(class = "mt-4", "Bend Genetics Parameters"),
        tableOutput(ns("bend_genetics_ranges"))
    )
}

definitions_server <- function(input, output, session) {
    
    output$hydro_lab_ranges <- renderTable({
        data.frame(
            Parameter = c(
                "Temperature, water (Temp)",
                "Depth",
                "Specific conductance (SpCond)",
                "Salinity (Sal)",
                "Total dissolved solids (TDS)",
                "Dissolved oxygen saturation (DO%)",
                "Dissolved oxygen (DO)",
                "pH",
                "Turbidity",
                "Chlorophyll a (CHL)",
                "Phycocyanin (PCY)"
            ),
            `Min Value` = c(0, 0.01, 0.01, 0.01, 0.01, 0.01, 0, 5, 1.5, 0.03, 100),
            `Max Value` = c(30, 10, 1, 1, 1, 150, 20, 11, 1000, 1000, 200000),
            Units = c("°C", "m", "mS/cm", "ppt", "g/L", "%", "mg/L", "SU", "NTU", "µg/L", "cells/mL")
        )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    output$alpha_lab_ranges <- renderTable({
        data.frame(
            Parameter = c(
                "Oil & Grease (HEM)",
                "Nitrate + Nitrite as N",
                "Phosphorus, total",
                "Total Organic Carbon",
                "Total Kjeldahl Nitrogen",
                "Fecal Coliform",
                "Total Coliform"
            ),
            `Min Value` = c(0, 0, 0, 0, 0, 0, 0),
            `Max Value` = c(3000, 10, 2, 10, 10, 300000, 300000),
            Units = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "MPN/100mL", "MPN/100mL")
        )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    output$bend_genetics_ranges <- renderTable({
        data.frame(
            Parameter = c(
                "Microcystin",
                "Anatoxin-a",
                "Cylindrospermopsin",
                "Saxitoxin"
            ),
            `Min Value` = c(0, 0, 0, 0),
            `Max Value` = c(0.8, 0.001, 0.001, 0.001),
            Units = c("µg/L", "µg/L", "µg/L", "µg/L")
        )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
}
