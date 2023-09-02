bslib::page_navbar(
    title = "BVR WQX Uploader",
    theme = bslib::bs_theme(preset = "cosmo"),
    navbarMenu(
        "Upload", 
        tabPanel("Hydro Lab",
                 tags$h2("Hydro Lab Data"),
                 sidebarLayout(
                     sidebarPanel(width = 3, 
                                  fileInput("hydro_lab_file", "Select Hydro Lab File", multiple = TRUE)), 
                     mainPanel(
                         tabsetPanel(
                             type = "pills", 
                             tabPanel(
                                 "Qa/Qc", 
                                 tagList(
                                     tags$p(class = "p-3 border rounded", 
                                            "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that
                                            all validations pass, and proceed to next tab when ready.")
                                 ),
                                 tableOutput("hydro_lab_table"), 
                                 tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again"), 
                                 # tags$p(tags$strong("Range Based Checks"), class = "p-3"),
                                 # tableOutput("hydro_lab_qaqc_table")
                                 
                                 layout_column_wrap(
                                     width = 1/2, 
                                     card(card_header("Range based rules"), card_body(tableOutput("hydro_lab_qaqc_table"))),
                                     card(card_header("Custom rules"), card_body(tableOutput("hydro_lab_custom_qaqc_table")))
                                 )
                                 
                                 # layout_column_wrap(
                                 #     width = 1/2,
                                 #     height = 300,
                                 #     card(full_screen = TRUE, card_header("Range based rules"), card_body(tableOutput("hydro_lab_qaqc_table"))),
                                 #     card(full_screen = TRUE, card_header("A filling map"), card_body(class = "p-0"))
                                 # )
                             ), 
                             tabPanel(
                                 "Formatted Data", 
                             )
                         )
                     )
                 )),
        tabPanel("Alpha Lab", 
                 tags$h2("Alpha Lab Data"),
                 sidebarLayout(
                     sidebarPanel(width = 3, 
                                  fileInput("alpha_lab_file", "Select Alpha Lab File")), 
                     mainPanel(
                         shiny::tabsetPanel(
                             type = "pills", 
                             tabPanel(
                                 "Qa/Qc", 
                                 tagList(
                                     tags$p("This section ")
                                 )
                             ), 
                             tabPanel(
                                 "Formatted Data"
                             )
                         )
                     ))),
        tabPanel("Bend Genetics", 
                 tags$h2("Bend Gentics Data"),
                 sidebarLayout(
                     sidebarPanel(width = 3, 
                                  fileInput("bendgenetics_file", "Bend Genetics File")), 
                     mainPanel(
                         shiny::tabsetPanel(
                             type = "pills", 
                             tabPanel(
                                 "Qa/Qc", 
                                 tagList(
                                     tags$p("This section ")
                                 )
                             ), 
                             tabPanel(
                                 "Formatted Data"
                             )
                         )
                     ))
        )
        
    )
)