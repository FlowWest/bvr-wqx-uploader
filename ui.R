bslib::page_navbar(
    title = "BVR WQX Uploader",
    theme = bslib::bs_theme(preset = "cosmo"),
    header = shinyWidgets::useSweetAlert(),
    
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
                                 "Qa/
                                 Qc", 
                                 tagList(
                                     tags$p(class = "p-3 border rounded", 
                                            "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that
                                            all validations pass, and proceed to next tab when ready.")
                                 ),
                                 tableOutput("hydro_lab_table"), 
                                 tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - ", emo::ji("check"), "- test passed, ", emo::ji("x"), "- test failed", emo::ji("warning"), "-verify manually (usually safe to ignore)"), 
                                 # tags$p(tags$strong("Range Based Checks"), class = "p-3"),
                                 # tableOutput("hydro_lab_qaqc_table")
                                 
                                 layout_column_wrap(
                                     width = 1/2, 
                                     card(card_header("Range based rules"), card_body(tableOutput("hydro_lab_qaqc_table"))),
                                     card(card_header("Custom rules"), card_body(tableOutput("hydro_lab_custom_qaqc_table")))
                                 )
                                 
                                 
                             ), 
                             tabPanel(
                                 "Formatted Data", 
                                 tags$p(class = "p-3 border rounded", 
                                        "Data Formatted to WQX, review and when ready select Download and Upload to WQX"), 
                                 bslib::layout_columns(
                                     col_widths = c(2, 2, 2),
                                     downloadButton("hydro_lab_download"),
                                     actionButton("hydro_lab_upload", label = "Upload to WQX", icon = shiny::icon("rocket")),
                                     uiOutput("hydro_upload_status")
                                 ),
                                 tableOutput("hydro_lab_wqx_formatted")
                             )
                         )
                     )
                 )),
        
        # ALPHA LAB -------------------------------------------------------------------------------------------
        tabPanel("Alpha Lab", 
                 tags$h2("Alpha Lab Data"),
                 sidebarLayout(
                     sidebarPanel(width = 3, 
                                  fileInput("alpha_lab_file", "Select Alpha Lab File")), 
                     mainPanel(
                         tabsetPanel(
                             type = "pills", 
                             tabPanel(
                                 "Qa/Qc", 
                                 tagList(
                                     tags$p(class = "p-3 border rounded", 
                                            "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that all validations pass, and proceed to next tab when ready."),
                                     tableOutput("alpha_lab_table"), 
                                     tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - ", emo::ji("check"), "- test passed, ", emo::ji("x"), "- test failed", emo::ji("warning"), "-verify manually (usually safe to ignore)"), 
                                     
                                     layout_column_wrap(
                                         width = 1/2, 
                                         card(card_header("Range based rules"), card_body(tableOutput("alpha_lab_qaqc_table"))),
                                         card(card_header("Custom rules"), card_body(tableOutput("alpha_lab_custom_qaqc_table")))
                                     )
                                     
                                     
                                 )), 
                             tabPanel(
                                 "Formatted Data", 
                                 tags$p(class = "p-3 border rounded", 
                                        "Data Formatted to WQX, review and when ready select Download and Upload to WQX"),
                                 bslib::layout_columns(
                                     col_widths = c(2, 2, 2),
                                     downloadButton("alpha_lab_download"),
                                     actionButton("alpha_lab_upload", label = "Upload to WQX", icon = shiny::icon("rocket")),
                                     uiOutput("alpha_lab_status")
                                 ),
                                 
                                 tableOutput("alpha_lab_wqx_formatted")
                             )
                             
                         )))
        ),
        
        # BEND GENETICS ------------------------------------------------------------------------------
        tabPanel("Bend Genetics", 
                 tags$h2("Bend Gentics Data"),
                 sidebarLayout(
                     sidebarPanel(width = 3, 
                                  fileInput("bend_genetics_file", "Bend Genetics File")), 
                     mainPanel(
                         shiny::tabsetPanel(
                             type = "pills", 
                             tabPanel(
                                 "Qa/Qc", 
                                 tagList(
                                     tags$p(class = "p-3 border rounded", 
                                            "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that all validations pass, and proceed to next tab when ready."),
                                     tableOutput("bend_genetics_table"), 
                                     tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - ", emo::ji("check"), "- test passed, ", emo::ji("x"), "- test failed", emo::ji("warning"), "-verify manually (usually safe to ignore)"), 
                                     
                                     layout_column_wrap(
                                         width = 1/2, 
                                         card(card_header("Range based rules"), card_body(tableOutput("bend_genetics_qaqc_table"))),
                                         card(card_header("Custom rules"), card_body(tableOutput("bend_genetics_custom_qaqc_table")))
                                     )
                                     
                                     
                                 )
                             ), 
                             tabPanel(
                                 "Formatted Data",
                                 tags$p(class = "p-3 border rounded", 
                                        "Data Formatted to WQX, review and when ready select Download and Upload to WQX"),
                                 bslib::layout_columns(
                                     col_widths = c(2, 2, 2),
                                     downloadButton("bend_genetics_download"),
                                     actionButton("bend_genetics_upload", label = "Upload to WQX", icon = shiny::icon("rocket")),
                                     uiOutput("bend_genetics_status") |> withSpinner(color="#0dc5c1")
                                 ),
                                 
                                 tableOutput("bend_genetics_wqx_formatted")
                             )
                         )
                     ))
        )
    ), 
    
    # Account ------------------------------------------------------------------------------------
    tabPanel("User Account", 
             tags$h2("Manage WQX Credentials"),
             sidebarLayout(
                 sidebarPanel(width = 5, 
                              textInput("wqx_username", "Username", value = cdx_account$USER_ID), 
                              textInput("wqx_api_key", "API Key", value = cdx_account$WQX_API_KEY),
                              textInput("wqx_config_id", "Config ID", value = cdx_account$CONFIG_ID)
                 ), 
                 mainPanel()
             )
             
    )
)

