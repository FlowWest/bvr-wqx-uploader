
shinyUI(
    bslib::page_navbar(
        title = "BVR WQX Uploader",
        theme = bslib::bs_theme(preset = "cosmo"),
        header = shinyWidgets::useSweetAlert(),
        navbarMenu(
            "Upload",
            tabPanel(title = 'Hydro Lab',
                     hydro_lab_ui('hydro_lab')),
            tabPanel(title = 'Bend Genetics',
                     bend_genetics_ui('bend_genetics')),
            tabPanel(title = 'Alpha Lab',
                     alpha_lab_ui('alpha_lab'))
        ),
        tabPanel(title = "User Account",
                 user_account_ui('user_account'))
))
    
#     navbarMenu(
#         "Upload", 
#         tabPanel("Hydro Lab",
#                  tags$h2("Hydro Lab Data"),
#                  sidebarLayout(
#                      sidebarPanel(width = 3, 
#                                   fileInput("hydro_lab_file", "Select Hydro Lab File", multiple = TRUE),
#                                   actionButton("reset", "Reset"),
#                                   conditionalPanel(condition="input.tabs == 'additional'",
#                                       selectInput("selected_location", "Select Monitoring Location:", choices = NULL),
#                                       selectInput("selected_day", "Select Monitoring Day:", choices = NULL),
#                                       numericInput("temperature_air", "Enter Air Temperature Measurement", value = ""),
#                                       textAreaInput("result_comment", "Enter Result Comment", rows = 2),
#                                       actionButton("add_result", "Add Result"),
#                                       actionButton("delete_result", "Delete Last Added Result")
#                                       )
#                                     ),
#                      mainPanel(
#                          tabsetPanel(
#                              id = "tabs",
#                              type = "pills", 
#                              tabPanel(
#                                  "Qa/
#                                  Qc",
#                                  value = "qa_qc",
#                                  tagList(
#                                      tags$p(class = "p-3 border rounded", 
#                                             "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that
#                                             all validations pass, and proceed to next tab when ready. Click on 'Reset' to clear all saved data and values in application.")
#                                  ),
#                                  card(card_header("Raw Data"), card_body(DT::dataTableOutput("hydro_lab_table"))), 
#                                  tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - 'O', - test passed, ', 'X' - test failed, '!' - verify manually (usually safe to ignore)"), 
#                                  
#                                  layout_column_wrap(
#                                      width = 1/2, 
#                                      card(card_header("Range based rules"), card_body(tableOutput("hydro_lab_qaqc_table"))),
#                                      card(card_header("Custom rules"), card_body(tableOutput("hydro_lab_custom_qaqc_table")))
#                                  )
#                                  
#                                  
#                              ),
#                              tabPanel(
#                                  "Enter Additional Data",
#                                  value = "additional",
#                                  tags$p(class = "p-3 border rounded", 
#                                         "Enter additional AccuWeather 'Temperature, Air' measurement and 'Result Comment' for each date and location in the sidebar panel."),
#                                  DT::dataTableOutput("temperature_data_table"),
#                                  actionButton("generate_formatted_df", "Generate WQX Ready Data"),
#                                  textOutput("check_df_message"),
#                                  tags$p(class = "p-3 border rounded", 
#                                         "If water body is too shallow, click on button below to generate empty dataframe after inputing air temperature and result comment."),
#                                  actionButton("generate_df", "Generate Empty WQX Data Sheet"),
#                                  textOutput("check_empty_df_message"),
#                                  
#                              ),
#                              tabPanel(
#                                  "Formatted Data", 
#                                  tags$p(class = "p-3 border rounded", 
#                                         "Review WQX formatted data. Click 'Download' and then 'Upload to WQX' when ready."), 
#                                  bslib::layout_columns(
#                                      col_widths = c(2, 2, 2),
#                                      downloadButton("hydro_lab_download"),
#                                      actionButton("hydro_lab_upload", label = "Upload to WQX", icon = shiny::icon("rocket")),
#                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                                                       tags$div(HTML("<b> Starting WQX upload. Please wait 25 seconds for the upload status from CDX...</b>"),id="loadmessage")),
#                                      uiOutput("hydro_upload_status")
#                                  ),
#                                  # DT::dataTableOutput("hydro_lab_wqx_formatted")
#                                  tableOutput("hydro_lab_wqx_formatted")
#                              )
#                          )
#                      )
#                  )),
#         
#         # ALPHA LAB -------------------------------------------------------------------------------------------
#         tabPanel("Alpha Lab", 
#                  tags$h2("Alpha Lab Data"),
#                  sidebarLayout(
#                      sidebarPanel(width = 3, 
#                                   fileInput("alpha_lab_file", "Select Alpha Lab File")), 
#                      mainPanel(
#                          tabsetPanel(
#                              type = "pills", 
#                              tabPanel(
#                                  "Qa/Qc", 
#                                  tagList(
#                                      tags$p(class = "p-3 border rounded", 
#                                             "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that all validations pass, and proceed to next tab when ready."),
#                                      tableOutput("alpha_lab_table"), 
#                                      tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - ", emo::ji("check"), "- test passed, ", emo::ji("x"), "- test failed", emo::ji("warning"), "-verify manually (usually safe to ignore)"), 
#                                      
#                                      layout_column_wrap(
#                                          width = 1/2, 
#                                          card(card_header("Range based rules"), card_body(tableOutput("alpha_lab_qaqc_table"))),
#                                          card(card_header("Custom rules"), card_body(tableOutput("alpha_lab_custom_qaqc_table")))
#                                      )
#                                      
#                                      
#                                  )), 
#                              tabPanel(
#                                  "Formatted Data", 
#                                  tags$p(class = "p-3 border rounded", 
#                                         "Data Formatted to WQX, review and when ready select Download and Upload to WQX"),
#                                  bslib::layout_columns(
#                                      col_widths = c(2, 2, 2),
#                                      downloadButton("alpha_lab_download"),
#                                      actionButton("alpha_lab_upload", label = "Upload to WQX", icon = shiny::icon("rocket")),
#                                      uiOutput("alpha_lab_status")
#                                  ),
#                                  
#                                  tableOutput("alpha_lab_wqx_formatted")
#                              )
#                              
#                          )))
#         ),
#         
#         # BEND GENETICS ------------------------------------------------------------------------------
#         tabPanel("Bend Genetics", 
#                  tags$h2("Bend Gentics Data"),
#                  sidebarLayout(
#                      sidebarPanel(width = 3, 
#                                   fileInput("bend_genetics_file", "Bend Genetics File")), 
#                      mainPanel(
#                          shiny::tabsetPanel(
#                              type = "pills", 
#                              tabPanel(
#                                  "Qa/Qc", 
#                                  tagList(
#                                      tags$p(class = "p-3 border rounded", 
#                                             "This section provides view of raw data, as well as results for Qa/Qc checks. Verify that all validations pass, and proceed to next tab when ready."),
#                                      tableOutput("bend_genetics_table"), 
#                                      tags$p(class = "p-3 border rounded", "Qa/Qc Results: check for failed test, make changes in the raw data and try to import again. The following icons are used - ", emo::ji("check"), "- test passed, ", emo::ji("x"), "- test failed", emo::ji("warning"), "-verify manually (usually safe to ignore)"), 
#                                      
#                                      layout_column_wrap(
#                                          width = 1/2, 
#                                          card(card_header("Range based rules"), card_body(tableOutput("bend_genetics_qaqc_table"))),
#                                          card(card_header("Custom rules"), card_body(tableOutput("bend_genetics_custom_qaqc_table")))
#                                      )
#                                      
#                                      
#                                  )
#                              ), 
#                              tabPanel(
#                                  "Formatted Data",
#                                  tags$p(class = "p-3 border rounded", 
#                                         "Data Formatted to WQX, review and when ready select Download and Upload to WQX"),
#                                  bslib::layout_columns(
#                                      col_widths = c(2, 2, 2),
#                                      downloadButton("bend_genetics_download"),
#                                      actionButton("bend_genetics_upload", label = "Upload to WQX", icon = shiny::icon("rocket")),
#                                      uiOutput("bend_genetics_status") |> withSpinner(color="#0dc5c1")
#                                  ),
#                                  
#                                  tableOutput("bend_genetics_wqx_formatted")
#                              )
#                          )
#                      ))
#         )
#     ), 
#     
#     # Account ------------------------------------------------------------------------------------
#     tabPanel("User Account", 
#              tags$h2("Manage WQX Credentials"),
#              sidebarLayout(
#                  sidebarPanel(width = 4,
#                               selectInput("wqx_username", "Username", choices = cdx_account$USER_ID), 
#                               selectInput("wqx_api_key", "API Key", choices = cdx_account$WQX_API_KEY),
#                               selectInput("wqx_config_id", "Config ID", choices = cdx_account$CONFIG_ID),
#                               ),
#             mainPanel( 
#                 uiOutput("error_message"),
#                 br(),
#                 actionButton("load_credential", "Load Credential")
# 
#             ) 
#         )
#              
#     )
# )

