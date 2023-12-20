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