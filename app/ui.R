# DaisyUI Corporate theme: clean, professional, slightly rounded
corporate_theme <- bslib::bs_theme(
    version = 5,
    
    # Colors - DaisyUI Corporate palette (converted from OKLCH)
    bg = "#ffffff",
    fg = "#181a2a",
    primary = "#4b6bfb",
    secondary = "#7b92b2",
    success = "#00a96e",
    info = "#00b5ff",
    warning = "#ffbe00",
    danger = "#ff5861",
    
    # Typography (using system fonts to avoid download issues on Windows)
    base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
    heading_font = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
    font_scale = 1,
    
    # Rounded corners (corporate uses 0.25rem)
    "border-radius" = "0.25rem",
    "border-radius-sm" = "0.2rem",
    "border-radius-lg" = "0.375rem",
    "border-radius-pill" = "9999px",
    
    # Inputs and buttons
    "input-border-radius" = "0.25rem",
    "btn-border-radius" = "0.25rem",
    "btn-border-radius-sm" = "0.2rem",
    "btn-border-radius-lg" = "0.375rem",
    
    # Cards
    "card-border-radius" = "0.25rem",
    "card-cap-bg" = "#f2f2f2",
    "card-border-color" = "#e5e6e6",
    
    # Borders
    "input-border-color" = "#d4d4d4",
    "input-focus-border-color" = "#4b6bfb"
)

shinyUI(
    bslib::page_navbar(
        title = "BVR WQX Uploader",
        theme = corporate_theme,
        header = tagList(
            shinyWidgets::useSweetAlert(),
            # DaisyUI Corporate CSS polish
            tags$style(HTML("
                /* Smooth transitions */
                .btn, .form-control, .card {
                    transition: all 0.15s ease;
                }
                
                /* Button hover states */
                .btn-primary:hover {
                    background-color: #3a5bd9;
                    border-color: #3a5bd9;
                }
                
                /* Input focus ring */
                .form-control:focus {
                    box-shadow: 0 0 0 2px rgba(75, 107, 251, 0.2);
                    border-color: #4b6bfb;
                }
                
                /* Card styling - flat with subtle border */
                .card {
                    border: 1px solid #e5e6e6;
                    box-shadow: none;
                }
                
                /* Navbar styling */
                .navbar {
                    background-color: #ffffff;
                    border-bottom: 1px solid #e5e6e6;
                }
                
                .navbar-brand {
                    font-weight: 600;
                    color: #181a2a;
                }
                
                /* Table styling */
                .table {
                    border-radius: 0.25rem;
                    overflow: hidden;
                }
                
                /* DataTables */
                .dataTables_wrapper {
                    border-radius: 0.25rem;
                }
                
                div.dataTables_wrapper div.dataTables_filter input {
                    border-radius: 0.25rem;
                }
                
                /* Secondary/muted text */
                .text-muted {
                    color: #7b92b2 !important;
                }
            "))
        ),
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
    )
)