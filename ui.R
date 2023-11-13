packages <- c("shiny", "shinymaterial", "shinyjs", "dplyr", "lubridate", "Hmisc", "data.table", 
              "stringr", "readr", "DT", "DHRInternal")

sapply(packages, require, character.only = TRUE)

options(shiny.maxRequestSize = 30*1024^3)

source("./functions/helpers.R") # Load all the code needed to show feedback on a button click
source("./functions/how_to.R")
source("./functions/analysis_tab.R")
source("./functions/monitor_tab.R")
source("./functions/DataCleaningScript.R")

# Wrap shinymaterial apps in material_page
material_page(
  useShinyjs(),
  title = "Career PathFinder Data Cleaner",
  
  nav_bar_color = "deep-orange",
  background_color = "white",
  nav_bar_fixed = TRUE,
  
  # Side-nav in the beginning of the UI
  material_side_nav(
    fixed = TRUE,
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "How to Use" = "how_nav_tab",
        "Input/Output" = "output_nav_tab",
        "Monitor" = "monitor_tab",
        "User Data Analysis" = "analysis_nav_tab"
      ),
      icons = c("info_outline", "import_export", "desktop_windows", "insert_chart")
    )
  ),
  # CONTENT tab 1
  material_side_nav_tab_content(
    side_nav_tab_id = "how_nav_tab",
    
    # How To Content Goes Here
    material_row(
      material_card(
        depth = 0,
        tags$div(
          how_to_content()
        )
      )
    )
  ),
  # CONTENT tab 2
  material_side_nav_tab_content(
    side_nav_tab_id = "output_nav_tab",
    
    material_card(
      depth = 0,
      tags$h3("Data for Career PathFinder"),
      
      tags$h6("Upload the required data using the button below."),
      
      material_modal(
        modal_id = "example_modal",
        button_text = "Upload",
        floating_button = FALSE,
        button_icon = "attach_file",
        # button_color = "deep-orange",
        title = "Upload All Files Before Cleaning",
        tags$div(
          # Modal Content Goes Here
          material_tabs(
            # color = "deep-orange",
            tabs = c(
              "Title Reference" = "title_tab",
              "PPRT" = "pprt_tab",
              "JPACT" = "jpact_tab"
            )
          ),
          material_tab_content(
            tab_id = "title_tab",
            fileInput("import_title", "Import Title Reference",
                      accept = c("text/plain") ),
            uiOutput("titlePath")
          ),
          material_tab_content(
            tab_id = "pprt_tab",
            fileInput("import_pprt", "Import PPRT",
                      accept = c("text/plain")),
            uiOutput("pprtPath")
          ),
          material_tab_content(
            tab_id = "jpact_tab",
            fileInput("import_jpact", "Import JPACT",
                      accept = c("text/plain")),
            uiOutput("jpactPath"),
            textOutput("my_file_name")
          ),
          br(),
          
          div(class = "center-align",
              tags$h6("Once all data is uploaded, you may click the button below to clean and prep the data."),
              
              tags$style(appCSS),
              withBusyIndicatorUI(
                actionButton("clean_data", " Clean Data", class = "btn-primary")
              )
          ),
          uiOutput("test")
        )
      ),
      
      # br(),
      # 
      # tags$h6("The updated item pairs will be placed in the 'data' sub-folder when all the data cleaning rules have been applied. 
      #         Click the link on this page to open the folder."),
      tags$br(),
      tags$h6("Do not open the csv files before uploading them to the Career PathFinder admin site. Opening them causes leading
              zeroes to disappear and will prevent the tool from working correctly. If you need to open the csv files, open them after
              uploading them or just make a copy of them on your desktop.")
      # actionLink("openData", "Open Folder"),
      
      # tableOutput("testTable")
    )
    
  ),
  # CONTENT tab 3
  material_side_nav_tab_content(
    side_nav_tab_id = "monitor_tab",
    
    material_card( depth = 0,
                   tags$div(
                     monitor_tab_content()  
                   )),
    
    material_tabs(
      # color = "deep-orange",
      tabs = c(
        "30 Year Data" = "data_30",
        "15 Year Data" = "data_15") ),
    material_tab_content(
      tab_id = "data_30",
      ## UI html content here
      material_card( depth = 0,
                     tags$div(
                       tags$h4("Overview"),
                       material_row(
                         material_column( width = 6,
                                          DTOutput("monitor_30_tbl") ) ),
                       tags$br(),
                       tags$h4("Missing Values"),
                       tags$h6("The first 7 columns should always have 0% missing values.
                               If there's missing values in one of these, inspect the data."),
                       plotOutput("monitor_30_missing"),
                       tags$h4("Distribution of Values"),
                       tags$h6("Only the continuous variables are plotted. This shows you the range
                               of values for each column. If any values seem unusual for a column,
                               inspect the data further."),
                       plotOutput("monitor_30_histogram")
                     )
      )
    ),
    material_tab_content(
      tab_id = "data_15",
      ## UI html content here
      material_card( depth = 0,
                     tags$div(
                       tags$h4("Overview"),
                       material_row(
                         material_column( width = 6,
                                          DTOutput("monitor_15_tbl") ) ),
                       tags$br(),
                       tags$h4("Missing Values"),
                       tags$h6("The first 7 columns should always have 0% missing values.
                               If there's missing values in one of these, inspect the data."),
                       plotOutput("monitor_15_missing"),
                       tags$h4("Distribution of Values"),
                       tags$h6("Only the continuous variables are plotted. This shows you the range
                               of values for each column. If any values seem unusual for a column,
                               inspect the data further."),
                       plotOutput("monitor_15_histogram")
                     )
      )
    )
  ),
  # CONTENT tab 4
  material_side_nav_tab_content(
    side_nav_tab_id = "analysis_nav_tab",
    
    material_tabs(
      # color = "deep-orange",
      tabs = c(
        "Instructions" = "instructions",
        "Format Data" = "format") ),
    material_tab_content(
      tab_id = "instructions",
      ## UI html content here
      material_card( depth = 0,
                     tags$div(
                       analysis_tab_content()
                     )
      )
    ),
    material_tab_content(
      tab_id = "format",
      ## UI html content here
      material_card( depth = 0,
                     tags$div(
                       tags$h4("Format Data from Word Press"),
                       tags$p("Upload the csv file that you downloaded from the admin site, then
                              click the 'Format User Data' button.")
                     ),
                     material_file_input("uploadUserData", "Upload User Data"),
                     actionButton("clean_export", "Format User Data"),
                     
                     uiOutput("analysis_output_UI")
      )
    )
  )
)