packages <- c("shiny", "shinymaterial", "shinyjs", "dplyr", "lubridate", "Hmisc", "data.table", 
              "stringr", "readr", "DT", "DHRInternal", "DataExplorer")

sapply(packages, require, character.only = TRUE)

options(shiny.maxRequestSize = 30*1024^3)

source("./functions/helpers.R") # Load all the code needed to show feedback on a button click
source("./functions/how_to.R")
source("./functions/analysis_tab.R")
source("./functions/DataCleaningScript.R")

function(input, output, session) {
  
  
  output$jpactPath <- renderUI({
    
    # Check file name
    if ( !is.null(input$import_jpact$datapath) ) {
      
      if( grepl("JPACT", basename(input$import_jpact$name) ) ) {
        
        tagList(
          HTML(paste("Upload of JPACT file was ", tags$span(style="color:green", "successful!"), sep = ""))
        )
      } else {
        shinyjs::alert("You tried uploading the wrong file. Please upload the JPACT text file.")
      }
    } else {
      tagList(
        HTML( paste("Waiting for ", tags$span(style="color:red", "JPACT file"), sep = "") )
      )
    }
    
  })
  
  # onclick("openData", alert("Remember, don't open the csv files before uploading to the admin site!"))
  
  output$pprtPath <- renderUI({
    
    # Check file name
    if ( !is.null(input$import_pprt$datapath) ) {
      
      if( grepl("PAY_POLICY_RATE", basename(input$import_pprt$name) ) ) {
        
        tagList(
          HTML(paste("Upload of PPRT file was ", tags$span(style="color:green", "successful!"), sep = ""))
        )
      } else {
        shinyjs::alert("You tried uploading the wrong file. Please upload the PAY POLICY RATE (PPRT) text file.")
      }
    } else {
      tagList(
        HTML( paste("Waiting for ", tags$span(style="color:red", "PPRT file"), sep = "") )
      )
    }
    
  })
  
  output$titlePath <- renderUI({
    
    # Check file name
    if ( !is.null(input$import_title$datapath) ) {
      
      if( grepl("TITLE_Reference_Extract", basename(input$import_title$name) ) ) {
        
        tagList(
          HTML(paste("Upload of Title Reference file was ", tags$span(style="color:green", "successful!"), sep = ""))
        )
      } else {
        shinyjs::alert("You tried uploading the wrong file. Please upload the TITLE Reference text file.")
      }
    } else {
      tagList(
        HTML( paste("Waiting for ", tags$span(style="color:red", "Title Reference file"), sep = "") )
      )
    }

  })
  
  # Read the Word Press Admin Export Data
  user_data_file <- reactive({
    validate(need(input$uploadUserData, message = FALSE))
    input$uploadUserData
  })
  
  # Enable the button to clean export data
  observe({
    if ( !is.null( input$uploadUserData ) ) {
      enable("clean_export")
    } else {
      disable("clean_export")
    }
  })
  
  # All data can be used for to find how many paths started and printed, etc.
  all_user_data <- reactive({
    events <- read_csv( user_data_file()$datapath ) %>%
      filter( !Time < mdy("05012019") )
  })
  
  # This events data is cleaned up and only concerned with 'Printed' paths
  events <- eventReactive(input$clean_export, {
    events <- all_user_data() %>% 
      mutate(Path = lag(`Full Event Url`)) %>% 
      mutate(PathDirection = ifelse( as.character(`Job Id`) == str_sub(Path, start = -4), "Forward", "Reverse")) %>%
      filter(`Event Type` == "print") %>% 
      filter(Path != "http://pathfinder.devcp.lacounty.gov/wp-json/careerpathfinder/v1/event?type=print") %>%
      select(Time, `User Session Id`, PathDirection, Path)
    
    events <- events %>%
      mutate(Path_Stripped = str_replace(events$Path, 
                                         "http\\:\\/\\/pathfinder\\.devcp\\.lacounty\\.gov\\/wp\\-json\\/careerpathfinder\\/v1\\/event\\?type\\=step\\?steps\\=", "")) 
    
    events <- events %>%
      mutate(Path_Stripped = str_replace(events$Path_Stripped,
                                         "http\\:\\/\\/pathfinder\\.devcp\\.lacounty\\.gov\\/wp\\-json\\/careerpathfinder\\/v1\\/event\\?type\\=start\\?steps\\=", "")) %>%
      select(Time, `User Session Id`, PathDirection, Path_Stripped)
    
    paths_df <- as.data.frame( str_split(events$Path_Stripped, '\";\"', simplify = TRUE) ) 
    
    paths_df <- as.data.frame( mapply(str_pad, paths_df, MoreArgs = list(width = 4, side = "left", pad = "0")) )
    
    paths_df <- as.data.frame( mapply(gsub, paths_df, MoreArgs = list(pattern = "0000", replacement = NA)) )
    
    paths_df$NumSteps <- as.data.frame( t( !apply(paths_df, 1, is.na) ) ) %>% rowSums(na.rm = TRUE, dims = 1)
    
    names(paths_df) <- c(paste("Title", 1:5), "NumSteps")
    
    paths_df$`Title 1` <- str_extract(paths_df$`Title 1`, 
                                      pattern = "=[0-9]{1,6}") %>% 
      str_extract(pattern = "[0-9]{1,6}") %>% 
      str_pad(width = 4, side = "left", pad = "0")
    
    events <- cbind(events, paths_df)
    
    events <- events %>% select(-Path_Stripped)
    
    events
    
  })  
  
  output$analysis_output_UI <- renderUI({
    printed_avgSteps <- round( mean( events()$NumSteps, na.rm = TRUE), 2 )
    printed_totalPaths <- nrow( events() )
    
    printed_direction <- events() %>% 
      filter(NumSteps > 1) %>% 
      group_by(PathDirection) %>% 
      summarise(Count = n() ) %>% 
      ungroup() %>% 
      mutate(Percent = round( Count/sum(Count)*100, 2) )
    
    starts <- all_user_data() %>% 
      mutate(Path = lag(`Full Event Url`)) %>% 
      # ADD HERE #  extract last 4 chars from event URL and compare to the JOB ID column on the next line. If same, it is a reverse path? Otherwise departure
      mutate(PathDirection = ifelse( as.character(`Job Id`) == str_sub(Path, start = -4), "Forward", "Reverse")) %>%
      filter(`Event Type` == "start")
    
    help <- nrow( all_user_data() %>% filter(`Event Type` == "help") )
    
    
    tagList(
      material_row(
        
        material_column( width = 4,
                         DT::datatable( events(),
                                        options = list(dom = 'ftp') )
        ),
        material_column( width = 8 )
      ),
      material_row(
        downloadButton("downloadCleanedExport")
      ),
      
      br(),
      
      material_row(
        material_column( width = 4,
                         
                         DT::datatable( rbind(printed_avgSteps, 
                                              format(printed_totalPaths, big.mark = ","), 
                                              format(nrow(starts), big.mark = ",")),
                                        options = list( dom = 't'),
                                        width = "300px",
                                        colnames = c("", ""),
                                        rownames = c("Printed Paths Avg. Steps", "Total Paths Printed", "Total Paths Started"),
                                        caption = "Statistics for Printed Career Paths")
        ),
        material_column( width = 4, offset = 1,
                         material_card( renderText( c(format(help, big.mark = ","), 
                                                      "visitors have used the 'Take-a-Tour' button")
                         ) )
        )
      )
    )
  })
  
  output$downloadCleanedExport <- downloadHandler(
    filename = function() {
      paste('events', Sys.Date(), format( Sys.time(), "%I%M %p"), '.csv', sep = ' ')
    },
    content = function(con) {
      write_csv(events(), con)
    }
  )
  
  # Read the JPACT file and show the user it has been read correctly
  jpact_file <- reactive({
    validate(need(input$import_jpact, message = FALSE))
    input$import_jpact
  })
  
  jpact_data <- reactive({
    
    job_specs <- DHRInternal::import_specs("JPACT", 
                                           keep = c("EMPLOYEE_ID", "APPOINTMENT_ID",
                                                    "TITLE_CD", "SUB_TITLE_CD", 
                                                    "HOME_DEPT_CD", "EMPLMT_STA_CD", 
                                                    "EFFECTIVE_DT", "EXPIRATION_DT", 
                                                    "PERS_ACTN_CD", "JOB_APPT_DT") )
    
    adv <- data.table::setDT(readr::read_fwf( jpact_file()$datapath , skip = 1,
                                              progress = FALSE,
                                              col_types = paste(rep("c", length(job_specs$FieldName)), collapse = ""), # new
                                              fwf_positions(job_specs$Start,
                                                            job_specs$End,
                                                            col_names = job_specs$FieldName)))
    
    as.data.frame(adv) 
    
  })
  
  jpact_file_date <- reactive({
    
    data.table::setDT(readr::read_fwf( jpact_file()$datapath , n_max = 1,
                                       progress = FALSE,
                                       fwf_positions(job_specs$Start,
                                                     job_specs$End,
                                                     col_names = job_specs$FieldName))) %>%
      select(1) %>%
      mdy()
    
  })
  
  title_file <- reactive({
    validate(need(input$import_title, message = FALSE))
    input$import_title
  })
  
  title_data <- reactive({
    
    title_specs <- DHRInternal::import_specs("Title")
    
    title <- data.table::setDT( readr::read_fwf( title_file()$datapath,
                                                 progress = FALSE,
                                                 col_types = paste(rep("c", length(title_specs$FieldName)), collapse = ""), # new
                                                 fwf_positions(title_specs$Start,
                                                               title_specs$End,
                                                               col_names = title_specs$FieldName)) %>%
                                  filter(ExpirationDate == "12319999")
    )
    
    as.data.frame(title)
    
  })
  
  pprt_file <- reactive({
    validate(need(input$import_pprt, message = FALSE))
    input$import_pprt
  })
  
  pprt_data <- reactive({
    
    pprt_specs <- DHRInternal::import_specs("PayPolicyRate", 
                                            keep = c("PayPolicy", "Step", "ExpirationDate", "PayRateAmount"))
    
    salary <- data.table::setDT(readr::read_fwf( pprt_file()$datapath,
                                                 progress = FALSE,
                                                 col_types = "cccd", # new
                                                 fwf_positions(pprt_specs$Start,
                                                               pprt_specs$End,
                                                               col_names = pprt_specs$FieldName)))
    
    as.data.frame(salary)
    
  })
  
  # Enable the button to clean data
  observe({
    if ( !is.null( input$import_jpact ) & !is.null( input$import_title ) & !is.null( input$import_pprt )  ) {
      enable("clean_data")
    } else {
      disable("clean_data")
    }
  })
  
  observe({
    onclick("clean_data", hide("clean_data"))
  })
  
  # Hide/Show download button
  
  # observeEvent(input$clean_data, {
  # shinyjs::show("download_CP_data")
  # withBusyIndicatorServer("clean_data", {
  #   Sys.sleep(1)
  # })
  # })
  
  # Run DataCleaningScript.R
  # observeEvent(input$clean_data, {
  #   source("./functions/DataCleaningScript.R")
  #   dataCleaning( jpact_data(), pprt_data(), title_data() )
  # })
  
  datasetsOutput <- eventReactive(input$clean_data, {
    dat <- dataCleaning( jpact_data(), pprt_data(), title_data() )
    return( dat )
  })
  
  observeEvent(input$clean_data, {
    
    # New Hires/Promos counts are for Career PathFinder stats page updates
    start <- jpact_file_date() %m-% months(6, FALSE)
    rptMonth <- lubridate::interval( start, jpact_file_date() )
    
    nh_by_class <- jpact_data() %>%
      dplyr::filter( is.na(APPOINTMENT_ID) ) %>%
      dplyr::filter(SUB_TITLE_CD %in% c("A", "D", "N", "L") &
                      HOME_DEPT_CD %nin% c("GJ", "NL", "SC") &
                      PERS_ACTN_CD %in% c("01", "44", "44A", "44B") &
                      mdy(EFFECTIVE_DT) %within% rptMonth ) %>%
      dplyr::distinct(EMPLOYEE_ID, PERS_ACTN_CD, TITLE_CD, .keep_all = TRUE)
    
    withBusyIndicatorServer("clean_data", {
      # material_spinner_show(session, "test")
      
      insertUI(selector = "#clean_data", where = "afterEnd",
               ui = tagList(
                 tags$br(),
                 downloadButton("download_CP_data"),
                 tags$p("Your datasets, such as ",
                        toupper( names( datasetsOutput() )[1] ),
                        "are ready for download."),
                 tags$br(),
                 tags$p("There were ", format( nrow(nh_by_class), big.mark = ",") , 
                        "new hires and promotions in the last 6 months.")
               )
      )
      
      # material_spinner_hide(session, "test")
      
    })
    
  })
  
  observeEvent(input$clean_data, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("clean_data", {
      Sys.sleep(3)
    })
  })
  
  # datasetInput <- reactive({
  #   return(list(rock=rock, pressure=pressure, cars=cars))
  # })
  
  output$download_CP_data <- downloadHandler(
    filename = 'item_pairs.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("item_pairs_15.csv", "item_pairs_15_rev.csv", 
              "item_pairs_30.csv", "item_pairs_30_rev.csv")
      write_csv(datasetsOutput()$item_pairs_15, file = "item_pairs_15.csv", na = "")
      write_csv(datasetsOutput()$item_pairs_15_rev, file = "item_pairs_15_rev.csv", na = "")
      write_csv(datasetsOutput()$item_pairs_30, file = "item_pairs_30.csv", na = "")
      write_csv(datasetsOutput()$item_pairs_30_rev, file = "item_pairs_30_rev.csv", na = "")
      print (fs)
      
      zip(zipfile=fname, files=fs)
      
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
      
    },
    contentType = "application/zip"
  )
  
  onclick("download_CP_data",
          shinyjs::alert("Your files are downloading to your 'Downloads' folder")
  )
  
  onclick("downloadCleanedExport",
          shinyjs::alert("Your file is downloading to your 'Downloads' folder"))
  
  
  # Monitor the Data --------------------------------------------
  
  # 30 Year Data Checks
  output$monitor_30_tbl <- DT::renderDataTable({
    tbl <- cbind(names(introduce(datasetsOutput()$item_pairs_30)), 
                 transpose( introduce(datasetsOutput()$item_pairs_30) ) )
    
    tbl <- tbl[c(1,2,5,6,7),]  # keep relevant rows
    
    tbl[,1] <- c("Rows", "Columns", "Empty Columns",
                 "Total Missing Values (Cells)", "Complete Rows")
    
    DT::datatable(
      tbl,  
      width = "500px",
      options = list( dom = 't'),
      colnames = c("", "")
    ) 
  })
  
  output$monitor_30_missing <- renderPlot({
    plot_missing(datasetsOutput()$item_pairs_30,
                 ggtheme = theme_minimal(),
                 theme_config = list("legend.position" = "none")) 
  })
  
  output$monitor_30_histogram <- renderPlot({
    plot_histogram( datasetsOutput()$item_pairs_30[, c("Incumbents", "ItemCt", "PairCt", "Prob", "Salary1", "Salary1Min", "Salary2", "Salary2Min", "SalaryDiff")],
                    ggtheme = theme_minimal()) 
  })
  
  # 15 Year Data Checks
  
  output$monitor_15_tbl <- DT::renderDataTable({
    tbl <- cbind(names(introduce(datasetsOutput()$item_pairs_15)), 
                 transpose( introduce(datasetsOutput()$item_pairs_15) ) )
    
    tbl <- tbl[c(1,2,5,6,7),]  # keep relevant rows
    
    tbl[,1] <- c("Rows", "Columns", "Empty Columns",
                 "Total Missing Values (Cells)", "Complete Rows")
    
    DT::datatable(
      tbl,  
      width = "500px",
      options = list( dom = 't'),
      colnames = c("", "")
    ) 
  })
  
  output$monitor_15_missing <- renderPlot({
    plot_missing(datasetsOutput()$item_pairs_15,
                 ggtheme = theme_minimal(),
                 theme_config = list("legend.position" = "none")) 
  })
  
  output$monitor_15_histogram <- renderPlot({
    plot_histogram( datasetsOutput()$item_pairs_15[, c("Incumbents", "ItemCt", "PairCt", "Prob", "Salary1", "Salary1Min", "Salary2", "Salary2Min", "SalaryDiff")],
                    ggtheme = theme_minimal()) 
  })
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}