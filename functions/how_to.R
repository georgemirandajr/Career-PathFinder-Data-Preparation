

how_to_content <- function() {
  
  tagList(
    tags$h3("How to Use This App"),
    
    tags$p("The purpose of this application is to prepare the data for use in the Career PathFinder tool. 
Simply upload e-HR extract text files and click a button to apply data cleaning rules, create item pairs, and create export
           files. The exported .csv files are zipped in a folder within your 'Downloads' folder."),
    tags$h4("How To Use This Application"),
    tags$div(
      tags$p("Update the data for the Career PathFinder tool by following these steps:"),
      tags$ol(
        tags$li("Upload the needed files in the 'Input/Output' tab (i.e., EMPL_JPACT_EXTR, PAY_POLICY_RATE, TITLE_Reference_Extract)"), 
        tags$li("Clean the data by clicking the 'Clean Data' button"), 
        tags$li("Make a copy of the export files and review them for any anomalies."),
        tags$li("Upload the original 4 datasets (the ones you did not view) to WordPress.")
      )
    ),
    tags$h4("Behind the Scenes"),
    tags$div(
      tags$p("The DataCleaningScript.R makes use of clean_careers.R to process the system-extracted text files
             by applying the following data cleaning rules:"),
      tags$ol(
        tags$li("Delete expired classifications (using the 'EXPIRED' keyword in the TITLE_Reference_Extract)"),
        tags$li("Delete multiple lateral transfers"),
        tags$li("Delete lateral transfers within 30 days"),
        tags$li("Delete demotions that are beyond 5.5% decrease in salary")
      ),
      tags$p("Once the data rules have been applied, DataCleaningScript.R calls the functions make_pairs.R and make_pairs_rev.R. 
These functions create the final datasets containing summarized career movements. 
The forward and backward item pairings for 30 and 15 year datasets are exported to your 'Downloads' folder.")
    )
  )
}