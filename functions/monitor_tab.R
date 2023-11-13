monitor_tab_content <- function() {
  
  tagList(
    
    tags$h6("Once you have cleaned the data, you may use this tab to check some basic traits about the exports.
            For example, you can verify that certain columns have no missing information and view the distribution
            of columns to determine if there's any unusual values. This page only analyzes the 'forward' datasets,
            because the 'reverse' data is almost exactly the same.")
  )
}