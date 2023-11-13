analysis_tab_content <- function() {
  
  tagList(
    
    tags$h6("This page allows you to format the user-generated data exported from 
the Word Press admin site. It provides you a preview of what the data looks like 
           after it has been formatted and some helpful stats about the career paths people have created."),
    tags$h4("Export Data from Word Press"),
    tags$h6("You must have login credentials to download user data from the Word Press
           admin site. Assuming you have access, you can download all user data by
           clicking the button shown below."),
    tags$img(src = "./wp-admin-export.png", height = "400px"),
    tags$br()
  )
}