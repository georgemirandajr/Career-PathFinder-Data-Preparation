# Build a hyperlink to government jobs based on user's search

getLink <- function(x) {
    
    linkBase <- "https://www.governmentjobs.com/careers/lacounty/classspecs?&keywords=" 
    
    space <- "%20"
    
    term <- as.character(x)
    
    # this prevents hyperlinks from being made with anything more than 4 digits 
    # in a title code
    term <- substr(term, 1, 4)  
    
    newTerm <- gsub(" ", space, term)
    
    link <- paste0(linkBase, newTerm)
    
    link <- paste0("<a href='", link, "' target = '_blank' >View Description</a>")
    
    return(link)
}