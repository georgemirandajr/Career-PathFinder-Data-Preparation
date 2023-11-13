
make_pairs_rev <- function(x) {
  
  ##--- Build in checks ---##
  
  
  ##--- Identify item numbers that belong to LACERA and Superior Court
  to_remove <- unique(x[ 
    grep("LACERA|,SC|, SC|\\bMUNICIPAL\\b|\\bNCS\\b|\\bJUDGE\\b|\\bMC\\b|\\bM\\.C\\.\\b|MUNI COURT|MUNI CT|SUP CT", 
         x$TitleLong), ]$Item2)
  
  
  ##----- Make Item Pairs -----##  (Make this a function in a separate script)
  # Drop most columns to end up with the EMPNO and their corresponding item movements
  # Remember: ITEM2 is actually the earlier item while ITEM is the one that follows ITEM2
  
  eHist <- x %>%
    dplyr::select( EmployeeID, Item1, Item2 )
  
  # Remove items from the eHist item pairings that are from LACERA or Superior Court
  eHist <- eHist %>%
    dplyr::filter( !Item1 %in% to_remove &
                     !Item2 %in% to_remove ) %>%
    dplyr::filter( !Item1 %in% c("9686", "9826", "9691", "9763", "9662") &
                     !Item2 %in% c("9686", "9826", "9691", "9763", "9662"))
  
  # Create a frequency table that counts the number of times Item1 appears 
  item1_count <- data.frame(table(eHist$Item1))  
  item1_count$Var1 <- as.character(item1_count$Var1)  # convert to character from factor
  
  # Create another table that counts the number of times each combination of items
  # appears and filter this new dataset to isolate those that have at least one 
  # combination occur
  item_pairs <- data.frame(table(eHist$Item2, eHist$Item1)) %>% 
    dplyr::filter(Freq > 0) 
  
  # Order by Item1 (Var1) 
  item_pairs <- item_pairs %>% dplyr::arrange(Var1, desc(Freq))
  
  # Add up all movements to Item2 (Var1) from Item1 (Var2) and save as a separate reference table
  movement_sums <- item_pairs %>% 
    dplyr::group_by(Var1) %>% 
    dplyr::summarise(Freq = sum(Freq)) %>% 
    dplyr::ungroup()
  
  movement_sums$Var1 <- as.character(movement_sums$Var1)  # convert to character from factor
  
  # Bring in the count of each unique item and align it with the item pair based on Item2 column 
  item_pairs$ItemCt <- movement_sums[match(item_pairs$Var1, movement_sums$Var1),"Freq"]
  
  item_pairs$ItemCt <- unlist(item_pairs$ItemCt)  # matching created a sub-data.frame object
  
  item_pairs <- item_pairs %>%
    dplyr::arrange(Var1, desc(Freq))
  
  # Rename some variables for clarity
  item_pairs <- item_pairs %>%
    dplyr::rename(Item2 = Var1, Item1 = Var2, PairCt = Freq)
  
  # Calculate the probability of each pair in item_pairs
  item_pairs <- item_pairs %>%
    dplyr::mutate(Prob = round(PairCt / ItemCt, 5)) %>%
    dplyr::arrange(Item2, -Prob, -PairCt)  # decreasing order by item, probability, then by count of each pair
  
  item_pairs$Item1 <- as.character(item_pairs$Item1)  # convert to character from factor
  
  item_pairs$Item2 <- as.character(item_pairs$Item2)  # convert to character from factor
  
  item_pairs$Item1Name <- unlist(sapply(item_pairs$Item1, get_title))  # get title names
  
  item_pairs$Item2Name <- unlist(sapply(item_pairs$Item2, get_title))  # get title names
  
  # Find any NA values in title names and replace with most recent title from data (if any)
  item_pairs <- item_pairs %>%
    dplyr::mutate( Item1Name = ifelse( !is.na(Item1Name),
                                       Item1Name,
                                       titles_in_data[ match(Item1, titles_in_data$TitleCode), "TitleLong" ] ),
                   Item2Name = ifelse( !is.na(Item2Name),
                                       Item2Name,
                                       titles_in_data[ match(Item2, titles_in_data$TitleCode), "TitleLong" ] ) )
  
  # Bring in the number of current incumbents in job 1
  # item_pairs$Incumbents <- item1_count[match(item_pairs$Item1, item1_count$Var1),"Freq"]
  item_pairs$Incumbents <- incumbents[match(item_pairs$Item1, incumbents$TitleCode),"Count"]
  
  rm(item1_count)
  
  # Bring in salary data, calculate the difference, and leave as numeric w/o any formatting
  item_pairs$Item1copy <- str_sub( item_pairs$Item1, 1, 4 )  # temp copy
  item_pairs$Item2copy <- str_sub( item_pairs$Item2, 1, 4 )  # temp copy
  item_pairs$Salary1 <- salary[match(item_pairs$Item1copy, salary$Item),"Range Max"]
  item_pairs$Salary2 <- salary[match(item_pairs$Item2copy, salary$Item),"Range Max"]
  
  item_pairs$Salary1Min <- salary[match(item_pairs$Item1copy, salary$Item), "Range Min"]
  item_pairs$Salary2Min <- salary[match(item_pairs$Item2copy, salary$Item),"Range Min"]
  
  item_pairs <- item_pairs %>%
    select(-Item1copy, -Item2copy)
  
  item_pairs$Hyperlink <- ifelse( grepl("\\(UC\\)|/UC|\\bUC\\b|\\bUNCLASSIFIED\\b|\\bUNCLASSSIFIED\\b", item_pairs$Item1Name),
                                  NA,
                                  ifelse( is.na(item_pairs$Salary1),
                                          NA,
                                          substr(item_pairs$Item1, 1, 4) ) )
  
  # TESTING 
  item_pairs$Salary1 <- ifelse( is.na(item_pairs$Hyperlink),
                                NA,
                                item_pairs$Salary1)
  
  item_pairs$Salary1Min <- ifelse( is.na(item_pairs$Hyperlink),
                                   NA,
                                   item_pairs$Salary1Min)
  
  # Find Salary Difference
  item_pairs <- item_pairs %>%
    dplyr::mutate(SalaryDiff = round(Salary2-Salary1, 2))   # Find difference in salaries
  
  # Order the variables 
  item_pairs <- item_pairs %>%
    dplyr::select(Item2, Item2Name, Item1, Item1Name, PairCt, ItemCt, Prob, 
                  Incumbents, SalaryDiff, Salary1, Salary2, Salary1Min, Salary2Min, Hyperlink)
  
  # Take the top 20 for every job in Item2 column (ties allowed)
  item_pairs <- item_pairs %>%
    dplyr::group_by(Item2) %>%
    dplyr::top_n(n = 20, wt = Prob)
  
  item_pairs
}    