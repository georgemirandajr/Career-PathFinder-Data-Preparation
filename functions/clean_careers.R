
clean_careers <- function(x) {
    
    ##--- Build in checks ---##
    # Check for:
    # JobApptDate and JobLastDate are %in% c("POSIXct", "POSIXt")
    # Check if the required datasets are loaded.
    if( !exists("salary") ) stop("'salary' dataset is not loaded.")
    if( !exists("current_titles") ) stop("'current_titles' dataset is not loaded.")
    
    # Check if colnames are correct 
    if( !all(c("EmployeeID",  "TitleCode",   "TitleLong",   "JobApptDate", "JobLastDate") %in%
             names(x)) ) 
        stop("missing or incorrect column names. Must be 'EmployeeID' 'TitleCode' 'TitleLong' JobApptDate' 'JobLastDate'")
    
    # Load the stat functions to get counts of key metrics at each rule        
    # source("./functions/statFunctions.R")  
    
    # Lag the EMPNO variable by 1 position
    combined <- x %>% mutate(EMPNO2 = lag(EmployeeID, n = 1))  
    
    # Lag the ITEM variable by 1 position
    combined <- combined %>% mutate(ITEM2 = lag(TitleCode, n = 1))  
    
    # Lagging shifted item numbers that didn't necessarily belong to the next person in the data.
    # Identify the records where the lagged EMPNO2 is not equal to EMPNO and convert their ITEM2 values to NA
    combined[which(combined$EMPNO2 != combined$EmployeeID),]$ITEM2 <- NA  
    
    # Rename title code variables
    combined <- combined %>%
      dplyr::rename(Item2 = TitleCode, Item1 = ITEM2)
    
    # beforeRuleStats <<- statAll(combined)
    
    ##-------- Rule: Any Moves Under 30 Days -----------##
    # Create a new variable called "JobApptDate1" and give it the lagged job appt dates. 
    # Use JobApptDate1 because it was the starting (or previous) item for the person
    combined <- combined %>% 
        mutate(JobApptDate1 = lag(JobApptDate, n = 1)) 
    
    # Rename JobApptDate to JobApptDate2 which indicates it is the next item the person held.
    combined <- combined %>%
        dplyr::rename(JobApptDate2 = JobApptDate)  
    
    # Subset where the employee IDs are not equal and make the JobApptDate1 column NA.
    combined[ which( combined$EMPNO2 != combined$EmployeeID ), ]$JobApptDate1 <- NA  # 500354 obs
    
    # Calculate the time in days between JobApptDate1 and JobApptDate2 
    combined$DaysToMovement <- lubridate::as.period(
        lubridate::interval(
            start = combined$JobApptDate1, 
            end = combined$JobApptDate2
        ), 
        unit = "days")$day  # get number of days
    
    under30days <- combined %>% dplyr::filter( !(DaysToMovement < 30) | 
                                            is.na( !(DaysToMovement < 30) ) )
    
    # rule1stat <<- statUnder30(under30days)
    
    ##-------- Rule: Multiple Laterals -----------##
    
    # Convert to data.table for analysis (efficiently removing duplicates)
    # combined_multiXfers <- data.table(under30days)
    combined_multiXfers <- under30days
    
    # Ensure that each employee has one record for each unique movement
    combined_multiXfers <- combined_multiXfers[
        !base::duplicated(
            combined_multiXfers[ c("EmployeeID", "Item2", "Item1")]
        ), ]
    
    class(combined_multiXfers) <- "data.frame"  # convert to data.frame
    
    # rule2stat <<- statMultiXfers(combined_multiXfers)
    
    ##-------- Rule: Expired Classes -----------##
    
    # Remove expired items from the list of current titles.
    
    combined_exp <- combined_multiXfers
    
    # impute missing titles using the Title Extract
    combined_exp$ImputedTitle <- 
        current_titles[ match(combined_exp$Item2, current_titles$TitleCode), ]$TitleLong
    
    combined_exp <- combined_exp %>%
        mutate(TitleLong = ifelse(!is.na(TitleLong),
                                  TitleLong,
                                  ifelse( is.na(TitleLong),
                                          ImputedTitle,
                                          NA))) %>%
        select(-ImputedTitle)
    
    
    # Identify expired items in the Item1 column
    combined_exp <- combined_exp %>% 
        mutate(Item1_exp = ifelse( !Item1 %in% current_titles$TitleCode & !is.na(Item1),
                                  1, 0))
    
    # Identify expired items in the Item2 column
    combined_exp <- combined_exp %>%
        mutate(Item2_exp = ifelse( !Item2 %in% current_titles$TitleCode,
                                  1, 0))
    
    # Identify expired items in EITHER Item1 or Item2
    combined_exp <- combined_exp %>%
        mutate(Any_item_exp = ifelse(Item1_exp == 1 | Item2_exp == 1,
                                     1, 0))
    
    # Keep records that are not expired
    combined_exp <- combined_exp %>%
        dplyr::filter(Any_item_exp == 0 | is.na(Item1))
    
    # rule3stat <<- statExp(combined_exp)
    
    ##-------- Rule: Demotions > 5% -----------##
    
    # Import salaries for each item (i.e., Item2 and Item1)
    combined_exp$Item2copy <- str_sub( combined_exp$Item2, 1, 4)  # temp copy
    combined_demotions <-
        left_join(combined_exp, salary[, c("Item", "Range Max")],
                  by = c("Item2copy" = "Item")) %>%
        dplyr::rename(Salary2 = `Range Max`) %>%
        select(-Item2copy)
    
    combined_demotions$Item1copy <- str_sub( combined_demotions$Item1, 1, 4)  # temp copy
    combined_demotions <- left_join(combined_demotions, salary[,c("Item", "Range Max")], 
                                    by = c("Item1copy" = "Item")) %>%
        dplyr::rename(Salary1 = `Range Max`) %>%
        select(-Item1copy)
    
    # Create a new variable called "Demotion" and assign 1 (demotion) or 0 (non-demotion)
    combined_demotions <- combined_demotions %>% 
        mutate(Demotion = ifelse(Salary2 == Salary1,        # if salaries are equal
                                 0,                         # assign 0
                                 ifelse(Salary2 > Salary1,  # if Salary2 is more
                                        0,                  # assign 0
                                        1)                  # otherwise, assign 1
        )
        ) 
    
    # New columns were created to identify demotions that were either under 5.5% or over 10%.
    combined_demotions <- combined_demotions %>% 
        mutate(greater5 = ifelse( (Salary1-Salary2)/Salary1 > 0.0550000,
                                  # if diff is less than or equal to 5.5% and at least 0
                                  1, 
                                  0 ) ) 
    
    # Remove demotions that are greater than 5.5% (exclusive)
    combined_demotions <- combined_demotions %>% dplyr::filter( !(Demotion == 1 & greater5 == 1) | 
                                                             is.na( !(Demotion == 1 & greater5 == 1) ) )
    
    rm(combined, combined_multiXfers, under30days)
    
    # rule4stat <<- statDemotions(combined_demotions)
    
    combined_demotions <- combined_demotions %>%
        select(EmployeeID, Item2, TitleLong, JobApptDate2, JobLastDate,   
               EMPNO2, Item1, JobApptDate1, DaysToMovement,
               Item1_exp, Item2_exp, Any_item_exp, Salary2, Salary1,       
               Demotion, greater5)
    
    combined_demotions
}