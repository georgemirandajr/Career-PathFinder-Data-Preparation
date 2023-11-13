# This function creates the item reference list used in the app

make_item_ref <- function(x) {
    
    # x must have a column named "TitleCode"
    ref <- x %>% select(Item1, Item1Name, Salary1, Salary1Min,
                        Hyperlink)

    # Identify rows that are duplicates based on the first 3 columns
    idx <- duplicated.data.frame(ref[,1:3])
    
    # Subset for rows that are not duplicates
    ref <- ref[!idx,]
    
    # Rename the columns
    ref <- ref %>%
        rename(TitleCode = Item1, TitleLong = Item1Name, SalaryMax = Salary1, SalaryMin = Salary1Min)
    
    ref <- left_join(ref, incumbents,
                     by = c("TitleCode" = "TitleCode") )
    
    ref <- ref %>% 
        rename(Incumbents = Count)
    
    ref[ is.na(ref$Incumbents), "Incumbents"] <- 0
    
    # Reorder columns
    ref <- ref %>% 
        select(TitleCode, TitleLong, SalaryMax, SalaryMin, Incumbents, Hyperlink)

    ref
}