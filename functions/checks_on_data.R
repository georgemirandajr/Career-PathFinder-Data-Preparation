
# Need to check for and display to analyst

## number of item pairings (all 4 datasets)
rows_pairs15 <- nrow(item_pairs_15)
rows_pairs15_rev <- nrow(item_pairs_15_rev)
rows_pairs30 <- nrow(item_pairs_30)
rows_pairs30_rev <- nrow(item_pairs_30_rev)

show_number_pairs <- tibble::tibble(Dataset = c("item_pairs_30", "item_pairs_30_rev",
                                                "item_pairs_15", "item_pairs_15_rev"),
                                    `Item Pairs` = c(rows_pairs30, rows_pairs30_rev,
                                                     rows_pairs15, rows_pairs15_rev) )

## new item pairs and pairs that are gone (number and actual pairing)
# 30 fwd
old_30 <- readRDS("./temp/item_pairs_30.rds")
new_in_30 <- anti_join(item_pairs_30, old_30, 
                       by = c("Item1", "Item2"))

out_of_30 <- anti_join(old_30, item_pairs_30,
                       by = c("Item1", "Item2"))

num_new_in_30 <- nrow(new_in_30)
num_out_of_30 <- nrow(out_of_30)

# 30 rev
old_30_rev <- readRDS("./temp/item_pairs_30_rev.rds")
new_in_30_rev <- anti_join(item_pairs_30_rev, old_30_rev, 
                           by = c("Item1", "Item2"))

out_of_30_rev <- anti_join(old_30_rev, item_pairs_30_rev,
                           by = c("Item1", "Item2"))

num_new_in_30_rev <- nrow(new_in_30_rev)
num_out_of_30_rev <- nrow(out_of_30_rev)

# 15 fwd
old_15 <- readRDS("./temp/item_pairs_15.rds")
new_in_15 <- anti_join(item_pairs_15, old_15, 
                       by = c("Item1", "Item2"))

out_of_15 <- anti_join(old_15, item_pairs_15,
                       by = c("Item1", "Item2"))

num_new_in_15 <- nrow(new_in_15)
num_out_of_15 <- nrow(out_of_15)

# 15 rev
old_15_rev <- readRDS("./temp/item_pairs_15_rev.rds")
new_in_15_rev <- anti_join(item_pairs_15_rev, old_15_rev, 
                           by = c("Item1", "Item2"))

out_of_15_rev <- anti_join(old_15_rev, item_pairs_15_rev,
                           by = c("Item1", "Item2"))

num_new_in_15_rev <- nrow(new_in_15_rev)
num_out_of_15_rev <- nrow(out_of_15_rev)

## titles that are new (and total number)
old_item_ref_30 <- readRDS("./temp/item_ref_30.rds")
new_title_item_ref_30 <- anti_join(item_ref_30, old_item_ref_30,
                                   by = "TitleCode")

out_of_item_ref_30 <- anti_join(old_item_ref_30, item_ref_30,
                                by = "TitleCode")

## title with the longest number of characters
longest_title <- item_ref_30[ which( nchar(item_ref_30$TitleLong) == max(nchar(item_ref_30$TitleLong), na.rm = TRUE) ), ]$TitleLong
longest_title_nchar <- nchar(longest_title)

## titles with more than 20 choices
titles_over_20_choices <- item_pairs_30 %>%
    group_by(Item1) %>%
    filter( n() > 20 ) 

titles_over_20_count <- titles_over_20_choices %>%
    mutate( Choices = n() ) %>%
    distinct( Item1, .keep_all = TRUE ) %>%
    select(Item1, Item1Name, Choices) %>%
    arrange( desc(Choices) )

## titles with the largest and smallest salary range
largest_range <- item_ref_30 %>%
    ungroup() %>%
    mutate(Range = SalaryMax - SalaryMin) %>%
    top_n( n = 1, wt = Range) 

smallest_range <- item_ref_30 %>%
    ungroup() %>%
    mutate(Range = SalaryMax - SalaryMin) %>%
    top_n( n = -1, wt = Range) 

## title with highest number of incumbents
incumbents_most <- item_ref_30 %>% 
    ungroup() %>%
    top_n( n = 1, wt = Incumbents )

## number of titles with zero (0) incumbents
incumbents_zero <- item_ref_30 %>% 
    ungroup() %>%
    top_n( n = -1, wt = Incumbents )

## item pairs with highest and lowest salary difference
salary_diff_low <- item_pairs_30 %>%
    ungroup() %>%
    top_n( n = -1, wt = SalaryDiff )

salary_diff_high <- item_pairs_30 %>%
    ungroup() %>%
    top_n( n = 1, wt = SalaryDiff )

## Check for items that have a hyperlink, but no salary. There should be none.
link_no_sal_30 <- item_pairs_30 %>%
    filter( is.na(Salary2) & is.na(Salary2Min) & !is.na(Hyperlink) )

link_no_sal_30_rev <- item_pairs_30_rev %>%
    filter( is.na(Salary1) & is.na(Salary1Min) & !is.na(Hyperlink) )

link_no_sal_15 <- item_pairs_15 %>%
    filter( is.na(Salary2) & is.na(Salary2Min) & !is.na(Hyperlink) )

link_no_sal_15_rev <- item_pairs_15_rev %>%
    filter( is.na(Salary1) & is.na(Salary1Min) & !is.na(Hyperlink) )


## Make sure all items that don't have a hyperlink also don't have a salary
pairs_30_noLink_wSal <- item_pairs_30 %>%
    filter( is.na(Hyperlink) ) %>%
    ungroup() %>%
    summarise( noLink_wSalary = any(!is.na(Salary2))) %>%
    mutate( Dataset = "item_pairs_30")

pairs_30_rev_noLink_wSal <- item_pairs_30_rev %>%
    filter( is.na(Hyperlink) ) %>%
    ungroup() %>%
    summarise( noLink_wSalary = any(!is.na(Salary1))) %>%
    mutate( Dataset = "item_pairs_30_rev")

pairs_15_noLink_wSal <- item_pairs_15 %>%
    filter( is.na(Hyperlink) ) %>%
    ungroup() %>%
    summarise( noLink_wSalary = any(!is.na(Salary2))) %>%
    mutate( Dataset = "item_pairs_15")

pairs_15_rev_noLink_wSal <- item_pairs_15_rev %>%
    filter( is.na(Hyperlink) ) %>%
    ungroup() %>%
    summarise( noLink_wSalary = any(!is.na(Salary1))) %>%
    mutate( Dataset = "item_pairs_15_rev")

pairs_noLink_wSal <- rbind(pairs_30_noLink_wSal, pairs_30_rev_noLink_wSal,
                           pairs_15_noLink_wSal, pairs_15_rev_noLink_wSal) %>%
    select(Dataset, noLink_wSalary)
