# Stats at each rule

statAll <- function(combined) {
# All Data Before Starting Any Rules
a <- table(combined$EmployeeID)  # count of each employeeID
a <- data.frame( table(a) )  # count of employees that held x number of jobs
a$a <- as.numeric(a$a)
a <- a %>% mutate(Records = a*Freq)  # compute number of records
a <- a %>% mutate(Moves = (a-1)*Freq)  # compute number of moves

total_moves <- sum(a$Moves)  # total movements in the combined dataset
total_records <- sum(a$Records)  # total number of records in combined dataset

## --- Build Overall Stats Table --- ##
Count_allData <- c(format(length(unique(combined$EmployeeID)), big.mark = ","),
                   format(length(unique(combined$Item2)), big.mark = ","),
                   format(total_moves, big.mark = ","),
                   format(total_records, big.mark = ",")
)
Count_allData
}

# Moves Under 30 Days Rule
statUnder30 <- function(under30days) {
a <- table(under30days$EmployeeID)  # count of each employeeID
a <- data.frame( table(a) )  # count of employees that held x number of jobs
a$a <- as.numeric(a$a)
a <- a %>% mutate(Records = a*Freq)  # compute number of records
a <- a %>% mutate(Moves = (a-1)*Freq)  # compute number of moves

total_moves <- sum(a$Moves)  # total movements in the combined dataset
total_records <- sum(a$Records) 

## --- Build Rule 1, 30 day Stats Table --- ##
Count_30day <- c(format(length(unique(under30days$EmployeeID)), big.mark = ","),
                 format(length(unique(under30days$Item2)), big.mark = ","),
                 format(total_moves, big.mark = ","),
                 format(total_records, big.mark = ",")
)
Count_30day
}

# Multiple Transfers Rule

statMultiXfers <- function(combined_multiXfers) {
a <- table(combined_multiXfers$EmployeeID)  # count of each employeeID
a <- data.frame( table(a) )  # count of employees that held x number of jobs
a$a <- as.numeric(a$a)
a <- a %>% mutate(Records = a*Freq)  # compute number of records
a <- a %>% mutate(Moves = (a-1)*Freq)  # compute number of moves

total_moves <- sum(a$Moves)  # total movements in the combined dataset
total_records <- sum(a$Records) 

## --- Build Rule 2 Stats Table --- ##
Count_MultiLats <- c(format(length(unique(combined_multiXfers$EmployeeID)), big.mark = ","),
                     format(length(unique(combined_multiXfers$Item2)), big.mark = ","),
                     format(total_moves, big.mark = ","),
                     format(total_records, big.mark = ",")
)
Count_MultiLats
}

# Demotions Rule
statDemotions <- function(combined_demotions) {
a <- table(combined_demotions$EmployeeID)  # count of each employeeID
a <- data.frame( table(a) )  # count of employees that held x number of jobs
a$a <- as.numeric(a$a)
a <- a %>% mutate(Records = a*Freq)  # compute number of records
a <- a %>% mutate(Moves = (a-1)*Freq)  # compute number of moves

total_moves <- sum(a$Moves)  # total movements in the combined dataset
total_records <- sum(a$Records) 

## --- Build Rule 3 Stats Table --- ##
Count_Demotions <- c(format(length(unique(combined_demotions$EmployeeID)), big.mark = ","),
                     format(length(unique(combined_demotions$Item2)), big.mark = ","),
                     format(total_moves, big.mark = ","),
                     format(total_records, big.mark = ",")
)
Count_Demotions
}

# Expired Classes Rule
statExp <- function(combined_exp) {
a <- table(combined_exp$EmployeeID)  # count of each employeeID
a <- data.frame( table(a) )  # count of employees that held x number of jobs
a$a <- as.numeric(a$a)
a <- a %>% mutate(Records = a*Freq)  # compute number of records
a <- a %>% mutate(Moves = (a-1)*Freq)  # compute number of moves

total_moves <- sum(a$Moves)  # total movements in the combined dataset
total_records <- sum(a$Records) 

## --- Build Rule 1 Stats Table --- ##
Count_Expired <- c(format(length(unique(combined_exp$EmployeeID)), big.mark = ","),
                   format(length(unique(combined_exp$Item2)), big.mark = ","),
                   format(total_moves, big.mark = ","),
                   format(total_records, big.mark = ",")
)
Count_Expired
}

