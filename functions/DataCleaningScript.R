# Package versions for R 3.3.0 (updated to work on 3.4.3): 
# readr - 1.1.0
# Hmisc - 4.0-3
# dplyr - 0.5.0
# stringr - 1.3.1  (stringi also updated to 1.1.7)
# knitr - 1.15.1
# htmlTable - 1.9
# data.table - 1.10.4
# lubridate - 1.6.0

# System info:
# R version - 3.3.0 (64-bit)
# R Studio 0.99.896
# Platform: 64-bit Windows 7 x64

get_title <- function(x) unlist(current_titles[which(current_titles$TitleCode == x), 
                                               "TitleLong"],
                                use.names = FALSE)  # a helper function to use later


dataCleaning <- function( jpact_data, pprt_data, title_data ) {
  
  ##-------- Load CWTAPPS Data -----------##
  load(file = "./input/taps.RDS")  # 394,908 records
  
  ##-------- Advantage Data -----------##
  
  # Date of the data is in the first row
  # file_date <- setDT(readr::read_fwf( jpact_file()$datapath, n_max = 1,
  #                                      fwf_positions(job_specs$Start,
  #                                                    job_specs$End,
  #                                                    col_names = job_specs$FieldName))) %>%
  #     select(2) %>%  # date is in the Employee ID column
  #     mdy()
  
  ##-------- Salary Data -------------##
  
  salaryMin <-  pprt_data %>% 
    select(PayPolicy, Step, ExpirationDate, PayRateAmount) %>%
    filter(ExpirationDate == "12319999") %>%
    mutate(Sub = str_sub(PayPolicy, -1, -1)) %>%  # extract the sub-title code
    mutate(Item = str_replace(PayPolicy, ".{1}$", "")) %>% # remove last char and rename
    filter(Sub == "A") %>%
    arrange(Item, PayRateAmount) %>%
    distinct(Item, .keep_all = TRUE) %>%
    dplyr::rename(`Range Min` = PayRateAmount) %>%
    mutate(`Range Min` = `Range Min`*2)  # convert to monthly salary
  
  salary <- pprt_data %>% 
    select(PayPolicy, Step, ExpirationDate, PayRateAmount) %>%
    filter(ExpirationDate == "12319999") %>%
    mutate(Sub = str_sub(PayPolicy, -1, -1)) %>%  # extract the sub-title code
    mutate(Item = str_replace(PayPolicy, ".{1}$", "")) %>% # remove last char and rename
    filter(Sub == "A") %>%
    arrange(Item, desc(PayRateAmount)) %>%
    distinct(Item, .keep_all = TRUE) %>%
    dplyr::rename(`Range Max` = PayRateAmount) %>%
    mutate(`Range Max` = `Range Max`*2)  # convert to monthly salary
  
  salary <- left_join(salary, 
                      salaryMin[ , c("Item", "Range Min") ],
                      by = c("Item" = "Item") )
  
  # If the minimum salary is zero, then use the maximum salary for the minimum
  salary <<- salary %>%
    mutate(`Range Min` = ifelse(`Range Min` == 0, `Range Max`, `Range Min`))
  
  ##--------- Current Titles ----------##
  # Use the TITLE_Reference_Extract as the authority for current titles
  
  current_titles <- title_data  # make a copy of title to find only the current titles
  current_titles <- current_titles[ grep("EXPIRED|~|SECONDARY",
                                         current_titles$TitleLong,
                                         invert = TRUE), ]

  current_titles <- current_titles[ grep("UNAVAILABLE",
                                         current_titles$TitleShort,
                                         invert = TRUE), ]

  current_titles <- current_titles[ grep("CONV",
                                         current_titles$TitleCode,
                                         invert = TRUE), ]

  # Replace abbreviations in 'current_titles' with long versions of title
  # source("./functions/remove_abbr.R")
  
  # Get rid of abbreviations
  
  # This first list is for titles that have special characters and cannot be wrapped with the '\\b'
  list_one <- c("SPEC. SVCS." = "SPECIAL SERVICES", "SPEC.COORD." = "SPECIFICATION COORDINATOR")
  pattern_one <- str_c(names(list_one), collapse = "|")
  abbrev2long <- function(abbr) {
    list_one[abbr]
  }
  
  current_titles$TitleLong <- str_replace_all(current_titles$TitleLong,
                                              pattern = pattern_one,
                                              replacement = abbrev2long)
  
  # This second list is for terms that can be wrapped with '\\b'.
  abbrev_list <- c("ASST" = "ASSISTANT" , "CHF" = "CHIEF", "DIV" = "DIVISION", "CNTR" = "CENTER",
                   "DIR" = "DIRECTOR", "EXEC" = "EXECUTIVE", "HD" = "HEAD", "HLTH SERVS" = "HEALTH SERVICES",
                   "HLTH" = "HEALTH", "MAINT" = "MAINTENANCE", "MGR" = "MANAGER", 
                   "MGMT" = "MANAGEMENT", "MGT" = "MANAGEMENT", "OPER" = "OPERATIONS", "PGMS" = "PROGRAMS", 
                   "PROG" = "PROGRAM", "PHYS" = "PHYSICIAN", "REG-REC/CO CLK" = "REGISTRAR-RECORDER/COUNTY CLERK", 
                   "SERVS" = "SERVICES", "SPEC" = "SPECIALIST", "SUPVG" = "SUPERVISING", "SVCS" = "SERVICES", 
                   "SVS" = "SERVICES", "DEP" = "DEPUTY", "REGISTRAR-REC/CO CLK" = "REGISTRAR-RECORDER/COUNTY CLERK",
                   "REG-REC/CO CLERK" = "REGISTRAR-RECORDER/COUNTY CLERK",
                   "REG-REC/CO CLK" = "REGISTRAR-RECORDER/COUNTY CLERK", "DEPTL" = "DEPARTMENTAL",
                   "REGISTRAR-RECORDER/CO CLK" = "REGISTRAR-RECORDER/COUNTY CLERK", "OPNS" = "OPERATIONS", "EMER" = "EMERGENCY",
                   "TTC" = "TREASURER AND TAX COLLECTOR", "INT MED" = "INTERNAL MED",
                   "INT DIAGNOSTIC" = "INT DIAGNOSITC", "RESEARCH TR" = "RESEARCH TRAINEE",
                   "MED" = "MEDICAL", "CTR" = "CENTER", "PHARM" = "PHARMACY", "OFC" = "OFFICE OF",
                   "PGM" = "PROGRAM", "ALCHL" = "ALCOHOL", "SUPVR" = "SUPERVISOR", "DEPY" = "DEPUTY", "TREAT" = "TREATMENT",
                   "DPY" = "DEPUTY", "OFCR" = "OFFICER", "OFFCR" = "OFFICER", "PROB" = "PROBATION", "WKR" = "WORKER",
                   "BLDG" = "BUILDING", "ENGRG" = "ENGINEERING", "ENGR" = "ENGINEER", "ASSOC" = "ASSOCIATE", 
                   "SVC" = "SERVICES", "MUS. O ART" = "MUSEUM OF ART", "MUS. OF ART" = "MUSEUM OF ART", 
                   "DPTY" = "DEPUTY", "SYS" = "SYSTEMS", "SR" = "SENIOR", "AST" = "ASSISTANT", "HOSP" = "HOSPITAL",
                   "CONSULT" = "CONSULTING", "EQUIP" = "EQUIPMENT", "FIGHTG" = "FIGHTING", "EQPT" = "EQUIPMENT",
                   "CONSTRUCT" = "CONSTRUCTION", "ADMR" = "ADMINISTRATOR", "COMP" = "COMPREHENSIVE",
                   "AMB" = "AMBULATORY", "ASSISTANT ADMN" = "ASSISTANT ADMINISTRATOR", "PRIN" = "PRINCIPAL",
                   "PRINCIPAL ADMN" = "PRINCIPAL ADMINISTRATOR", "ASSISTANT PROGRAM ADMN" = "ASSISTANT PROGRAM ADMINISTRATOR",
                   "ADMN SERVICES" = "ADMINISTRATIVE SERVICES", "AGRI COMM/WTS & MEAS" = "AGRICULTURAL COMMISSIONER/WEIGHTS AND MEASURES",
                   "PULMONARY PHYS TECH SUPV" = "PULMONARY PHYSIOLOGY TECHNICIAN SUPERVISOR", "ASST ADMN" = "ASSISTANT ADMINISTRATOR",
                   "INFORMATION TECHNOLOGY TECH SUPPORT SUPERVISOR" = "INFORMATION TECHNOLOGY TECHNICAL SUPPORT SUPERVISOR",
                   "PAT FINANCIAL SERVS CONTROL SUPV" = "PATIENT FINANCIAL SERVICES CONTROL SUPERVISOR",
                   "SUPV APPEALS" = "SUPERVISING APPEALS", "BD OF SUPVRS" = "BOARD OF SUPERVISORS", 
                   "BD OF SUPV" = "BOARD OF SUPERVISORS", "ENGINEER APP" = "ENGINEER APPRENTICE", "OFFR" = "OFFICER",
                   "BRD OF SUPVRS" = "BOARD OF SUPERVISORS", "BD OF SUP" = "BOARD OF SUPERVISORS",
                   "BOARD OF SUPVRS" = "BOARD OF SUPERVISORS", "PSS" = "PUBLIC SOCIAL SERVICES", 
                   "ELECTRONIC COMMUNIC TECH WKG SUPVRR" = "ELECTRONICS COMMUNICATIONS TECHNICIAN WORKING SUPERVISOR",
                   "SUPV CHILDREN'S" = "SUPERVISING CHILDREN'S", "PERSONNEL TECH" = "PERSONNEL TECHNICIAN",
                   "RECS" = "RECORDS", "MH" = "MENTAL HEALTH", "EMPL" = "EMPLOYEE",
                   "REP" = "REPRESENTATIVE", "CAPTAIN,DA" = "CAPTAIN, DISTRICT ATTORNEY", "WKG" = "WORKING",
                   "INFORMATION TECHNOLOGY TECH SUPPORT SUPERVISOR" = "INFORMATION TECHNOLOGY TECHNICAL SUPPORT SUPERVISOR",
                   "PUB DEFENDER" = "PUBLIC DEFENDER", "ADMIN DEPUTY" = "ADMINISTRATIVE DEPUTY", "ADMIN & FACILITIES" = "ADMINISTRATIVE & FACILITIES",
                   "ADMIN SYSTEMS" = "ADMINISTRATIVE SYSTEMS", "ADMIN," = "ADMINISTRATOR,", 
                   "ADMIN, CONTRACT" = "ADMINISTRATOR, CONTRACT", "ADMIN SERVICES" = "ADMINISTRATIVE SERVIES",
                   "CHIEF ADMIN" = "CHIEF ADMINISTRATIVE", "DEVELOPMENT ADMIN" = "DEVELOPMENT ADMINISTRATOR", 
                   "HUMAN RESOURCES ADMIN" = "HUMAN RESOURCES ADMINISTRATOR", "ADMIN SUPPORT" = "ADMINISTRATIVE SUPPORT",
                   "IT"="INFORMATION TECHNOLOGY", "MEDICAL SERVICE COORDINATOR,CCS" = "MEDICAL SERVICE COORDINATOR, CALIFORNIA CHILDREN SERVICES",
                   "CHF,ASSESS APPEALS BDS,BD OF SUPVRS" = "CHIEF, ASSESSMENT APPEALS BOARDS, BOARD OF SUPERVISORS",
                   "PHYSICIAN SPECIALIST, MF" = "MEGAFLEX", "PROC SVS" = "PROCESSING SERVICES", "CRD" = "COORDINATOR",
                   "COORD" = "COORDINATOR", "CFS" = "CHILDREN AND FAMILY SERVICES", "TESTING TECH" = "TESTING TECHNICIAN",
                   "SECTION MANAGER, INFO. TECH., SYSTEMS PROG., ISD" = "SECTION MANAGER, INFORMATION TECHNOLOGY, SYSTEMS PROGRAM, ISD",
                   "PARKS & REC" = "PARKS & RECREATION", "MKTG" = "MARKETING", "REPR" = "REPRESENTATIVE",
                   "UNCLASS" = "UNCLASSIFIED", "FIN SVS" = "FINANCIAL SERVICES", "OPR" = "OPERATOR", "A/C" = "AUDITOR/CONTROLLER",
                   "AUTO FINGERPRINT IDENTIFICATION SYST OPS SUPVR" = "AUTOMATED FINGERPRINT IDENTIFICATION SYSTEM OPERATIONS SUPERVISOR",
                   "GRNDS" = "GROUNDS", "ADMIN COORDINATOR" = "ADMINISTRATIVE COORDINATOR",
                   "SUPVG MEDICAL SERVICE COORD,CCS" = "SUPERVISING MEDICAL SERVICE COORDINDATOR, CALIFORNIA CHILDREN SERVICES",
                   "WELFR" = "WELFARE", "RECIP" = "RECIPIENT", "VEND" = "VENDOR", "SUPT"="SUPERINTENDENT", "MECHANIC APP" = "MECHANIC APPRENTICE",
                   "PULMONARY PHYSIOLOGY, TECH SUPVR" = "PULMONARY PHYSIOLOGY TECHNICIAN SUPERVISOR", 
                   "PUBLIC DEF" = "PUBLIC DEFENDER", "MNGR" = "MANAGER", "AVRC" = "ANTELOPE VALLEY REHABILITATION CENTER",
                   "REGIONAL PLANNIN" = "REGIONAL PLANNING", "PW" = "PUBLIC WORKS", "HS" = "HEALTH SERVICES",
                   "TRANSP"="TRANSPORTATION", "DCS" = "DEPARTMENT OF CHILDREN SERVICES", "COMPREHEN" = "COMPREHENSIVE",
                   "HCC" = "HEALTH CARE CENTER", "ADMN SVCS" = "ADMINISTRATIVE SERVICES","B & H" = "BEACHES & HARBORS",
                   "PUBL" = "PUBLIC", "PRINC" = "PRINCIPAL", "DEP" = "DEPUTY", "COMLPL" = "COMPLIANCE", 
                   "PGM ADMN" = "PROGRAM ADMINISTRATOR", "EQUIP OPER" = "EQUIPMENT OPERATOR", "PRG ADMN" = "PROGRAM ADMINISTRATOR",
                   "PLT OPER" = "PLANT OPERATOR", "INVR" = "INVESTIGATOR", "RES ANALYS" = "RESEARCH ANALYST", "RES" = "RESEARCH",
                   "ASST." = "ASSISTANT", "EXEC." = "EXECUTIVE", "MGR." = "MANAGER", "CONSERV." = "CONSERVATOR",
                   "BOARD OF SUPVRS." = "BOARD OF SUPERVISORS", "H.S." = "HEALTH SERVICES", "SUPVRS." = "SUPERVISORS", 
                   "CHIEF COUNSEL, DISAB." = "CHIEF COUNSEL, DISABILITY", "ADMIN DEPUTY, CHILD" = "ADMINISTRATIVE DEPUTY, CHILD",
                   "TRNG" = "TRAINING", "INT" = "INTERMEDIATE")
  
  abbrev_pattern <- str_c("\\b", names(abbrev_list), "\\b", collapse = "|")
  
  abbrev2long <- function(abbr) {
    abbrev_list[abbr]
  }
  
  # Remove all periods in the titles
  current_titles$TitleLong <- str_remove_all(current_titles$TitleLong, "\\.")
  
  current_titles$TitleLong <- str_replace_all(current_titles$TitleLong,
                                              pattern = abbrev_pattern,
                                              replacement = abbrev2long)
  
  current_titles <<- current_titles

  ##-------- Combine df and adv -----------##

  # First, find the number of current incumbents in each classification to display later
  incumbents <- left_join(jpact_data,
                          title_data[,c("TitleCode", "TitleLong")],  # get title descriptions
                          by = c("TITLE_CD" = "TitleCode")) %>%
    dplyr::filter(SUB_TITLE_CD %in% c("A", "D", "N", "L") &
             !HOME_DEPT_CD %in% c("GJ", "NL", "SC") &
             EMPLMT_STA_CD %in% c("A", "H") &
             EXPIRATION_DT == "12319999")  %>%
    dplyr::select( TITLE_CD, TitleLong) %>%
    dplyr::group_by( TITLE_CD ) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::rename( TitleCode = TITLE_CD )

  class(incumbents) <- "data.frame"
  
  incumbents <<- incumbents

  # Join the data with the title names and subset for records that indicate job movement
  adv <- left_join(jpact_data,
                   title_data[,c("TitleCode", "TitleLong")],  # get title descriptions
                   by = c("TITLE_CD" = "TitleCode")) %>%
    dplyr::filter(nchar(EMPLOYEE_ID) < 7) %>%  # get rid of trailing row
    dplyr::filter(PERS_ACTN_CD %in% c("01", "44", "44A", "44B", "45", "45A", "46", "49", "49A"),  # keep only job movements
           EMPLMT_STA_CD %in% c("A"),  # Historical FTE records
           SUB_TITLE_CD %in% c("A", "D", "N", "L"),     # Permanent subtitle codes
           !HOME_DEPT_CD %in% c("GJ", "NL", "SC") ) %>%
    dplyr::mutate(JobApptDate = mdy(JOB_APPT_DT, tz = "UTC"),  # convert to date format
           JobLastDate = mdy(EXPIRATION_DT, tz = "UTC")) %>%
    dplyr::filter(JobApptDate >= as.POSIXct("2012-04-01", tz = "UTC") ) %>%  # filter after formatting
    dplyr::rename( EmployeeID = EMPLOYEE_ID, TitleCode = TITLE_CD )

  # Make the columns the same in both datasets
  df <- df %>% select(EMPNO, ITEM, ITEM_DESC, JOB_APPT_D, JOB_STOP_D)  # taps.rds
  adv <- adv %>% select(EmployeeID, TitleCode, TitleLong, JobApptDate, JobLastDate)

  colnames(df) <- colnames(adv)  # Use adv column names for df colnames

  # combine datasets
  combined <- rbind(df, adv)

  # Make a reference list of titles in case there's NA values in the data later
  class(combined) <- "data.frame"

  titles_in_data <- combined %>% dplyr::arrange( desc(JobApptDate) )

  titles_in_data <- titles_in_data[ !base::duplicated( titles_in_data[ "TitleCode" ] ), ]

  titles_in_data <<- titles_in_data
  
  rm(adv, df)  # unload the original taps and advantage data

  ##-------- Tidy the Data -----------##
  # Remove whitespace from the title description column
  combined$TitleLong <- str_trim(combined$TitleLong, side = "both")

  # Arrange each employee's items in ascending order by date (earliest at top)
  # And filter to create a 30 year dataset
  combined <- combined %>%     # original assigned to combined_30
    dplyr::group_by(EmployeeID) %>%
    dplyr::arrange(EmployeeID, JobApptDate) %>%
    dplyr::ungroup()

  # Filter the data to create 30 year dataset
  combined_30 <- combined %>%
    dplyr::filter(JobApptDate > as.character( today()-years(30) ) )  # (30 year dataset)  // 400k+

  # Copy and filter the data to create 15 year dataset
  combined_15 <- combined %>%
    dplyr::filter(JobApptDate > as.character( today()-years(15) ) )  # (15 year dataset) // 250k+

  ##----- Apply the Cleaning Rules -----##
  # Use the clean_careers() function to run all the cleaning rules:
  source("./functions/clean_careers.R")
  combined_30 <- clean_careers(combined_30)
  # # tbl_1 <- cbind(beforeRuleStats, rule1stat, rule2stat, rule3stat, rule4stat)  # stats for 30 yr data

  combined_15 <- clean_careers(combined_15)
  # tbl_2 <- cbind(beforeRuleStats, rule1stat, rule2stat, rule3stat, rule4stat)  # stats for 15 yr data

  ##----- Make Item Pairs -----##
  # Use the make_pairs() and make_pairs_rev() functions to create item_pairs for each time data cut
  # Forward
  source("./functions/make_pairs.R")
  item_pairs_30 <- make_pairs(combined_30)
  item_pairs_15 <- make_pairs(combined_15)

  # Reverse
  source("./functions/make_pairs_rev.R")
  item_pairs_30_rev <- make_pairs_rev(combined_30)
  item_pairs_15_rev <- make_pairs_rev(combined_15)
  
  return( list( item_pairs_15 = item_pairs_15,
                item_pairs_15_rev = item_pairs_15_rev,
                item_pairs_30 = item_pairs_30,
                item_pairs_30_rev = item_pairs_30_rev) )

  ##----- Make Item Reference Table -----##
  # source("./functions/make_item_ref.R")
  # item_ref_30 <- make_item_ref(item_pairs_30)
  # item_ref_15 <- make_item_ref(item_pairs_15)
  
  # saveRDS(item_pairs_30, file = "./temp/item_pairs_30.rds")
  # saveRDS(item_pairs_30_rev, file = "./temp/item_pairs_30_rev.rds")
  # saveRDS(item_pairs_15, file = "./temp/item_pairs_15.rds")
  # saveRDS(item_pairs_15, file = "./temp/item_pairs_15_rev.rds")
  # saveRDS(item_ref_30, file = "./temp/item_ref_30.rds")
  # saveRDS(item_ref_15, file = "./temp/item_ref_15.rds")
  # 
  # # Run checks on the data that will display to analyst in desktop app
  # source("./functions/checks_on_data.r")

  # # Copy the old files into a folder called 'temp'
  # # 30 fwd
  # file.copy("./data/Item_Pairs_30.csv",
  #           "./temp/Item_Pairs_30.csv")
  # 
  # file.rename("./temp/Item_Pairs_30.csv",
  #             "./temp/Item_Pairs_30_old.csv")
  # 
  # # 30 reverse
  # file.copy("./data/Item_Pairs_30_rev.csv",
  #           "./temp/Item_Pairs_30_rev.csv")
  # 
  # file.rename("./temp/Item_Pairs_30_rev.csv",
  #             "./temp/Item_Pairs_30_rev_old.csv")
  # 
  # # 15 fwd
  # file.copy("./data/Item_Pairs_15.csv",
  #           "./temp/Item_Pairs_15.csv")
  # 
  # file.rename("./temp/Item_Pairs_15.csv",
  #             "./temp/Item_Pairs_15_old.csv")
  # 
  # # 15 reverse
  # file.copy("./data/Item_Pairs_15_rev.csv",
  #           "./temp/Item_Pairs_15_rev.csv")
  # 
  # file.rename("./temp/Item_Pairs_15_rev.csv",
  #             "./temp/Item_Pairs_15_rev_old.csv")

  # Save the OUTPUT in the 'data' folder

  # readr::write_csv(item_pairs_30, "./data/Item_Pairs_30.csv", na = "")
  # readr::write_csv(item_pairs_15, "./data/Item_Pairs_15.csv", na = "")
  # readr::write_csv(item_pairs_30_rev, "./data/Item_Pairs_30_rev.csv", na = "")
  # readr::write_csv(item_pairs_15_rev, "./data/Item_Pairs_15_rev.csv", na = "")
  
  
}  # Ends function definition









