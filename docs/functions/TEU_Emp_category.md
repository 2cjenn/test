This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Emp_category <- function(data) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding2_flat_Employment.csv"))
      data <- left_join(data, map[,c("Code", "L0")], by=c("Emp_JobCode.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["L0"]]), 
                                  as.character(data[["TEU_Emp_CurrStat"]]))
      
      y <- dplyr::case_when(
        is.na(data$TEU_EmpCat) ~ "Unemployed/unanswered",
        data$TEU_EmpCat %in% c("Managers and Senior Officials", "Professional Occupations",
                               "Associate Professional and Technical Occupations",
                               "Administrative and Secretarial Occupations") ~ "White collar",
        data$TEU_EmpCat == "Skilled Trades Occupations" ~ "Skilled trades",
        data$TEU_EmpCat %in% c("Personal Service Occupations",
                               "Sales and Customer Service Occupations") ~ "Services",
        data$TEU_EmpCat %in% c("Process, Plant and Machine Operatives",
                               "Elementary Occupations") ~ "Blue collar",
        data$TEU_EmpCat %in% c("Other job (free text entry)",
                               "In paid employment or self-employed") ~ "Other employment",
        data$TEU_EmpCat == "Retired" ~ "Retired",
        data$TEU_EmpCat == "Unable to work because of sickness or disability" ~ "Disability",
        data$TEU_EmpCat %in% c("Looking after home and/or family",
                               "Unemployed", "Full or part-time student",
                               "Doing unpaid or voluntary work",
                               "None of the above", "Prefer not to answer") ~ "Unemployed/unanswered",
        TRUE ~ "Error?"
      )
      y <- factor(y, 
                  levels=c("White collar", "Skilled trades", "Services", 
                           "Blue collar", "Other employment", "Retired", 
                           "Disability", "Unemployed/unanswered"), 
                  labels=c("Professional and Administrative", "Skilled trades", "Services", 
                           "Manual and Industrial", "Other employment", "Retired", 
                           "Unable to work because of sickness or disability", 
                           "Unemployed/unanswered"),
                  ordered=FALSE)
      
    }
```


