This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
Emp_JobCode.0.0 <- function(x) {
      v2_emp <- DBfunc$DB_extract(extract_cols = c("ID", "Emp_JobCode.0.0"),
                                  db = "K:/TEU/UKB33952_Data/Data_Downloads/V2_database_duckdb0.2.1/ukb_v2.db",
                                  name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")
      y <- v2_emp[["Emp_JobCode.0.0"]][match(x, v2_emp$ID)]
      y <- as.numeric(y)
      return(y)
    }
```


