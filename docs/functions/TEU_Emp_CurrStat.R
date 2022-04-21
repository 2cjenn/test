# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_Emp_CurrStat <- function(data) {
      # XL change: Changed the order of uncorrected and corrected
      y <- factor(coalesce(as.character(data[["Emp_CurrStat.0.0"]]), 
                           as.character(data[["Emp_CurrStatUnc.0.0"]])),
                  ordered=FALSE)
      return(y)
    }


