# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_treatedHTN <- function(data) {
      y <- data[["TEU_selfrepHTN_meds"]]
      y[(data[["TEU_awareHTN"]] == FALSE | is.na(data[["TEU_awareHTN"]]))] <- NA
      return(y)
    }


