# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_awareHTN <- function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]])
      y[(data[["TEU_evidenceHTN"]] == FALSE | is.na(data[["TEU_evidenceHTN"]]))] <- NA
      return(y)
    }


