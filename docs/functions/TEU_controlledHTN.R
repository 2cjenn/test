# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_controlledHTN <- function(data) {
      y <- !data[["TEU_BlP_measuredHTN"]]
      y[(data[["TEU_treatedHTN"]] == FALSE | is.na(data[["TEU_treatedHTN"]]))] <- NA
      return(y)
    }


