# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_selfrepHTN_dx <- function(data) {
      VI <- !is.na(data[["TEU_VeI_HTN"]])
      TQ <- (data[["TEU_HMH_prevHTN"]] == "Self-reported hypertension")
      y <- (VI | TQ)
      return(y)
    }


