# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_BlP_nSBP <- function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1")]))
    }


