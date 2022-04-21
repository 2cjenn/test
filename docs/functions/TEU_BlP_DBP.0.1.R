# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_BlP_DBP.0.1 <- function(data) {
      coalesce(data[["BlP_DBPAuto.0.1"]], data[["BlP_DBPMan.0.1"]])
    }


