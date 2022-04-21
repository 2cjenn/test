# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_MACE_time_yrs <- function(x){
      as.numeric(round(x/365.25, digits = 2)) 
    }


