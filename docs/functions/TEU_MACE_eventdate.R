# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_MACE_eventdate <- function(data){
      y<-pmin(data$TEU_HES_MACE_fudate,data$TEU_Dth_MACE_dthdate,na.rm = TRUE)
      return(y)
    }


