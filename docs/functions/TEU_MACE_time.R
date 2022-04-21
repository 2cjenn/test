# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_MACE_time <- function(data){
      
      data=data%>%
        mutate(time=case_when(
          TEU_MACE_status==0 ~ as.numeric(difftime(TEU_MACE_censordate, Rec_DateAssess, unit='days')),
          TEU_MACE_status==1 ~ as.numeric(difftime(TEU_MACE_eventdate, Rec_DateAssess, unit='days'))))
      
      return(data$time)
      
    }


