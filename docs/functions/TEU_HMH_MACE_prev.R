# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_HMH_MACE_prev <- function(data){
      y<-FN_Vascular_condition(condition=c("Stroke", "Heart attack"), string="MACE")(data)
      levels(y)<-c('Yes','No','Unanswered')
      return(y)
      }


