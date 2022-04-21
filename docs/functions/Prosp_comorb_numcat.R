# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


Prosp_comorb_numcat <- function(x){
      factor(ifelse(x>=3,'>=3',x),levels = c('0','1','2','>=3'),ordered = FALSE)
    }


