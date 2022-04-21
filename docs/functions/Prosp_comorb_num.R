# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


Prosp_comorb_num <- function(data){
      rowSums(sapply(select(data,everything()),function(x) grepl("Yes",x)))
    }


