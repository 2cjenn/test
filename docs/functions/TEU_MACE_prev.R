# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_MACE_prev <- function(data){
      y<-factor(apply(data,1, function(x) any(x %in% 'Yes')),levels = c('FALSE','TRUE'),labels = c('No','Yes'))
      return(y)
    }


