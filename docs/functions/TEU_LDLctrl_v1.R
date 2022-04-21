# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_LDLctrl_v1 <- function(x) {
      y <- ifelse(x<threshold, 1, 0)
      y <- factor(as.numeric(y), levels=c(0,1), 
                  labels=c("Not controlled", "Controlled"))
      return(y)
    }


