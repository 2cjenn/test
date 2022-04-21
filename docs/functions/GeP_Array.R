# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


GeP_Array <- function(x){
      coding <- read.csv(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- coding$L0[match(x, coding$Code)]
    }


