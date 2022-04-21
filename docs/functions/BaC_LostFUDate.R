# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


BaC_LostFUDate <- function(x){
  as.Date(x, origin=as.Date("1970-01-01"))
}


