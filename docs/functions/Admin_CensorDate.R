# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


Admin_CensorDate <- function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml"))
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        y <- do.call(c, datelist)
      }
      else {
        y <- dplyr::case_when(
          x=='England' ~ FN_toDate('2017-03-31'),
          x=='Scotland' ~ FN_toDate('2016-10-31'),
          x=='Wales' ~ FN_toDate('2016-02-29')
        )
      } 
      return(y)
    }


