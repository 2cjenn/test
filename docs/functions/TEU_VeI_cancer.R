# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_VeI_cancer <- function(data){
      
      cancer_mapper=read.csv_kdrive(file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))
      cancer_codes=cancer_mapper[-which(cancer_mapper$L0%in% c('skin cancer')),]$Code
      
      y<- FN_VI_filtercodes(dx_codes = cancer_codes,
                            colname = "VeI_Cancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))(data)
      return(y)
    }


