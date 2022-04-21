# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_Dth_MACE_dthdate <- function(data){
        mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
        ICD10_codes<-mapping[which(mapping$Conditions=='MACE'),]$Code
        
        y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level)(data)
        
        return(y)
      }


