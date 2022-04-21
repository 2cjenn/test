This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Dth_MACE_dthtype <- function(data){
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
      ICD10_codes<-mapping$Code[!is.na(mapping$ConditionsType)]
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_code', record_level=record_level)(data)

      y <- as.character(sapply(y, function(i) { if(!is.na(i)){mapping$ConditionsType[mapping$Code==i]}}))
      y <- str_remove(y, "Nonfatal ")
      
      return(y)
    }
```


