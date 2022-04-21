This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_VeI_seriouscomb <- function(data){
      # read in finalised excel mapping
      noncancer=read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))
      # Coding list for serious comorbidities
      serious_comb<-noncancer[which(noncancer$Exclude=='Yes'),]$coding
      y<- FN_VI_filtercodes(dx_codes = serious_comb,
                        colname = "VeI_NonCancer",
                        instance = 0,
                        return_label = "dx",
                        mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
      
      return(y)
    }
```


