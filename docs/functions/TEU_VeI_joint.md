This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_VeI_joint <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }
```


