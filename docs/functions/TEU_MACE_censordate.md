This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_MACE_censordate <- function(data){
      y<-pmin(data$TEU_Dth_NotMACE_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
      return(y)
    }
```


