This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_MACE_time_yrs <- function(x){
      as.numeric(round(x/365.25, digits = 2)) 
    }
```


