This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_MACE_HaemStroke <- function(data){
      
      HES <- ifelse(data[["TEU_MACE_fucomp"]] %in% c("Nonfatal Stroke - Haemorrhagic"), 1,0)
      death <- ifelse((data[["TEU_MACE_fucomp"]]=="CVD death" & !is.na(data[["TEU_MACE_fucomp"]])) & 
                        data[["TEU_Dth_MACE_dthtype"]] %in% c("Stroke - Haemorrhagic"), 1,0)
      y <- as.numeric(HES|death)
      
    }
```


