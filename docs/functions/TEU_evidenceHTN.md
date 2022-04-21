This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_evidenceHTN <- function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]] | data[["TEU_BlP_measuredHTN"]])
    }
```


