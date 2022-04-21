This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_selfrepHTN_meds <- function(data) {
      VI <- data[["TEU_VeI_HTNmeds_rubric"]]
      VI[is.na(VI)] <- FALSE
      TQ <- (data[["TEU_HMH_Meds_BP"]] == "Self-reported BP meds")
      y <- (VI | TQ)
      return(y)
    }
```


