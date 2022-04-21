This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alc_WeeklyCat <- function(data) {
      cat <- cut(x=data[["TEU_Alc_WeeklyAlcUnits"]],
                 breaks=c(-1, 0, 5, 10, 20, 30, 100),
                 labels=c("None reported", "Less than 5 units", "5 to 10 units", 
                          "10 to 20 units", "20 to 30 units", "30 units or more"),
                 right=FALSE)
      cat[is.na(data[["Alc_Freq.0.0"]])] <- "None reported"
      cat[data[["Alc_Freq.0.0"]] %in% c("Never", "Special occasions only", "Prefer not to answer")] <- "None reported"
      return(cat)
      }
```


