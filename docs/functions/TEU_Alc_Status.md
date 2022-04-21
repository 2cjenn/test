This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alc_Status <- function(x) {
      y <- FN_MissingCategory(missingvals = c("Prefer not to answer"), categ_name = "Unanswered")(x)
      y <- FN_factor(levelorder = c("Never", "Previous", "Current", "Prefer not to answer"))(y)
      }
```


