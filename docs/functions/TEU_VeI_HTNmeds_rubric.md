This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_VeI_HTNmeds_rubric <- function(x) {
      rubric <- readRDS(file.path(config$data$derived, "HTNMedsRubric.rds"))
      y <- rubric[["HTN_probablemeds"]][match(x, rubric$ID)]
    }
```


