This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_HouseholdIncome <- function(x){
    # XL add: Need to assign variable as factor format first 
    x<-factor(x)
    # Categorise missing data - change levels so specified levels and NA are both "Unanswered"
    labels <- c(levels(x)[-which(levels(x) %in% missingvals)], categ_name)
    y <- as.character(x)
    y[y %in% missingvals] <- categ_name
    y[is.na(y)] <- categ_name
    y <- factor(y, levels=labels, ordered=FALSE)
    return(y)
  }
```


