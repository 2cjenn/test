# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_BSM_BMIcat <- function(x) {
      y <-
        as.character(cut(x, breaks = c(0, 18.5, 25, 30, 200), right = FALSE))
      y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)", "Unknown"),
          labels = c("Normal", "Underweight", "Overweight", "Obese", "Unknown")
        )
      return(y)
    }


