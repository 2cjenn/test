# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_HMH_BowelCancerScreen <- function(x) {
      y <- as.character(x)
      y[y %in% c("Prefer not to answer", "Do not know") |
          is.na(y)] <- "Unanswered"
      y <- factor(
        y,
        levels = c("Yes", "No", "Unanswered"),
        labels = c(
          "Screened for bowel cancer",
          "Not screened for bowel cancer",
          "Unanswered"
        ),
        ordered = FALSE
      )
      return(y)
    }


