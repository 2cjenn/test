# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_VeI_numHTNmedscat <- function(x) {
      y <- dplyr::case_when(
        is.na(x) ~ "None reported",
        x == 0 ~ "None reported",
        x == 1 ~ "1",
        x == 2 ~ "2",
        x >= 3 ~ "3 or more",
        TRUE ~ as.character(x)
      )
      y <- factor(y, levels=c("None reported", "1", "2", "3 or more"))
    }


