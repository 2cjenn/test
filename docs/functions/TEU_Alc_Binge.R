# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_Alc_Binge <- function(data) {
      y <- dplyr::case_when(data[["BaC_Sex"]] == "Female" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 7 ~ TRUE,
                            data[["BaC_Sex"]] == "Male" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 14 ~ TRUE,
                            TRUE ~ FALSE)
    }


