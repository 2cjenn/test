# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_BSM_WaistCircCat <- function(data) {
      # Categorise waist circ into labelled categories
      y <- dplyr::case_when(
        data[["BaC_Sex.0.0"]] == "Female" & data[["BSM_Waist.0.0"]] >= 88 ~ "Obese",
        data[["BaC_Sex.0.0"]] == "Female" & data[["BSM_Waist.0.0"]] >= 80 ~ "Overweight",
        data[["BaC_Sex.0.0"]] == "Male" & data[["BSM_Waist.0.0"]] >= 102 ~ "Obese",
        data[["BaC_Sex.0.0"]] == "Male" & data[["BSM_Waist.0.0"]] >= 94 ~ "Overweight",
        is.na(data[["BSM_Waist.0.0"]]) ~ "Unknown",
        TRUE ~ "Normal"
      )
      y <-
        factor(y, levels = c("Normal", "Overweight", "Obese", "Unknown"))
      return(y)
    }


