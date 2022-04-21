# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_Edu_ISCED <- function(x) {
      # Convert UKB qualification categories into ISCED education categories
      y <- dplyr::case_when(
        x == "College or University degree" ~ "5: Tertiary",
        x == "NVQ or HND or HNC or equivalent" ~ "5: Tertiary",
        x == "Other professional qualifications eg: nursing, teaching" ~ "4: Post-secondary non-tertiary",
        x == "A levels/AS levels or equivalent" ~ "2-3: Secondary",
        x == "O levels/GCSEs or equivalent" ~ "2-3: Secondary",
        x == "CSEs or equivalent" ~ "2-3: Secondary",
        x == "None of the above" ~ "1: Primary",
        x == "Prefer not to answer" ~ "Unanswered",
        is.na(x) ~ NA_character_
      )
      y <- factor(
        y,
        levels = c(
          "5: Tertiary",
          "4: Post-secondary non-tertiary",
          "2-3: Secondary" ,
          "1: Primary" ,
          "Unanswered"
        )
      )
      
      return(y)
    }


