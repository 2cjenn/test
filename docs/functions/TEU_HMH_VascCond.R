# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_HMH_VascCond <- function(data) {
  # Combine the vascular condition columns
  vcon <- coalesce(data[["HMH_HeartProbs.0.0"]], data[["HMH_HeartProbs.0.1"]], 
                   data[["HMH_HeartProbs.0.2"]], data[["HMH_HeartProbs.0.3"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
  y <- dplyr::case_when(
    is.na(vcon) ~ "Unanswered",
    vcon == "None of the above" ~ "No",
    vcon == "Do not know" ~ "Do not know",
    vcon == "Prefer not to answer" ~ "Prefer not to answer",
    vcon %in% condlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer", "Unanswered"))
  return(y)
}


