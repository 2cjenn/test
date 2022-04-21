# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_Edu_HighestQual <- function(data) {
      qual_list <- c(
        "Prefer not to answer",
        "None of the above",
        "CSEs or equivalent",
        "O levels/GCSEs or equivalent",
        "A levels/AS levels or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "NVQ or HND or HNC or equivalent",
        "College or University degree" 
      )
      for(i in seq(1, length(qual_list), by=1)) {
        data[data == qual_list[i]] <- as.character(i)
      }
      y <- do.call(pmax, c(data, list(na.rm=TRUE)))
      y[is.na(y)] <- 1
      y <- factor(y,
                  levels = seq(length(qual_list), 1, by=-1),
                  labels = qual_list[seq(length(qual_list), 1, by=-1)]
                  )
      return(y)
    }


