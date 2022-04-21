# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_MACE_MI <- function(data){
      
      HES <- ifelse(data[["TEU_MACE_fucomp"]] %in% c("Nonfatal MI",
                                                     "MI prevention procedure"), 1,0)
      death <- ifelse((data[["TEU_MACE_fucomp"]]=="CVD death" & !is.na(data[["TEU_MACE_fucomp"]])) & 
                        data[["TEU_Dth_MACE_dthtype"]] %in% c("MI"), 1,0)
      y <- as.numeric(HES|death)
      
    }


