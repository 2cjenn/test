# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_FaH_CVD <- function(data){
    y <- apply(data[,c(grep("FaH_FatherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_MotherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_SibIll.0.", colnames(data), fixed=TRUE)
    )
    ], 1, function(x) any(x %in% conditions)
    )
    y <- factor(as.numeric(y), levels=c(0,1), 
                labels=c(paste0("No family history of ", label), paste0("Family history of ", label)))
    return(y)
  }


