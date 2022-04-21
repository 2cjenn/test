# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_TownsendDepInd_Quint <- function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    y <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
    if(any(is.na(y))) {
      levels(y) <- c(levels(y), recodeNA)
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }


