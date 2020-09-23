# Jennifer Collister
# 22/09/20

derive_variables <- function(indata, colnames){
  names(common) <- sapply(common, function(x) x$name)
  outdata <- indata
  for(colname in cols){
    colinfo <- common[[colname]]
    colfunc <- colinfo$mapper
    if(length(colinfo$source)>1){
      outdata[[colinfo$name]] <- colfunc(indata)
    } else {
      outdata[[colinfo$name]] <- colfunc(indata[[colinfo$source]])
    }
    print(colinfo$name)
    print(colinfo$description)
  }
  outdata <- outdata[,cols]
  return(outdata)
}