library(stringr)

code_filter <- function(df, diagcolname, datecolname=NULL, ncols, codelist, codelength=NA, separator=".", first=FALSE) {
  if (is.na(codelength)) {
    codelength <- nchar(codelist[1])
    print(codelength)
  }
  for (m in 0:ncols) {
    print(m)
    codecol <- paste0(diagcolname, separator, m)
    if (!is.null(datecolname)) {
      datecol <- paste0(datecolname, separator, m)
      df[[datecol]][!substr(df[[codecol]], 1, codelength) %in% codelist] <- NA
    }
    df[[codecol]][!substr(df[[codecol]], 1, codelength) %in% codelist] <- NA
  }
  # To reduce the size of the data set, remove missing observations
  df <- df[rowSums(!is.na(df))>1,]
  df <- df[, unlist(lapply(df, function(x) !all(is.na(x))))]
  # Rearrange into long format
  df <- reshape(df, varying=sort(colnames(df[,-1])), direction="long", idvar="ID", sep=separator)
  df <- df[,!(names(df) == "time")][!is.na(df[[diagcolname]]),]
  # Call the diagnosis code column Code
  names(df)[names(df)==diagcolname] <- "Code"
  # If there's a date column then convert it into date format and call it Date
  if (!is.null(datecolname)) {
    df[[datecolname]] <- as.Date(df[[datecolname]], origin=as.Date("1970-01-01"))
    names(df)[names(df)==datecolname] <- "Date"
    # If the first diagnosis OF ANY CODE per individual is required, filter
    if (first==TRUE) {
      df <- do.call(rbind, by(df, df[,c("ID")], function(x) x[which.min(x$Date),]))
    }
  }
  # If we're only interested in the first diagnosis per individual but there are no dates
  # just take unique diagnoses
  if (first==TRUE) {
    df <- unique(df)
  }
  return(df)
}

diagcollist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

df_to_table <- function(tablist, overwrite) {
  conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")
  for (table in tablist) {
    dbWriteTable(conn, table, eval(as.name(table)), overwrite=overwrite)
  }
  dbDisconnect(conn)
}

# Prettyprint the results from a Cox model
# To use this, 
# model <- coxph(Surv(time_to_dementia, dementia_status) ~ age, data=data)
# kable(printcoxresults(model), caption="")
printcoxresults <- function(modeloutput, loghr=TRUE){
  if(loghr==TRUE){
    results <- summary(modeloutput)$coefficients[,c(1:3,5)]
    colnames(results) <- c("Log HR", "Hazard Ratio", "SE(logHR)", "p-value")
    rows <- rownames(results)
    variables <- names(modeloutput$xlevels)
    for (row in 1:length(rows)){
      for (var in variables){
        rows[row] <- str_replace(string=rows[row], pattern=fixed(var), replacement="")
      }
    }
    rownames(results) <- rows
  } else {
    results <- cbind(summary(modeloutput)$coefficients[,2], summary(modeloutput)$coefficients[,5])
    colnames(results) <- c("Hazard Ratio", "p-value")
  }
  return(results)
}

# Pretty print the results from a logistic regression model
printlogresults <- function(modeloutput){
  results <- cbind(exp(modeloutput$coefficients), summary(modeloutput)$coefficients[,4])
  colnames(results) <- c("Odds Ratio", "p-value")
  rows <- rownames(results)
  variables <- names(modeloutput$xlevels)
  for (row in 1:length(rows)){
    for (var in variables){
      rows[row] <- str_replace(string=rows[row], pattern=fixed(var), replacement="")
    }
  }
  rownames(results) <- rows
  return(results)
}

# Round to the nearest m
mround <- function(x, base){
  base*round(x/base)
}


# Make a pretty proportion table
# To use this,
# tab <- table(data$VIhyp, data$prevHBP, useNA='ifany')
# kable(propped(tab), caption="")
propped <- function(table, margin=NULL) {
  prop <- round(100*prop.table(table, margin=margin),2)
  tabsums <- addmargins(table)
  for(i in c(1:dim(table)[1])) {
    for(j in c(1:dim(table)[2])) {
      tabsums[i,j] <- paste0(table[i,j], " (", prop[i,j], "%)")
    }
  }
  return(tabsums)
}
