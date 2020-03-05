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
printlogresults <- function(modeloutput, coefflist=NULL){
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  NOMVAR <- rownames(coeff)
  regression <- data.frame(
    NOMVAR=(rownames(coeff)),
    OR=format(round(exp(coeff[,1]),3), nsmall=3), # OR
    CI=paste0("(",format(round(exp(coeff[,1]-coeff[,2]),2), nsmall=2), ", ",
              format(round(exp(coeff[,1]+coeff[,2]),2),nsmall=2),")"), # 95% CI
    P=formatC(coeff[,4], format="e", digits=3) # p-value
    ) 
  VARIABLE=c("",gsub("[-^0-9]", "", names(unlist(modeloutput$xlevels))))
  MODALITY=c("",as.character(unlist(modeloutput$xlevels)))
  names=data.frame(VARIABLE,MODALITY,NOMVAR=c("(Intercept)",paste(VARIABLE,MODALITY,sep="")[-1]))
  
  results <- merge(names,regression,all.x=TRUE)
  results <- results[match(rownames(coeff), results$NOMVAR),]
  # rows <- rownames(results)
  # levels <- modeloutput$xlevels
  # 
  # variables <- names(modeloutput$xlevels)
  # n <- rep(-1, length(rows))
  # for (row in 1:length(rows)){
  #   for (var in variables){
  #     rows[row] <- str_replace(string=rows[row], pattern=fixed(var), replacement="")
  #     # Count n for factors: 
  #     # modeloutput$data is the dataframe used in the regression model
  #     # count the number of rows where that factor is equal to each value
  #     # n[row] <- max(n[[row]], nrow(modeloutput$data[modeloutput$data[[var]] == rows[row], ]))
  #   }
  # }
  # # results <- cbind(n, results)
  # colnames(results) <- c("Odds Ratio", "95% CI", "p-value")
  # rownames(results) <- rows
  # 
  # if(!is.null(coefflist)){
  #   rs <- data.frame(results, stringsAsFactors=FALSE)
  #   rs$names <- rownames(rs)
  #   tb <- data.frame(coefflist, stringsAsFactors=FALSE)
  #   results <- merge(tb, rs, by.x="coefflist", by.y="names", all.x=TRUE)
  #   results <- results[match(coefflist, results$coefflist),]
  #   
  #   results$Odds.Ratio[is.na(results$Odds.Ratio)] <- "1"
  #   
  #   colnames(results) <- c("Coefficient", "Odds ratio", "95% CI", "p value")
  # }
  return(results)
}
# modeloutput <- model


# VARIABLE=c("",gsub("[-^0-9]", "", names(unlist(modeloutput$xlevels))))
# MODALITY=c("",as.character(unlist(modeloutput$xlevels)))
# names=data.frame(VARIABLE,MODALITY,NOMVAR=c("(Intercept)",paste(VARIABLE,MODALITY,sep="")[-1]))
# regression=data.frame(NOMVAR=names(coefficients(modeloutput)), COEF=as.numeric(coefficients(modeloutput)))
# thing <- merge(names,regression,all.x=TRUE)

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
