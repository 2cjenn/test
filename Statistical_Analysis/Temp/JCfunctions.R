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

# Print numbers and proportions for factors, median and IQR or mean and 95% CI for continuous variables
# Optionally provide p-values from chi-squared (categorical) and t-test (continuous)
descriptivetable <- function(df, varlist, contavg='mean', assocvar=NULL, pretty_names=NULL){
  if(is.null(pretty_names)){
    pretty_names <- setNames(varlist, varlist)
  }
  outtable <- c()
  for(var in varlist){
    if(is.factor(df[[var]])){
      n <- table(df[[var]], useNA='ifany')
      pct <- format(round(100*prop.table(n),2), digits=1, nsmall=1)
      variable <- c(pretty_names[[var]], rep(NA, dim(n)-1))
      levels <- names(n)
      if(!is.null(assocvar)){
        tab <- table(df[[assocvar]], df[[var]])
        chi <- chisq.test(tab)
        pval <- c(ifelse(chi$p.value<0.001, "<0.001", round(chi$p.value,3)), rep(NA, dim(n)-1))
      }
    } else {
      if(contavg=="mean"){
        n <- format(round(mean(df[[var]], na.rm=TRUE),2), digits=1, nsmall=1)
        pct <- format(round(sd(df[[var]], na.rm=TRUE),2), digits=1, nsmall=1)
        variable <- paste0("Mean ", pretty_names[[var]], " (SD)")
      } else if (contavg=="median"){
        n <- format(round(median(df[[var]], na.rm=TRUE),2), digits=1, nsmall=1)
        IQR <- format(round(quantile(df[[var]], na.rm=TRUE),1), digits=1, nsmall=1)
        pct <- paste0("(", IQR[2], "-", IQR[4], ")")
        variable <- paste0("Median ", pretty_names[[var]], " (IQR)")
      }
      levels <- NA
      if(!is.null(assocvar)){
        tt <- t.test(df[[var]][df[[assocvar]]==TRUE], df[[var]][df[[assocvar]]==FALSE])
        pval <- ifelse(tt$p.value<0.001, "<0.001", round(tt$p.value,3))
      }
    }
    if(!is.null(assocvar)){
      outtable <- rbind(outtable, cbind(variable, levels, n, pct, pval))
    } else {
      outtable<- rbind(outtable, cbind(variable, levels, n, pct))
    }
  }
  rownames(outtable) <- c()
  colnames(outtable) <- c("Variable", "Levels (for categorical)", "n", "%")
  return(outtable)
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
printlogresults <- function(model, coeffnames=NULL, IDcol=FALSE){
  summ <- summary(model)
  coeff <- summ$coefficients
  # NOMVAR <- rownames(coeff)
  regression <- data.frame(
    coeffname=(rownames(coeff)),
    OR=format(round(exp(coeff[,1]),3), digits=3, nsmall=3), # OR
    CI=paste0("(",format(round(exp(coeff[,1]-(1.96*coeff[,2])),2), digits=2, nsmall=2), ", ",
              format(round(exp(coeff[,1]+(1.96*coeff[,2])),2), digits=2, nsmall=2),")"), # 95% CI
    # p=formatC(coeff[,4], format="e", digits=3), # p-value
    p=ifelse(coeff[,4]<0.001, "<0.001", format(round(coeff[,4],3), digits=3, nsmall=3)), # p-value
    stringsAsFactors=FALSE
    )
  if(!is.null(coeffnames)){
    results <- merge(coeffnames, regression, all.x=TRUE)
    results$OR[is.na(results$OR)] <- "1"
    results <- results[match(coeffnames$coeffname, results$coeffname),]
    if(IDcol==TRUE){
      results <- results[,c("coeffname", "variable", "levels", "OR", "CI", "p")]
      names(results) <- c("IDcol", "Coefficient", "Level", "OR", "95% CI", "p")
    } else {
      results <- results[,c("variable", "levels", "OR", "CI", "p")]
      names(results) <- c("Coefficient", "Level", "OR", "95% CI", "p")
    }
  } else {
    results <- regression
    names(results) <- c("Coefficient", "OR", "95% CI", "p")
  }
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  # VARIABLE=c("",gsub("[-^0-9]", "", names(unlist(modeloutput$xlevels))))
  # MODALITY=c("",as.character(unlist(model$xlevels)))
  # names=data.frame(VARIABLE,MODALITY,NOMVAR=c("(Intercept)",paste(VARIABLE,MODALITY,sep="")[-1]))

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


preparecoefflist <- function(df, varname){
  if(is.factor(df[[varname]])){
    levels <- levels(df[[varname]])
    variable <- c(varname,rep(NA, length(levels)-1))
    coeffname <- paste0(varname,levels)
  } else {
    levels <- NA
    variable <- varname
    coeffname <- varname
  }
  return(data.frame(coeffname, variable, levels, stringsAsFactors=FALSE))
}

regressiontable <- function(df, outcome, varlist, regresstype, adjvarlist=c("agegrp", "gender")){
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var)
  }
  
  if(regresstype=="univariable"){
    modellist <- list()
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome," ~ ", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=TRUE)
    }
    # Vertically concatenate all the pretty outputs into one output table
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="adjusted"){
    modellist <- list()
    
    # Run the regressions for age and gender separately to go on top of the table
    for(adjvar in adjvarlist){
      coeffnames <- preparecoefflist(df=df, varname=adjvar)
      
      formula <- paste0(outcome," ~ ", paste(adjvarlist, collapse="+"))
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[adjvar]] <- printlogresults(model, coeffnames, IDcol=TRUE)
    }
    
    # Putting age or gender in the regression twice would confuse it, so make sure they're not in the varlist
    varlist <- varlist[!varlist %in% adjvarlist]
    
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome," ~ ", paste(adjvarlist, collapse="+"), "+", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=TRUE)
    }
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="multivariable"){
    coeffnames <- do.call(rbind, coefflist)
    formula <- paste0("controlled ~ ", paste(varlist, collapse=" + "))
    model <- glm(formula, data=df, family="binomial")
    outdf <- printlogresults(model, coeffnames, IDcol=TRUE)
  }
  
  rownames(outdf) <- NULL
  return(outdf)
}