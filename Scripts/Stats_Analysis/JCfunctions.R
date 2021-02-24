library(stringr)
options("scipen"=100)

# Run chunks of R code in arbitrary working directory
# From Hadley Wickham
# https://github.com/yihui/knitr/issues/38
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  
  force(code)
}

pretty_dp <- function(x, dp=0, pct=FALSE, comma=FALSE){
  if(pct){x <- 100*x}
  if(comma){
    format(round(x, dp), digits=dp, nsmall=dp, big.mark=",") %>% trimws
  } else {
    format(round(x, dp), digits=dp, nsmall=dp) %>% trimws
  }
}

pretty_confint <- function(lci, uci, dp, pct=FALSE){
  paste0("[", pretty_dp(x=lci, dp=dp, pct=pct), ", ", pretty_dp(x=uci, dp=dp, pct=pct), "]")
}

pretty_pval <- function(p, cutoff=0.001, string="<0.001", dp=3){
  ifelse(p<cutoff, string, pretty_dp(p, dp))
}

lower <- function(x){
  paste0(tolower(substring(x, 1,1)), substring(x, 2))
}

upper <- function(x){
  paste0(toupper(substring(x, 1,1)), substring(x, 2))
}

prettyfunc <- function(x, pnames=list(), upper=FALSE, bold=FALSE, flist=c()){
  out <- x
  if(x %in% names(pnames)){
    out <- pnames[[x]]
    if(upper){
      out <- upper(out)
    }
    if(bold){
      out <- paste0("**", out, "**")
    }
  }
  if(x %in% flist){
    if(!exists("footnote_no")){
      footnote_no <<- 1
    }
    out <- paste0(out, "^", footnote_no, "^")
    footnote_no <<- footnote_no + 1
  }
  return(out)
}

diagcollist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

# Print numbers and proportions for factors, median and IQR or mean and 95% CI for continuous variables
# Optionally provide p-values from chi-squared (categorical) and t-test (continuous)
descriptivetable <- function(df, varlist, contavg='mean', assocvar=NULL, pretty_names=list(), footnote_list=c()){
  if(!exists("footnote_no")){footnote_no <<- 1} # Note use of <<- instead of <- to assign this globally
  outtable <- c()
  for(var in varlist){
    if(is.factor(df[[var]])){ # Categorical variables (factors) need a row per level, n's and %'s
      n <- table(df[[var]], useNA='ifany')
      pct <- pretty_dp(prop.table(n), dp=1, pct=TRUE)
      variable <- c(prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list))
      levels <- names(n)
      if(!is.null(assocvar)){
        tab <- table(df[[assocvar]], df[[var]])
        chi <- chisq.test(tab)
        pval <- c(ifelse(chi$p.value<0.001, "<0.001", round(chi$p.value,3)))
        outtable <- rbind(outtable, 
                          c(var, paste0("**", variable, "**"), "", "", pval), 
                          cbind(paste0(var, levels), levels, n, pct, ""))
      } else{
        outtable<- rbind(outtable, 
                         c(var, paste0("**", variable, "**"), "", ""), 
                         cbind(paste0(var, levels), levels, n, pct))
      }
    } else { # Continuous variables need the mean (and SD) or median (and IQR)
      if(contavg=="mean"){
        n <- pretty_dp(mean(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- pretty_dp(sd(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        variable <- paste0("Mean ", prettyfunc(var, pretty_names, upper=FALSE, flist=footnote_list), " (SD)")
      } else if (contavg=="median"){
        n <- pretty_dp(median(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        IQR <- pretty_dp(quantile(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- paste0("(", IQR[2], "-", IQR[4], ")")
        variable <- paste0("Median ", prettyfunc(var, pnames=pretty_names, upper=FALSE, flist=footnote_list), " (IQR)")
      } else if(contavg=="n"){
        n <- nrow(df[!is.na(df[[var]]),])
        pct <- NA
        variable <- prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list)
      }
      if(!is.null(assocvar)){
        tt <- t.test(df[[var]][df[[assocvar]]==TRUE], df[[var]][df[[assocvar]]==FALSE])
        pval <- pretty_pval(tt$p.value)
        outtable <- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, pval))
      } else {
        outtable<- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct))
      }
    }
  }
  rownames(outtable) <- c()
  if(!is.null(assocvar)){
    colnames(outtable) <- c("IDcol", "Variable", "n", "%", "p")
  } else {
    colnames(outtable) <- c("IDcol", "Variable", "n", "%")
    }
  outdf <- as.data.frame(outtable, stringsAsFactors=FALSE)
  return(outdf)
}

# Prettyprint the results from a Cox model
# To use this, 
# model <- coxph(Surv(time_to_dementia, dementia_status) ~ age, data=data)
# kable(printcoxresults(model), caption="")
printcoxresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  conf <- summ$conf.int
  
  regression <- data.frame(
    IDcol=rownames(coeff),
    HR=pretty_dp(coeff[,2], dp=2), # HR
    CI=pretty_confint(conf[,3], conf[,4], dp=2), # 95% CI
    p=pretty_pval(coeff[,5]), # p-value
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
  } else {
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  results <- results[,c(coeffcols, "HR", "CI", "p")]
  names(results) <- c(coeffcols, "HR", "95% CI", "p")
    
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}

# Pretty print the results from a logistic regression model
printlogresults <- function(model, coeffnames=NULL, IDcol=FALSE){
  summ <- summary(model)
  coeff <- summ$coefficients
  # NOMVAR <- rownames(coeff)
  regression <- data.frame(
    IDcol=(rownames(coeff)),
    OR=pretty_dp(exp(coeff[,1]), dp=2), # OR
    CI=pretty_confint(exp(coeff[,1]-(1.96*coeff[,2])),
                      exp(coeff[,1]+(1.96*coeff[,2])),
                      dp=2), # 95% CI
    p=pretty_pval(coeff[,4]), # p-value
    stringsAsFactors=FALSE
  )
  if(is.null(coeffnames)){
    results <- regression
    names(results) <- c("Coefficient", "OR", "95% CI", "p")
  } else {
    results <- merge(coeffnames, regression, all.x=TRUE)
    results$OR[is.na(results$OR)] <- "1"
    results <- results[match(coeffnames$IDcol, results$IDcol),]
    
    coeffcols <- colnames(coeffnames)
    if(IDcol==FALSE){
      coeffcols <- coeffcols[coeffcols != "IDcol"]
    }
    results <- results[,c(coeffcols, "OR", "CI", "p")]
    names(results) <- c(coeffcols, "OR", "95% CI", "p")
  }
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
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
  dimt <- dim(table)
  for(i in c(1:dimt[1])) {
    if(length(dimt)>1){
      for(j in c(1:dimt[2])) {
        tabsums[i,j] <- paste0(table[i,j], " (", prop[i,j], "%)")
      }
    }
    else{
      tabsums[i] <- paste0(table[i], " (", prop[i], "%)") 
    }
  }
  return(tabsums)
}

# Check correlation among all pairs of covariates in a given dataframe 
# (this will need extending to be more robust if I want to use it for non-categorical covariates)
corr_mat <- function(data){
  corr_matrix <- matrix(0L, nrow=ncol(data), ncol=ncol(data))
  colnames(corr_matrix) <- colnames(data)
  rownames(corr_matrix) <- colnames(data)
  for(i in c(1:(ncol(data)-1))){
    x <- data[[i]]
    for(j in c((i+1):ncol(data))){
      y <- data[[j]]
      if(is.factor(x) & is.factor(y)){
        corr_matrix[i, j] <- (cramerV(table(x, y)))
      } else if (is.numeric(x) & is.numeric(y)) {
        corr_matrix[i, j] <- cor(x, y, method="pearson")
      } else {
        print("Unanticipated type")
      }
    }
  }
  return(corr_matrix)
}

preparecoefflist_1col <- function(df, varname, pretty_names=list()){
  pretty_varname <- prettyfunc(varname, pnames=pretty_names, bold=TRUE, upper=TRUE)
  if(is.factor(df[[varname]])){
    levels <- levels(df[[varname]])
    variable <- c(pretty_varname, levels)
    coeffname <- c(pretty_varname, paste0(varname,levels))
  } else {
    variable <- pretty_varname
    coeffname <- varname
  }
  output <- data.frame(coeffname, variable, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient")
  rownames(output) <- NULL
  return(output)
}

preparecoefflist_2col <- function(df, varname, pretty_names=list()){
  pretty_varname <- prettyfunc(varname, pnames=pretty_names, upper=TRUE)
  if(is.factor(df[[varname]])){
    levels <- levels(df[[varname]])
    variable <- c(pretty_varname,
                  rep(NA, length(levels)-1))
    coeffname <- paste0(varname,levels)
  } else {
    levels <- NA
    variable <- pretty_varname
    coeffname <- varname
  }
  output <- data.frame(coeffname, variable, levels, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient", "Levels")
  rownames(output) <- NULL
  return(output)
}

preparecoefflist <- function(onecol=FALSE, ...){
  if(onecol) {
    preparecoefflist_1col(...)
  } else {
    preparecoefflist_2col(...)
  }
}

regressiontable <- function(df, outcome, varlist, regresstype, adjvarlist=c("agegrp", "gender"), pretty_names=list(), IDcol=TRUE){
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names)
  }
  
  if(regresstype=="univariable"){
    modellist <- list()
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    # Vertically concatenate all the pretty outputs into one output table
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="adjusted"){
    modellist <- list()
    
    # Run the regressions for age and gender separately to go on top of the table
    for(adjvar in adjvarlist){
      coeffnames <- preparecoefflist(df=df, varname=adjvar)
      
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"))
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[adjvar]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    
    # Putting age or gender in the regression twice would confuse it, so make sure they're not in the varlist
    varlist <- varlist[!varlist %in% adjvarlist]
    
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"), "+", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="multivariable"){
    coeffnames <- do.call(rbind, coefflist)
    formula <- paste0(outcome, " ~ ", paste(varlist, collapse=" + "))
    model <- glm(formula, data=df, family="binomial")
    outdf <- printlogresults(model, coeffnames, IDcol=IDcol)
  }
  
  rownames(outdf) <- NULL
  return(outdf)
}


