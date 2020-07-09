#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 14/11/2019
# Define and refine BP variables by regressing against stroke outcomes
#--------------------------------------------------------------------------------------------------------------

library(RSQLite)
library(epitools)
library(reshape2)
library(dplyr)
library(survival)
library(survminer)
library(car)

setwd("K:/TEU/APOE on Dementia/Data Management")
rm(list=(ls()[ls()!="bd"]))

conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")

# Investigate the list of tables
dbListTables(conn)
# and the columns of data in a table
dbGetQuery(conn, "PRAGMA table_info(BlP_base)")

#--------------------------------------------------------------------------------------------------------------
# BLOOD PRESSURE
dbExecute(conn, "DROP VIEW IF EXISTS BP_LONG")
dbExecute(conn, "CREATE VIEW BP_LONG AS
  SELECT ID, '0' AS MEASUREMENT, `BlP_SBPMan.0` AS SBP_M, `BlP_SBPAuto.0` AS SBP_A, `BlP_DBPMan.0` AS DBP_M, `BlP_DBPAuto.0` AS DBP_A FROM BlP_base
  UNION ALL 
  SELECT ID, '1' AS MEASUREMENT, `BlP_SBPMan.1` AS SBP_M, `BlP_SBPAuto.1` AS SBP_A, `BlP_DBPMan.1` AS DBP_M, `BlP_DBPAuto.1` AS DBP_A FROM BlP_base
  ORDER BY ID")
dbExecute(conn, "DROP TABLE IF EXISTS BP")
dbExecute(conn, "CREATE TABLE BP AS
          SELECT ID, CASE WHEN SBP_A  = 'NA' THEN SBP_M ELSE SBP_A END AS SBP, CASE WHEN DBP_A = 'NA' THEN DBP_M ELSE DBP_A END AS DBP FROM
          (SELECT ID, AVG(SBP_M) AS SBP_M, AVG(SBP_A) AS SBP_A, AVG(DBP_M) AS DBP_M, AVG(DBP_A) AS DBP_A FROM BP_LONG GROUP BY ID)")
# dbGetQuery(conn, "SELECT * FROM BP")
# dbDisconnect(conn)

dbExecute(conn, "DROP TABLE IF EXISTS BPVARS")
dbExecute(conn, "CREATE TABLE BPVARS AS
          SELECT ID, SBP, DBP, 
          CASE WHEN SBP<=120 THEN 'NORMAL' WHEN SBP>120 AND SBP<=140 THEN 'HIGH' WHEN SBP > 140 THEN 'VERY HIGH' END AS S_STAT,
          CASE WHEN DBP<=80 THEN 'NORMAL' WHEN DBP>80 AND DBP<=95 THEN 'HIGH' WHEN DBP>95 THEN 'VERY HIGH' END AS D_STAT
          FROM BP")
bpvars <- dbGetQuery(conn, "SELECT * FROM BPVARS")
# Exclude individuals with missing BP data
bpvars <- bpvars[rowSums(!is.na(bpvars[,-1])) !=0,]
# n=30,116

# Make the categorical variables into factors
bpvars$S_STAT <- factor(bpvars$S_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=FALSE)
bpvars$D_STAT <- factor(bpvars$D_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=FALSE)
# And ordered factors
bpvars$S_STATo <- factor(bpvars$S_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=TRUE)
bpvars$D_STATo <- factor(bpvars$D_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=TRUE)

#--------------------------------------------------------------------------------------------------------------
# STROKE DIAGNOSES

conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")

# Get a dataframe of the HES diagnoses
# HES <- dbGetQuery(conn, "SELECT * FROM HES")

# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61", "I62")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
stroke_ICD9 <- c("430", "431", "432")


# Function to filter HES diagnosis datasets
stroke_only <- function(df, diagcolname, datecolname=NULL, ncols, codelist, codelength=NA, separator=".", first=FALSE) {
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

# ICD 10 codes
# # Primary
ICD10Prim <- HES[,c(1,grep("ICD10Main", colnames(HES), fixed=TRUE))]
ICD10Prim <- code_filter(df=ICD10Prim, diagcolname="HES_ICD10Main", datecolname="HES_ICD10MainDateFirst", ncols=65, codelist=stroke_ICD10, separator=".m", first=TRUE)
# Secondary
ICD10Sec <- HES[,c(1,grep("ICD10Sec", colnames(HES), fixed=TRUE))]
ICD10Sec <- code_filter(df=ICD10Sec, diagcolname="HES_ICD10Secondary", ncols=183, codelist=stroke_ICD10, separator=".m", first=TRUE)
# Primary and secondary diagnoses, i.e. including codes from HES data with level=1 or level=2
# See resource 141140 https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=141140
# ICD10Diag <- HES[,c(1,grep("ICD10Diag", colnames(HES), fixed=TRUE), grep("ICD10DateFirst", colnames(HES), fixed=TRUE))]
# If you only want a particular type of diagnosis:
collist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}
ICD10Diag <- dbGetQuery(conn, paste0("SELECT ID, ", collist("HES_ICD10Diag", ".m", 212), ", ", collist("HES_ICD10DateFirst", ".m", 212), "FROM HES"))
ICD10DiagS <- stroke_only(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=stroke_ICD10, separator=".m", first=FALSE)
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))
# Classify the stroke diagnoses by type of stroke
ICD10DiagFirst$type <- ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% haem_ICD10, "Haemorrhagic",
                              ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% infarct_ICD10, "Ischaemic", "Other"))

# Repeat for ICD9 diagnoses
ICD9Diag <- dbGetQuery(conn, paste0("SELECT ID, ", collist("HES_ICD9Diag", ".m", 46), ", ", collist("HES_ICD9DateFirst", ".m", 46), "FROM HES"))
ICD9Diag <- stroke_only(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=stroke_ICD9, separator=".m")
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))
ICD9DiagFirst$type <- ifelse(substr(ICD9DiagFirst$Code, 1, 3) == "430", "Haemorrhagic",
                             ifelse(substr(ICD9DiagFirst$Code, 1, 3) == "431", "Ischaemic", "Other"))
# ICD 9 codes
# Primary
ICD9Prim <- HES[,c(1,grep("ICD9Main", colnames(HES), fixed=TRUE))]
ICD9Prim <- code_filter(df=ICD9Prim, diagcolname="HES_ICD9Main", ncols=27, codelist=stroke_ICD9, separator=".m", first=TRUE)
# Secondary
ICD9Sec <- HES[,c(1,grep("ICD9Sec", colnames(HES), fixed=TRUE))]
ICD9Sec <- code_filter(df=ICD9Sec, diagcolname="HES_ICD9Secondary", ncols=29, codelist=stroke_ICD9, separator=".m", first=TRUE)

length(unique(ICD9Sec$ID))

rm(HES)

# Combine the ICD10 and ICD9 diagnoses into one stroke diagnosis dataset
stroke <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first stroke per person
# (There were only 3 individuals who had both ICD9 and ICD10 stroke diagnoses)
stroke <- do.call(rbind, by(stroke, stroke[,c("ID")], function(x) x[which.min(x$Date),]))

# # Verify that the summary of distinct primary/secondary diagnoses (ICD10Diag) correctly contains the individual ones (ICD10Main and ICD10Sec)
# # Take first date per diagnosis per individual from the primary diagnoses (secondary don't have dates)
# ICD10PrimFirst <- do.call(rbind, by(ICD10Prim, ICD10Prim[,c("ID", "HES_ICD10Main")], function(x) x[which.min(x$HES_ICD10MainDateFirst),]))
# # Merge with the secondary diagnoses
# ICD10All <- merge(x=ICD10PrimFirst, y=ICD10Sec, by.x=c("ID","HES_ICD10Main"), by.y=c("ID", "HES_ICD10Secondary"), all=TRUE)
# # Check that the same individuals are in both the manually merged and the UKB merged datasets
# nrow(anti_join(ICD10Diag, ICD10All, by="ID")) == 0
# length(unique(ICD10All$ID)) == length(unique(ICD10Diag$ID))
# 
# test <- merge(ICD10Diag, ICD10All, by.x=c("ID", "HES_ICD10Diag"), by.y=c("ID", "HES_ICD10Main"), all=TRUE)
# length(unique(test$ID))
# datemismatch <- test[test$HES_ICD10DateFirst!=test$HES_ICD10MainDateFirst & !is.na(test$HES_ICD10MainDateFirst),]
# 
# # Ok, going forward, use ICD10Diag (data fields 41270 and 41280)
# # Added the equivalent ICD9 fields to data download list, but there are so few of these that for now ok to just use prim and sec
# # Combine these to form ICD9All
# ICD9All <- merge(x=ICD9Prim, y=ICD9Sec, by.x=c("ID","HES_ICD9Main"), by.y=c("ID", "HES_ICD9Secondary"), all=TRUE)
# ICD9All <- unique(ICD9All)
# 
# all <- merge(ICD9All,ICD10All, by="ID")

#--------------------------------------------------------------------------------------------------------------
# RECRUITMENT DATE AND AGE
conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")
keydates <- dbGetQuery(conn, "SELECT ID, `BAC_BIRTHYEAR`, `BAC_BIRTHMONTH`, `BAC_DATELOSTFU`, `REC_DATEASSESS`, `ETH_ETHNICITY` 
                      FROM BaC JOIN REC_BASE USING (ID) JOIN ETH_BASE USING (ID)")
keydates$recdate <- as.Date(keydates$Rec_DateAssess, origin=as.Date("1970-01-01"))
keydates$lfudate <- as.Date(keydates$BaC_DateLostFU, origin=as.Date("1970-01-01"))
keydates$dob <- as.Date(paste0("15",keydates$BaC_BirthMonth, keydates$BaC_BirthYear), "%d%B%Y")
keydates$age <- round(difftime(keydates$recdate, keydates$dob, unit="days")/365.25,2)
dbWriteTable(conn, "keydates", keydates, overwrite=TRUE)

#--------------------------------------------------------------------------------------------------------------
# PUT IT ALL TOGETHER
# Assume we don't care what kind of stroke diagnosis, just want the date of first diagnosis with any form
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$HES_ICD10DateFirst),]))
# Combine these diagnosis dates with the recruitment, dates of birth and loss to follow up
dates <- merge(ICD10DiagFirst, keydates[,c("ID", "recdate", "lfudate", "dob", "age", "Eth_Ethnicity")], by="ID", all=TRUE)
# Remove any individuals who had a diagnosis of stroke before they were recruited
# This neatly avoids the question of any individuals with an implausibly early stroke diagnosis (eg dob)
dates <- dates[(dates$HES_ICD10DateFirst>dates$recdate & !is.na(dates$HES_ICD10DateFirst))|is.na(dates$HES_ICD10DateFirst),]
# Excludes 2775 individuals
# Note being specially careful about < and > because there are NA values

# Remove any individuals with non-White ethnicity
dates <- dates[dates$Eth_Ethnicity %in% c("White", "British", "Irish","Any other white background"),]
# Excludes 29630 individuals

# And merge in the bp variables
df <- merge(bpvars, dates, by="ID")

# Time from recruitment to event (stroke) in days
df$stime <- difftime(df$HES_ICD10DateFirst, df$recdate, units="days")

# Time from recruitment until lost to followup
df$futime <- difftime(df$lfudate, df$recdate, units="days")

# Administrative censoring:
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016 -> use earliest date, 29/02/2016
df$actime <- difftime(as.Date("29Feb2016", "%d%B%Y"), df$recdate, units="days")

# Combine these to give a "survival time" column
df$time <- pmin(df$stime, df$actime, df$futime, na.rm=TRUE)
# And generate a status column - stroke or censored
df$stroke <- 1*!is.na(df$HES_ICD10Diag)

#--------------------------------------------------------------------------------------------------------------
# NOW WE CAN DO SOME ANALYSIS
df$SBP2 = (df$SBP)^2
survobj <- Surv(df$time, df$stroke)
for (col in colnames(bpvars)[-1]) {
  print(col)
  res.cox <- coxph(survobj~df[[col]] + age, data=df)
  print(summary(res.cox))
}
res.cox <- coxph(Surv(time, stroke)~SBP + SBP2 + age, data=df)
summary(res.cox)
vif(res.cox)

fit <- survfit(survobj~df$D_STAT)
ggsurvplot(fit, data=df, risk.table = TRUE, ylim=c(0.95,1))

dbListTables(conn)
ethnicity <- dbGetQuery(conn, "SELECT * FROM ETH_BASE")

df <- merge(df, ethnicity, by="ID")
