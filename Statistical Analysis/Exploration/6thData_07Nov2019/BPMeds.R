#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 25/11/2019
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

source("K:/TEU/APOE on Dementia/Statistical Analysis/JCfunctions.R")

conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")

# Investigate the list of tables
dbListTables(conn)
# and the columns of data in a table
dbGetQuery(conn, "PRAGMA table_info(HMH_base)")


bpvars <- dbGetQuery(conn, "SELECT * FROM BPVARS")
# Exclude individuals with missing BP data
bpvars <- bpvars[rowSums(is.na(bpvars[,-1])) ==0,]
# n=30,116
# Make the categorical variables into factors
bpvars$S_STAT <- factor(bpvars$S_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=FALSE)
bpvars$D_STAT <- factor(bpvars$D_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=FALSE)
# And ordered factors
bpvars$S_STATo <- factor(bpvars$S_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=TRUE)
bpvars$D_STATo <- factor(bpvars$D_STAT, levels=c("NORMAL", "HIGH", "VERY HIGH"), ordered=TRUE)


# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61", "I62")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4654947/
haem_ICD9 <- c("430", "431", "432")
infarct_ICD9 <- c("433", "434", "436")
other_ICD9 <- c()
stroke_ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9)


ICD10Diag <- dbGetQuery(conn, paste0("SELECT ID, ", diagcollist("HES_ICD10Diag", ".m", 212), ", ", diagcollist("HES_ICD10DateFirst", ".m", 212), "FROM HES"))
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=stroke_ICD10, separator=".m", first=FALSE)
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))
# Classify the stroke diagnoses by type of stroke
ICD10DiagFirst$type <- ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% haem_ICD10, "Haemorrhagic",
                              ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% infarct_ICD10, "Ischaemic", "Other"))

# Repeat for ICD9 diagnoses
ICD9Diag <- dbGetQuery(conn, paste0("SELECT ID, ", diagcollist("HES_ICD9Diag", ".m", 46), ", ", diagcollist("HES_ICD9DateFirst", ".m", 46), "FROM HES"))
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=stroke_ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))
ICD9DiagFirst$type <- ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% haem_ICD9, "Haemorrhagic",
                             ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% infarct_ICD9, "Ischaemic", "Other"))



# Combine the ICD10 and ICD9 diagnoses into one stroke diagnosis dataset
stroke <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first stroke per person
# (There were only 3 individuals who had both ICD9 and ICD10 stroke diagnoses)
stroke <- do.call(rbind, by(stroke, stroke[,c("ID")], function(x) x[which.min(x$Date),]))



# To exclude prevalent strokes, grab the recruitment dates
keydates <- dbGetQuery(conn, "SELECT ID, `BAC_BIRTHYEAR`, `BAC_BIRTHMONTH`, `BAC_DATELOSTFU`, `REC_DATEASSESS`, `ETH_ETHNICITY` 
                      FROM BaC JOIN REC_BASE USING (ID) JOIN ETH_BASE USING (ID)")
keydates$recdate <- as.Date(keydates$Rec_DateAssess, origin=as.Date("1970-01-01"))
keydates$lfudate <- as.Date(keydates$BaC_DateLostFU, origin=as.Date("1970-01-01"))
keydates$dob <- as.Date(paste0("15",keydates$BaC_BirthMonth, keydates$BaC_BirthYear), "%d%B%Y")
keydates$age <- round(difftime(keydates$recdate, keydates$dob, unit="days")/365.25,2)

# Join the stroke diagnoses to the full list of all participants and their recruitment dates etc
df <- merge(stroke, keydates, by="ID", all=TRUE)
# Filter out the prevalent cases to be eyeballed
prevalent <- df[df$Date<df$recdate&!is.na(df$Date),]
# Exclude the prevalent cases from the working dataset
df <- df[(df$Date>df$recdate & !is.na(df$Date))|is.na(df$Date),]



medhist <- dbGetQuery(conn, "SELECT * FROM HMH_base")
medhist$HBP <- apply(medhist[,grep("HMH_HeartProbs", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "High blood pressure"))
medhist$HBPmeds <- apply(medhist[,grep("HMH_Med", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Blood pressure medication"))
medhist$prevstroke <- apply(medhist[,grep("HMH_HeartProbs", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Stroke"))
# Exclude individuals with previous self-reported stroke
medhist <- medhist[medhist$prevstroke==FALSE,]

df <- merge(df, medhist[,c("ID", "HBP", "HMH_HBPAhe.0", "HBPmeds")], by="ID")




veint <- dbGetQuery(conn, "SELECT * FROM VEI_BASE")
coding6 <- read.table("K:/TEU/CancerPRS/Data_Dictionary/Mappings/coding6.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)
strcodes <- coding6$coding[grep("stroke|ischaemic stroke|subdural haemorrhage|subarachnoid haemorrhage|brain haemorrhage", coding6$meaning)]
veint$prevstroke <- apply(veint[,grep("VeI_NonCancerCode", colnames(veint), fixed=TRUE)], 1, function(x) any(x %in% strcodes))
veint <- veint[veint$prevstroke==FALSE,]
bpcodes <- coding6$coding[grep("hypertension|essential hypertension", coding6$meaning)]
veint$hxhyp <- apply(veint[,grep("VeI_NonCancerCode", colnames(veint), fixed=TRUE)], 1, function(x) any(x %in% bpcodes))
df <- merge(df, veint[,c("ID", "prevstroke", "hxhyp")], by="ID")

# And merge in the bp variables
df <- merge(df, bpvars, by="ID")

# Adjusted SBP and DBP if individual is taking hypertensive medication
df$SBPtrt <- ifelse(df$HBPmeds==TRUE, df$SBP+15, df$SBP)
df$DBPtrt <- ifelse(df$HBPmeds==TRUE, df$DBP+10, df$DBP)

table(df$HBP, df$HBPmeds)

# Time from recruitment to event (stroke) in days
df$stime <- difftime(df$Date, df$recdate, units="days")

# Time from recruitment until lost to followup
df$futime <- difftime(df$lfudate, df$recdate, units="days")

# Administrative censoring:
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016 -> use earliest date, 29/02/2016
df$actime <- difftime(as.Date("29Feb2016", "%d%B%Y"), df$recdate, units="days")

# Combine these to give a "survival time" column
df$time <- pmin(df$stime, df$actime, df$futime, na.rm=TRUE)
df$htime <- ifelse(df$type=="Haemorrhagic" & !is.na(df$type), pmin(df$stime, df$actime, df$futime, na.rm=TRUE), pmin(df$actime, df$futime, na.rm=TRUE))
df$itime <- ifelse(df$type=="Ischaemic" & !is.na(df$type), pmin(df$stime, df$actime, df$futime, na.rm=TRUE), pmin(df$actime, df$futime, na.rm=TRUE))
  
# And generate a status column - stroke or censored
df$stroke <- 1*!is.na(df$Code)
df$haem <- 1*(df$type=="Haemorrhagic" & !is.na(df$type))
df$isch <- 1*(df$type=="Ischaemic" & !is.na(df$type))

# Save this to the database
dbWriteTable(conn, "SURV_BP_STROKE", df, overwrite=TRUE)

survobj <- Surv(df$time, df$stroke)

coxph(Surv(time, stroke)~SBP + HBP + HBPmeds + age, data=df)


haemobj <- Surv(df$htime, df$haem)
ischobj <- Surv(df$itime, df$isch)

coxph(haemobj ~ D_STAT + age, data=df)
coxph(ischobj ~ D_STAT + age, data=df)




coxph(haemobj ~ S_STAT + age, data=df)

coxph(ischobj ~ D_STAT + age, data=df)

dbDisconnect(conn)

table(df$S_STAT[df$HBP==TRUE], df$HBPmeds[df$HBP==TRUE])
