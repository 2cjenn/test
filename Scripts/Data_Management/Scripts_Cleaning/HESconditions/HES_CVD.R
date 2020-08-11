#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
# https://www.ahajournals.org/doi/full/10.1161/01.STR.0000174293.17959.a1
HES <- readRDS(paste0(config$data$received, "HES.rds"))
source(config$functions)

### ICD 10 codes for CVD ###
# Stroke
haem_ICD10 <- c("I60", "I61")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
# Myocardial infarction
MI_ICD10 <- c("I21")
# Combine them all
ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10, MI_ICD10)

### ICD 9 codes for CVD ###
# Stroke
haem_ICD9 <- c("430", "431")
infarct_ICD9 <- c("433")
other_ICD9 <- c("434","436")
# Myocardial infarction
MI_ICD9 <- c()
# Combine them all
ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9, MI_ICD9)


# Filter out diagnoses and dates only
ICD10Diag <- HES[,c(1,grep("HES_ICD10Diag", colnames(HES), fixed=TRUE),grep("HES_ICD10DateFirst", colnames(HES), fixed=TRUE))]
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=ICD10, separator=".m", first=FALSE)
# Keep only the first diagnosis (of any kind) per person
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))

# Repeat for ICD9 diagnoses
ICD9Diag <- HES[,c(1,grep("HES_ICD9Diag", colnames(HES), fixed=TRUE),grep("HES_ICD9DateFirst", colnames(HES), fixed=TRUE))]
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))


# Combine the ICD10 and ICD9 diagnoses into one diagnosis dataset
data <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first diagnosis per person
data <- do.call(rbind, by(data, data[,c("ID")], function(x) x[which.min(x$Date),]))

names(data)[names(data)=="Date"] <- "CVD_date"
names(data)[names(data)=="Code"] <- "CVD_code"

saveRDS(data, file=paste0(config$data$derived, "CVD_HESevents.rds"))