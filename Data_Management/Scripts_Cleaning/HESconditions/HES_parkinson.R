#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

config = yaml.load_file("K:/TEU/APOE on Dementia/config.yml")

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
HES <- readRDS(paste0(config$cleaning$rawdata, "HES.rds"))
BaC <- readRDS(paste0(config$cleaning$organised, "basechar.rds"))
source(config$functions)

# ICD 10 codes for Parkinson disease
park_ICD10 <- c("G20")
# ICD 9 codes for Parkinson disease
park_ICD9 <- c("3320")

# Filter out Parkinson disease diagnoses and dates only
ICD10Diag <- HES[,c(1,grep("HES_ICD10Diag", colnames(HES), fixed=TRUE),grep("HES_ICD10DateFirst", colnames(HES), fixed=TRUE))]
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=park_ICD10, separator=".m", first=FALSE)
# Keep only the first Parkinson disease diagnosis (of any kind) per person
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))

# Repeat for ICD9 diagnoses
ICD9Diag <- HES[,c(1,grep("HES_ICD9Diag", colnames(HES), fixed=TRUE),grep("HES_ICD9DateFirst", colnames(HES), fixed=TRUE))]
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=park_ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))



# Combine the ICD10 and ICD9 diagnoses into one Parkinson disease diagnosis dataset
parkinson <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first Parkinson disease per person
parkinson <- do.call(rbind, by(parkinson, parkinson[,c("ID")], function(x) x[which.min(x$Date),]))


parkinson$Code <- factor(parkinson$Code)

parkinson <- merge(parkinson, BaC[,c("ID", "recdate", "dob")], by="ID", order=FALSE)
parkinson <- parkinson[parkinson$Date > parkinson$recdate,]
parkinson <- parkinson[parkinson$Date < Sys.Date(),]


saveRDS(parkinson, file=paste0(config$cleaning$organised, "parkinson_incident.rds"))