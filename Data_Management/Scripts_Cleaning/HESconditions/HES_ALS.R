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
HES <- readRDS(paste0(config$data$received, "HES.rds"))
BaC <- readRDS(paste0(config$data$derived, "basechar.rds"))
source(config$functions)

# ICD 10 codes for ALS (motor neurone disease)
als_ICD10 <- c("G122")
# ICD 9 codes for ALS
als_ICD9 <- c("3352")

# Filter out ALS diagnoses and dates only
ICD10Diag <- HES[,c(1,grep("HES_ICD10Diag", colnames(HES), fixed=TRUE),grep("HES_ICD10DateFirst", colnames(HES), fixed=TRUE))]
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=als_ICD10, separator=".m", first=FALSE)
# Keep only the first ALS diagnosis per person
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))

# Repeat for ICD9 diagnoses
ICD9Diag <- HES[,c(1,grep("HES_ICD9Diag", colnames(HES), fixed=TRUE),grep("HES_ICD9DateFirst", colnames(HES), fixed=TRUE))]
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=als_ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))


# Combine the ICD10 and ICD9 diagnoses into one ALS diagnosis dataset
als <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first Parkinson disease per person
als <- do.call(rbind, by(als, als[,c("ID")], function(x) x[which.min(x$Date),]))

als$Code <- factor(als$Code)

als <- merge(als, BaC[,c("ID", "recdate", "dob")], by="ID", order=FALSE)
als <- als[als$Date > als$recdate,]
als <- als[als$Date < Sys.Date(),]

saveRDS(als, file=paste0(config$data$derived, "als_incident.rds"))