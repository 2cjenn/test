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
HES <- readRDS(file.path(config$data$received, "HES.rds"))
source(config$functions)

# ICD 10 codes for dementia
alz_ICD10 <- c("F00")
vasc_ICD10 <- c("F01")
other_ICD10 <- c("F02", "F03")
dement_ICD10 <- c(alz_ICD10, vasc_ICD10, other_ICD10)
# ICD 9 codes for dementia
alz_ICD9 <- c("3310")
vasc_ICD9 <- c("2904")
other_ICD9 <- c("2900", "2901", "2902", "2903", "2941", "2942", "3311")
dement_ICD9 <- c(alz_ICD9, vasc_ICD9, other_ICD9)

# Filter out dementia diagnoses and dates only
ICD10Diag <- HES[,c(1,grep("HES_ICD10Diag", colnames(HES), fixed=TRUE),grep("HES_ICD10DateFirst", colnames(HES), fixed=TRUE))]
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=dement_ICD10, separator=".m", first=FALSE)
# Keep only the first dementia diagnosis (of any kind) per person
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))
# Classify the dementia diagnoses by type of dementia
ICD10DiagFirst$type <- ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% alz_ICD10, "Alzheimers",
                              ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% vasc_ICD10, "Vascular", "Other"))

# Repeat for ICD9 diagnoses
ICD9Diag <- HES[,c(1,grep("HES_ICD9Diag", colnames(HES), fixed=TRUE),grep("HES_ICD9DateFirst", colnames(HES), fixed=TRUE))]
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=dement_ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))
ICD9DiagFirst$type <- ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% alz_ICD9, "Alzheimers",
                             ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% vasc_ICD9, "Vascular", "Other"))



# Combine the ICD10 and ICD9 diagnoses into one dementia diagnosis dataset
dementia <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first dementia per person
dementia <- do.call(rbind, by(dementia, dementia[,c("ID")], function(x) x[which.min(x$Date),]))

names(dementia)[names(dementia)=="Date"] <- "dement_date"
names(dementia)[names(dementia)=="Code"] <- "dement_code"
names(dementia)[names(dementia)=="type"] <- "dement_type"

saveRDS(dementia, file=file.path(config$data$derived, "dementia.rds"))