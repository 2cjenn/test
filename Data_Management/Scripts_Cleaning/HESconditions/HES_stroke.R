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
# https://www.ahajournals.org/doi/full/10.1161/01.STR.0000174293.17959.a1
HES <- readRDS(paste0(config$cleaning$rawdata, "HES.rds"))
source("K:/TEU/APOE on Dementia/Statistical Analysis/JCfunctions.R")

# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
haem_ICD9 <- c("430", "431")
infarct_ICD9 <- c("433")
other_ICD9 <- c("434","436")
stroke_ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9)

# Filter out dementia diagnoses and dates only
ICD10Diag <- HES[,c(1,grep("HES_ICD10Diag", colnames(HES), fixed=TRUE),grep("HES_ICD10DateFirst", colnames(HES), fixed=TRUE))]
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=stroke_ICD10, separator=".m", first=FALSE)
# Keep only the first dementia diagnosis (of any kind) per person
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))
# Classify the dementia diagnoses by type of dementia
ICD10DiagFirst$type <- ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% haem_ICD10, "Haemorrhagic",
                              ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% infarct_ICD10, "Ischaemic", "Other"))

# Repeat for ICD9 diagnoses
ICD9Diag <- HES[,c(1,grep("HES_ICD9Diag", colnames(HES), fixed=TRUE),grep("HES_ICD9DateFirst", colnames(HES), fixed=TRUE))]
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=stroke_ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))
ICD9DiagFirst$type <- ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% haem_ICD9, "Haemorrhagic",
                             ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% infarct_ICD9, "Ischaemic", "Other"))



# Combine the ICD10 and ICD9 diagnoses into one dementia diagnosis dataset
stroke <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first dementia per person
stroke <- do.call(rbind, by(stroke, stroke[,c("ID")], function(x) x[which.min(x$Date),]))

names(stroke)[names(stroke)=="Date"] <- "stroke_date"
names(stroke)[names(stroke)=="Code"] <- "stroke_code"
names(stroke)[names(stroke)=="type"] <- "stroke_type"

saveRDS(stroke, file=paste0(config$cleaning$organised, "stroke.rds"))