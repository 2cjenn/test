#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

bp <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\bp.rds")
ethnicity <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\ethnicity.rds")
basechar <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\basechar.rds")
dementia <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\dementia.rds")
VImedhist <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\veint_HTNhist.rds")
TQmedhist <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\tq_medhist.rds")
famhist <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\familyhistory.rds")
deathdate <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\deathdate.rds")
covars <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\covars.rds")
rubric <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTNMedsRubric.rds")

# Combine the baseline characteristics with the ethnicities
data <- merge(basechar, ethnicity, by="ID", all=TRUE)
# And with the dates of death
data <- merge(data, deathdate, by="ID", all=TRUE)
# And with the blood pressure data
data <- merge(data, bp, by="ID", all=TRUE)
# And the medical history data from the touchscreen questionnaire
data <- merge(data, TQmedhist, by="ID", all=TRUE)
# And the medical history data from the verbal interview
data <- merge(data, VImedhist, by="ID", all=TRUE)
# And the family history data from the touchscreen questionnaire
data <- merge(data, famhist, by="ID", all=TRUE)
# And with the dementia diagnoses from HES data over the course of the study
data <- merge(data, dementia, by="ID", all=TRUE)
# And with all other selected covariates
data <- merge(data, covars, by="ID", all=TRUE)
# Merge with the "probable BP meds" variable from the rubric
data <- merge(data, rubric[,c("ID", "HTN_probablemeds")], by="ID", all.x=TRUE)

# Duration of hypertension diagnosis
data$HTNdx_TQ <- data$age - data$HBPAge
data$HTNdx_VI <- decimal_date(data$recdate) - data$VIhypdx_yr
data$HTNdx <- coalesce(data$HTNdx_VI, data$HTNdx_TQ)

# Single variable for self-reported hypertension
# If reported in either touchscreen questionnaire or verbal interview
data$selfrephyp <- (data$prevHBP==TRUE & !is.na(data$prevHBP)) | data$VIhyp==TRUE

# Single variable for self-reported meds
# If participant answered the question in touchscreen questionnaire, use this
# If they did not answer ("Do not know"/Prefer not to answer"/skipped question) then use the "probable BP meds"
data$HTN_probablemeds[is.na(data$HTN_probablemeds)] <- FALSE
data$selfrepmeds <- (data$HBPmeds==TRUE & !is.na(data$HBPmeds)) | data$HTN_probablemeds

# Indicator variable for "some evidence of hypertension" vs "no evidence of hypertension"
data$evidenceHTN <- (data$selfrephyp==TRUE | data$selfrepmeds==TRUE | data$measuredhyp==TRUE)
unique(data$evidenceHTN)

# Indicator variable for hypertension awareness - 
# did they say they had hypertension or say that they were taking BP meds
data$aware <- data$selfrephyp==TRUE | data$selfrepmeds==TRUE
data$aware[data$evidenceHTN==FALSE | is.na(data$evidenceHTN)] <- NA

# Indicator variable for hypertension treatment status
# Return to this after finalising incorporation of VI medication data
data$treated <- data$selfrepmeds
data$treated[data$aware==FALSE | is.na(data$aware)] <- NA

# Control
data$controlled[data$treated==FALSE | is.na(data$treated)] <- NA

# Center age on the minimum
data$c_age <- data$age - 50

# Make the key dichotomous variables into factors to improve readability of outputs
data$prevHBP_ <- factor(as.numeric(data$prevHBP), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (touchscreen)", "Self-reported prior HTN diagnosis in touchscreen questionnaire"))
data$VIhyp_ <- factor(as.numeric(data$VIhyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (VI)", "Self-reported prior HTN diagnosis in verbal interview"))
data$selfrephyp_ <- factor(as.numeric(data$selfrephyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis", "Self-reported prior HTN diagnosis"))

data$measuredhyp_ <- factor(as.numeric(data$measuredhyp), levels=c(0,1), labels=c("Measured BP < 140/90 at baseline", "Measured BP >= 140/90 at baseline"))

data$controlled_ <- factor(as.numeric(data$controlled), levels=c(0,1), labels=c("Inadequately controlled", "Successfully controlled"))

data$aware_ <- factor(as.numeric(data$aware), levels=c(0,1), labels=c("Unaware of hypertension", "Aware of hypertension"))

data$treated_ <- factor(as.numeric(data$treated), levels=c(0,1), labels=c("Did not report BP medication", "Self-reported BP medication"))

data$evidenceHTN_ <- factor(as.numeric(data$evidenceHTN), levels=c(0,1), labels=c("No evidence of hypertension", "Evidence of hypertension"))




saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_raw.rds")

