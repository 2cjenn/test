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
cogfunc <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\cognitivefunction.rds")
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
# And with the cognitive function data
data <- merge(data, cogfunc, by="ID", all=TRUE)
# And with all other selected covariates
data <- merge(data, covars, by="ID", all=TRUE)
# Merge with the "probable BP meds" variable from the rubric
data <- merge(data, rubric[,c("ID", "HTN_probablemeds")], by="ID", all.x=TRUE)

saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_raw.rds")

