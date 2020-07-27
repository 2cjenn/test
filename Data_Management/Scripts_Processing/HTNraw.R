#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)
library(here)

config = yaml.load_file(here("config.yml"))

#--------------------------------------------------------------------------------------------------------------

bp <- readRDS(paste0(config$cleaning$organised, "bp.rds"))
ethnicity <- readRDS(paste0(config$cleaning$organised, "ethnicity.rds"))
basechar <- readRDS(paste0(config$cleaning$organised, "basechar.rds"))
dementia <- readRDS(paste0(config$cleaning$organised, "dementia.rds"))
VImedhist <- readRDS(paste0(config$cleaning$organised, "veint_HTNhist.rds"))
TQmedhist <- readRDS(paste0(config$cleaning$organised, "tq_medhist.rds"))
famhist <- readRDS(paste0(config$cleaning$organised, "familyhistory.rds"))
deathdate <- readRDS(paste0(config$cleaning$organised, "deathdate.rds"))
cogfunc <- readRDS(paste0(config$cleaning$organised, "cognitivefunction.rds"))
covars <- readRDS(paste0(config$cleaning$organised, "covars.rds"))
rubric <- readRDS(paste0(config$exclusions$htn, "HTNMedsRubric.rds"))

prs <- readRDS(file=paste0(config$analysisdata$prs, "htn-evangelou2018_PRS.rds"))
pc <- readRDS(paste0(config$cleaning$organised, "principalcomponents.rds"))

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
data <- merge(data, rubric[,c("ID", "hypmedsno", "HTN_probablemeds")], by="ID", all.x=TRUE)


# Merge with the calculated PRS scores for SBP and DBP
data <- merge(data, prs, by="ID", all.x=TRUE)
data <- merge(data, pc, by="ID", all.x=TRUE)

saveRDS(data, file=paste0(config$exclusions$raw, "HTN_raw.rds"))

