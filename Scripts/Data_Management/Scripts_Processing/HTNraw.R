#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)
library(here)

config <- yaml.load_file(here::here("config.yml"))

#--------------------------------------------------------------------------------------------------------------

bp <- readRDS(file.path(config$data$derived, "bp.rds"))
ethnicity <- readRDS(file.path(config$data$derived, "ethnicity.rds"))
basechar <- readRDS(file.path(config$data$derived, "basechar.rds"))
VImedhist <- readRDS(file.path(config$data$derived, "veint_HTNhist.rds"))
TQmedhist <- readRDS(file.path(config$data$derived, "tq_medhist.rds"))
famhist <- readRDS(file.path(config$data$derived, "familyhistory.rds"))
deathdate <- readRDS(file.path(config$data$derived, "deathdate.rds"))
covars <- readRDS(file.path(config$data$derived, "covars.rds"))
rubric <- readRDS(file.path(config$data$derived, "HTNMedsRubric.rds"))
CVDevents <- readRDS(file.path(config$data$derived, "CVD1_HESevents.rds"))
CVDprior <- readRDS(file.path(config$data$derived, "CVD1prior_HESevents.rds"))
hyperlipidaemia <- readRDS(file.path(config$data$derived, "highchol_prevalent.rds"))


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
# And with all other selected covariates
data <- merge(data, covars, by="ID", all=TRUE)
# Merge with the "probable BP meds" variable from the rubric
data <- merge(data, rubric[,c("ID", "hypmedsno", "HTN_probablemeds", "HTN_txalg")], by="ID", all.x=TRUE)
# And with all CVD outcomes
data <- merge(data, CVDevents, by="ID", all.x=TRUE)
data <- merge(data, CVDprior, by="ID", all.x=TRUE)
# And with hyperlipidaemia
data <- merge(data, hyperlipidaemia, by="ID", all.x=TRUE)


saveRDS(data, file=file.path(config$data$derived, "HTN_CVD.rds"))


