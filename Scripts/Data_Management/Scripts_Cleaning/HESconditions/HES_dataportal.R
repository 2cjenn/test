#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 08/09/2020
# Load and clean the HES diagnosis data from the Data Portal
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(lubridate)
library(yaml)
library(stringr)
library(glue)
library(data.table)

config = yaml.load_file("config.yml")

source(config$functions)

filedate <- "20200908"

datafile <- file.path(config$data$portal, "HES_diagnoses", glue("ukb_{filedate}.txt"))

data <- fread(datafile)

data <- data %>% mutate(
  Date = coalesce(epistart, admidate),
  Date = as.Date(Date, format="%Y-%m-%d"),
  diag_icd9 = na_if(diag_icd9, ""),
  diag_icd10 = na_if(diag_icd10, ""),
  dsource = factor(dsource)
) %>%
  select(-epistart, -admidate)

ICD9 <- data %>% 
  filter(!is.na(diag_icd9)) %>% 
  select(-diag_icd10) %>% 
  rename(Code = diag_icd9,
         ID = eid) %>%
  mutate(ICD = "ICD9")

ICD10 <- data %>% 
  filter(!is.na(diag_icd10)) %>% 
  select(-diag_icd9) %>% 
  rename(Code = diag_icd10,
         ID = eid) %>%
  mutate(ICD = "ICD10")

saveRDS(ICD9, file.path(config$data$derived, "HES_recordlevelICD9.rds"))
saveRDS(ICD10, file.path(config$data$derived, "HES_recordlevelICD10.rds"))