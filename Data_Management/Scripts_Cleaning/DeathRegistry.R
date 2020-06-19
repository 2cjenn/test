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
deaths <- readRDS(paste0(config$cleaning$rawdata, "Dth.rds"))

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

deaths$deathdate <- pmin(deaths$Dth_Date.m0.i0, deaths$Dth_Date.m0.i1, na.rm=TRUE)
deaths$demdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% dement_ICD10))
deaths$strdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% stroke_ICD10))

saveRDS(deaths[,c("ID", "deathdate", "demdeath", "strdeath", "Dth_Cause.m0.i0")], 
        file=paste0(config$cleaning$organised, "deathdate.rds"))