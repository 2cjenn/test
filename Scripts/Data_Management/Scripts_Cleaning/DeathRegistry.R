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
deaths <- readRDS(paste0(config$data$received, "Dth.rds"))

# ICD 10 codes for dementia
alz_ICD10 <- c("F00")
vasc_ICD10 <- c("F01")
other_ICD10 <- c("F02", "F03")
dement_ICD10 <- c(alz_ICD10, vasc_ICD10, other_ICD10)

# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)

# ICD 10 codes for ALS (motor neurone disease)
als_ICD10 <- c("G122")

# ICD 10 codes for Parkinson disease
park_ICD10 <- c("G20")

# ICD 10 codes for myocardial infarction
MI_ICD10 <- c("I21")


deaths$deathdate <- pmin(deaths$Dth_Date.m0.i0, deaths$Dth_Date.m0.i1, na.rm=TRUE)
deaths$demdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% dement_ICD10))
deaths$strdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% stroke_ICD10))
deaths$alsdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 4) %in% als_ICD10))
deaths$parkdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% park_ICD10))

# For the HTN prospective analysis, we're interested in deaths with CVD as the underlying (primary) cause
CVD_ICD10 <- c(stroke_ICD10, MI_ICD10)
deaths$CVDdeath_primary <- apply(deaths[,grep("Dth_ICD10Underlying", colnames(deaths), fixed=TRUE)],
                                 1, function(x) any(substr(x, 1, 3) %in% CVD_ICD10))



saveRDS(deaths[,c("ID", "deathdate", "demdeath", "strdeath", "alsdeath", "parkdeath", 
                  "CVDdeath_primary", "Dth_Cause.m0.i0")], 
        file=paste0(config$data$derived, "deathdate.rds"))