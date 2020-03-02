#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
deaths <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Dth.rds")

deaths$deathdate <- pmin(deaths$Dth_Date.m0.i0, deaths$Dth_Date.m0.i1, na.rm=TRUE)
deaths$demdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% dement_ICD10))
deaths$strdeath <- apply(deaths[,grep("Dth_ICD10", colnames(deaths), fixed=TRUE)], 1, function(x) any(substr(x, 1, 3) %in% stroke_ICD10))

saveRDS(deaths[,c("ID", "deathdate", "demdeath", "strdeath", "Dth_Cause.m0.i0")], 
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\deathdate.rds")