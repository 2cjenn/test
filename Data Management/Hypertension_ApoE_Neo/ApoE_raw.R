#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Combine the apoe data with all the other UKB data
apoe <- readRDS("K:\\TEU\\UKB Genetic Data\\SNP Extraction\\APOE_20191018\\extracted_files\\R\\gen_apoe.rds")
data <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_raw.rds")

names(apoe)[names(apoe)=="IID"] <- "ID"
length(unique(apoe$ID))

apoe <- merge(apoe, data, by="ID")

saveRDS(apoe, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\APOE\\apoe_raw.rds")