#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
cogfunc <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\CoF_base.rds")

names(cogfunc)[names(cogfunc)=="CoF_RTTTimeID"] <- "mean_reacttime"

saveRDS(cogfunc[,c("ID", "mean_reacttime")],
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\cognitivefunction.rds")
