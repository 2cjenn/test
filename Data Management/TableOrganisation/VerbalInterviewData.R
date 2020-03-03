#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# The verbal interview data has a lot of columns for multiple diagnoses of cancer/other illness/operations/medicines
# Split these out into separate tables for easier use
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
veint <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Non-cancer illness codes and durations
VI_diag <- veint[,c(1, grep("VeI_NonCancerCode.", colnames(veint), fixed=TRUE), 
                    grep("VeI_NNonCancer.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_NonCancerCode.", colnames(veint), fixed=TRUE), 
                   grep("VeI_NNonCancer.", colnames(veint), fixed=TRUE))]
VI_diagdur <- veint[,c(1, grep("VeI_NonCancerYear.", colnames(veint), fixed=TRUE), 
                       grep("VeI_NonCancerAge.", colnames(veint), fixed=TRUE),
                       grep("VeI_NonCancerYrAgeFirst.", colnames(veint), fixed=TRUE),
                       grep("VeI_MTimeNonCancerFst.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_NonCancerYear.", colnames(veint), fixed=TRUE), 
                   grep("VeI_NonCancerAge.", colnames(veint), fixed=TRUE),
                   grep("VeI_NonCancerYrAgeFirst.", colnames(veint), fixed=TRUE),
                   grep("VeI_MTimeNonCancerFst.", colnames(veint), fixed=TRUE))]

saveRDS(VI_diag, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_diag_base.rds")
saveRDS(VI_diagdur, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_diagdur_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Cancer codes and durations
VI_cancer <- veint[,c(1, grep("VeI_CancerCode.", colnames(veint), fixed=TRUE), 
                      grep("VeI_Ncancer.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_CancerCode.", colnames(veint), fixed=TRUE), 
                   grep("VeI_Ncancer.", colnames(veint), fixed=TRUE))]
VI_cancerdur <- veint[,c(1, grep("VeI_CancerYr.", colnames(veint), fixed=TRUE), 
                         grep("VeI_CancerAge.", colnames(veint), fixed=TRUE), 
                         grep("VeI_CancerYrAgeFirst.", colnames(veint), fixed=TRUE),
                         grep("VeI_MTimeCancerFst.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_CancerYr.", colnames(veint), fixed=TRUE), 
                   grep("VeI_CancerAge.", colnames(veint), fixed=TRUE), 
                   grep("VeI_CancerYrAgeFirst.", colnames(veint), fixed=TRUE),
                   grep("VeI_MTimeCancerFst.", colnames(veint), fixed=TRUE))]

saveRDS(VI_cancer, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_cancer_base.rds")
saveRDS(VI_cancerdur, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_cancerdur_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Medication codes
VI_medcode <- veint[,c(1, grep("VeI_MedCode.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_MedCode.", colnames(veint), fixed=TRUE))]

saveRDS(VI_medcode, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_medcodes_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Operation codes and durations
VI_operation <- veint[,c(1, grep("VeI_OperationCode.", colnames(veint), fixed=TRUE), 
                         grep("VeI_NOperation.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_OperationCode.", colnames(veint), fixed=TRUE), 
                   grep("VeI_NOperation.", colnames(veint), fixed=TRUE))]
VI_opdur <- veint[,c(1, grep("VeI_OperationYear.", colnames(veint), fixed=TRUE),
                     grep("VeI_OperationAge.", colnames(veint), fixed=TRUE),
                     grep("VeI_OpYrAgeFirst.", colnames(veint), fixed=TRUE),
                     grep("VeI_MTimeOperation.", colnames(veint), fixed=TRUE))]
veint <- veint[,-c(grep("VeI_OperationYear.", colnames(veint), fixed=TRUE),
                   grep("VeI_OperationAge.", colnames(veint), fixed=TRUE),
                   grep("VeI_OpYrAgeFirst.", colnames(veint), fixed=TRUE),
                   grep("VeI_MTimeOperation.", colnames(veint), fixed=TRUE))]

saveRDS(VI_operation, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_operation_base.rds")
saveRDS(VI_opdur, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_opdur_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Save the remaining verbal interview data
saveRDS(veint, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeIcovars_base.rds")