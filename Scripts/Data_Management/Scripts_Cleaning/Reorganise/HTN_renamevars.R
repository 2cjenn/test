
# Rename variables
# Baseline characteristics
names(data)[names(data)=="BaC_RsnLostFU"] <- "lfureason"
names(data)[names(data)=="BaC_DeprivInd"] <- "townsend_depind"
names(data)[names(data)=="DateLostFU"] <- "lfudate"
names(data)[names(data)=="DV_DateOfBirth"] <- "dob"
names(data)[names(data)=="DV_AgeAtRec"] <- "age"
names(data)[names(data)=="BaC_Sex"] <- "gender"
names(data)[names(data)=="DV_DateAssess"] <- "recdate"

# Blood pressure
names(data)[names(data)=="DV_SBP.avg"] <- "SBP"
names(data)[names(data)=="DV_DBP.avg"] <- "DBP"
names(data)[names(data)=="DV_PulseRate.avg"] <- "PulseRate"
names(data)[names(data)=="DV_SBP.cat"] <- "SBPcat"
names(data)[names(data)=="DV_DBP.cat"] <- "DBPcat"
names(data)[names(data)=="DV_MeasuredHTN"] <- "measuredhyp"

# Create controlled var
data$controlled <- !data$measuredhyp

# Covariates
names(data)[names(data)=="CoF_RTTTimeID"] <- "mean_reacttime"

names(data)[names(data)=="VeI_Ncancer.0"] <- "NumberCancers"
names(data)[names(data)=="VeI_NNonCancer.0"] <- "NumberDiagnoses"
names(data)[names(data)=="VeI_NOperation.0"] <- "NumberOperations"
names(data)[names(data)=="VeI_Ntreatments"] <- "NumberMedications"

names(body)[names(body)=="BSM_BMI"] <- "BMI"
names(body)[names(body)=="BSM_Waist"] <- "WaistCirc"

#--------------------------------------------------------------------------------------------------------------
# TQ medical history: illness/disabilities and diabetes
#--------------------------------------------------------------------------------------------------------------
names(data)[names(data)=="HMH_IllDisab"] <- "illdisab"
names(data)[names(data)=="HMH_Diabetes"] <- "diabetes"
names(data)[names(data)=="HMH_HBPAge.0"] <- "HBPAge"
names(data)[names(data)=="HMH_BowelSc"] <- "BowelCancerScreening"

# Age at diagnosis of HTN is coded -1 or -3 for don't know or prefer not to answer
medhist$HBPAge[medhist$HBPAge %in% c(-1,-3)] <- NA