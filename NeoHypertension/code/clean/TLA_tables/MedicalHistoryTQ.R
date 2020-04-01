#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
medhist <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\HMH_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Medications
#--------------------------------------------------------------------------------------------------------------


# Indicator variables for self-reported hypertension, stroke, and whether they take medicine for blood pressure
medhist$HBPmeds <- apply(medhist[,grep("HMH_MedCholBPDiab", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Blood pressure medication"))
medhist$diabmeds <- apply(medhist[,grep("HMH_MedCholBPDiab", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Insulin"))
medhist$cholmeds <- apply(medhist[,grep("HMH_MedCholBPDiab", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Cholesterol lowering medication"))
medhist$contraceptive <- apply(medhist[,grep("HMH_MedCholBPDiab", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Oral contraceptive pill or minipill"))
medhist$HRT <- apply(medhist[,grep("HMH_MedCholBPDiab", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Hormone replacement therapy"))
for(col in c("HBPmeds", "diabmeds", "cholmeds", "contraceptive", "HRT")){
  medhist[[col]][medhist$medication %in% c("Prefer not to answer", "Do not know") | is.na(medhist$medication)] <- NA
  # medhist[[col]][medhist$medication %in% c("Do not know") | is.na(medhist$medication)] <- "Do not know"
}

#--------------------------------------------------------------------------------------------------------------
# Vascular conditions
#--------------------------------------------------------------------------------------------------------------

vasccond <- medhist[,c("ID", "HMH_HeartProbs.0", "HMH_HeartProbs.1", "HMH_HeartProbs.2",  "HMH_HeartProbs.3")]
vasccond <- vasccond %>% pivot_longer(-ID, names_to="Instance", values_to="Condition")
vasccond$Instance <- 1
vasccond$Condition <- as.character(vasccond$Condition)
vasccond$Condition[is.na(vasccond$Condition)] <- "NA"
vasccond <- unique(vasccond)
vasccond <- vasccond %>% pivot_wider(names_from=Condition, values_from=Instance)




medhist$vcon <- coalesce(medhist$HMH_HeartProbs.0, medhist$HMH_HeartProbs.1, medhist$HMH_HeartProbs.2, medhist$HMH_HeartProbs.3)
# Create a new vascular condition variable: yes/no/prefer not to answer/NA
condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
medhist$vasc_cond <- ifelse(medhist$vcon %in% condlist, "Yes", NA)
medhist$vasc_cond[medhist$vcon=="None of the above"] <- "No"
medhist$vasc_cond[medhist$vcon=="Prefer not to answer"] <- "Prefer not to answer"
medhist$vasc_cond[is.na(medhist$vcon)] <- NA
medhist$vasc_cond <- factor(medhist$vasc_cond)

# Indicator variables for various vascular conditions
medhist$prevHBP <- apply(medhist[,grep("HMH_HeartProbs", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "High blood pressure"))
medhist$prevstroke <- apply(medhist[,grep("HMH_HeartProbs", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Stroke"))
medhist$prevCVD <- apply(medhist[,grep("HMH_HeartProbs", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% c("Angina", "Heart attack")))
for(col in c("prevHBP", "prevstroke", "prevCVD")){
  medhist[[col]][medhist$vasc_cond %in% c("Do not know", "Prefer not to answer") | is.na(medhist$vasc_cond)] <- NA
}


saveRDS(medhist[,c("ID", "medication", "HBPmeds", "diabmeds", "cholmeds", "contraceptive", "HRT", 
                   "vasc_cond", "prevHBP", "prevstroke", "prevCVD", 
                   "HMH_IllDisab", "HMH_Diabetes", "HMH_BowelSc", "HMH_HBPAge.0")],
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\tq_medhist.rds")