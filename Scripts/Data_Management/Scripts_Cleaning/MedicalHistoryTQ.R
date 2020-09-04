#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

if(!exists("config")){
  config = yaml.load_file("config.yml")
}
#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
medhist <- readRDS(paste0(config$data$received, "HMH_base.rds"))

#--------------------------------------------------------------------------------------------------------------
# Medications
#--------------------------------------------------------------------------------------------------------------
# Combine medications across male and females
medhist$medf <- coalesce(medhist$HMH_MedCholBPDiabHorm.0, medhist$HMH_MedCholBPDiabHorm.1, medhist$HMH_MedCholBPDiabHorm.2, medhist$HMH_MedCholBPDiabHorm.3)
medhist$medm <- coalesce(medhist$HMH_MedCholBPDiab.0, medhist$HMH_MedCholBPDiab.1, medhist$HMH_MedCholBPDiab.2)
medhist$medcombine <- coalesce(as.character(medhist$medf), as.character(medhist$medm))
medhist$medcombine <- factor(medhist$medcombine)
# Create a new medication variable: yes/no/do not know/prefer not to answer/NA
medlist <- c("Cholesterol lowering medication", "Blood pressure medication", "Oral contraceptive pill or minipill", "Hormone replacement therapy", "Insulin")
medhist$medication <- ifelse(medhist$medcombine %in% medlist, "Yes", NA)
medhist$medication[medhist$medcombine=="None of the above"] <- "No"
medhist$medication[medhist$medcombine=="Do not know"] <- "Do not know"
medhist$medication[medhist$medcombine=="Prefer not to answer"] <- "Prefer not to answer"
medhist$medication[is.na(medhist$medcombine)] <- NA
medhist$medication <- factor(medhist$medication)

# Indicator variables for self-reported hypertension, stroke, and whether they take medicine for blood pressure
medhist$HBPmeds <- apply(medhist[,grep("HMH_Med", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Blood pressure medication"))
medhist$diabmeds <- apply(medhist[,grep("HMH_Med", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Insulin"))
medhist$cholmeds <- apply(medhist[,grep("HMH_Med", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Cholesterol lowering medication"))
medhist$contraceptive <- apply(medhist[,grep("HMH_Med", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Oral contraceptive pill or minipill"))
medhist$HRT <- apply(medhist[,grep("HMH_Med", colnames(medhist), fixed=TRUE)], 1, function(x) any(x %in% "Hormone replacement therapy"))
for(col in c("HBPmeds", "diabmeds", "cholmeds", "contraceptive", "HRT")){
  medhist[[col]][medhist$medication %in% c("Prefer not to answer", "Do not know") | is.na(medhist$medication)] <- NA
  # medhist[[col]][medhist$medication %in% c("Do not know") | is.na(medhist$medication)] <- "Do not know"
}

#--------------------------------------------------------------------------------------------------------------
# Vascular conditions
#--------------------------------------------------------------------------------------------------------------
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

#--------------------------------------------------------------------------------------------------------------
# Rename the variables for illness/disabilities and diabetes
#--------------------------------------------------------------------------------------------------------------
names(medhist)[names(medhist)=="HMH_IllDisab"] <- "illdisab"
names(medhist)[names(medhist)=="HMH_HBPAge.0"] <- "HBPAge"
names(medhist)[names(medhist)=="HMH_BowelSc"] <- "BowelCancerScreening"
medhist$HBPAge[medhist$HBPAge %in% c(-1,-3)] <- NA

medhist$diabetes <- factor(medhist$HMH_Diabetes, levels=c("No", "Yes", "Prefer not to answer", "Do not know"))


saveRDS(medhist[,c("ID", "medication", "HBPmeds", "diabmeds", "cholmeds", "contraceptive", "HRT", 
                   "vasc_cond", "prevHBP", "prevstroke", "prevCVD", "illdisab", "diabetes", "BowelCancerScreening", "HBPAge")],
        file=paste0(config$data$derived, "tq_medhist.rds"))