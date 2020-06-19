#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(lubridate)
library(yaml)

config = yaml.load_file("K:/TEU/APOE on Dementia/config.yml")

#--------------------------------------------------------------------------------------------------------------

# Read in the dataset
# n = 514,776
apoe <- readRDS(paste0(config$exclusions$apoe, "apoe_surv.rds"))

apoe$e4carrier <- apoe$apoe4>0
apoe$e4carrier_ <- factor(as.numeric(apoe$e4carrier), levels=c(0,1), labels=c("Not a carrier", "e4 carrier"))
apoe$apoe4 <- factor(apoe$apoe4)
apoe$number_of_apoe4_alleles <- apoe$apoe4

apoe$alcdaily <- apoe$Alc_Freq=="Daily or almost daily"
apoe$diab <- apoe$diabetes=="Yes"


## Remove individuals who have withdrawn from UKB
withdrawn <- read.csv("K:\\TEU\\APOE on Dementia\\Data Management\\WithdrawnIDs.csv", header=FALSE)
apoe <- apoe[!apoe$ID %in% withdrawn$V1,]

## Genetic exclusions

# Exclude individuals with APOE e1 because it's rare and don't know much about it
# Exclude individuals with genotype e2/e4 because they cancel out?
# Also can't be definitively sure if genotype is e1/e3 or e2/e4
# n = 11,493
apoe <- apoe[apoe$apoe1==0 & !is.na(apoe$apoe1),]

# Exclude individuals with genetic/reported sex mismatch
# n=337
apoe$Sex <- factor(apoe$Sex, ordered=FALSE)
apoe <- apoe[apoe$Sex == apoe$gender,]

## Other important exclusions

# Restrict to self-report "white"
apoe$cauc <- apoe$eth_group=="White"
apoe <- apoe[apoe$cauc==TRUE,]


# Exclude individuals with prevalent dementia
# n = 155
apoe <- apoe[!apoe$VIdementia,]
apoe <- apoe[is.na(apoe$dement_date) | (apoe$dement_date>apoe$recdate & !is.na(apoe$dement_date)),]


# Exclude individuals with no age data
# n = 0
apoe <- apoe[!is.na(apoe$age),]
# Exclude those outside the 40-70 age range
# n = 2247
apoe <- apoe[apoe$age >= 40 & apoe$age < 70,]

#--------------------------------------------------------------------------------------------------------------
# Create some more complex variables
#--------------------------------------------------------------------------------------------------------------
data <- apoe
# Duration of hypertension diagnosis
data$HTNdx_TQ <- round(data$age - data$HBPAge,1)
data$HTNdx_VI <- round(decimal_date(data$recdate) - data$VIhypdx_yr,1)
data$HTNdx_duration <- coalesce(data$HTNdx_VI, data$HTNdx_TQ)
# Any durations less than 0 are clearly errors (only 2)
# Any diagnoses before the age of about 20 are probably errors
data$HTNdx_duration[(data$HTNdx_duration < 0 | data$HTNdx_duration > data$HBPAge-20)& !is.na(data$HTNdx_duration)] <- NA

# Single variable for self-reported hypertension
# If reported in either touchscreen questionnaire or verbal interview
data$selfrephyp <- (data$prevHBP==TRUE & !is.na(data$prevHBP)) | data$VIhyp==TRUE
data$selfrephyp[is.na(data$prevHBP) & is.na(data$NumberDiagnoses)] <- NA

# Single variable for self-reported meds
# If participant answered the question in touchscreen questionnaire, use this
# If they did not answer ("Do not know"/Prefer not to answer"/skipped question) then use the "probable BP meds"
data$HTN_probablemeds[is.na(data$HTN_probablemeds)] <- FALSE
data$selfrepmeds <- (data$HBPmeds==TRUE & !is.na(data$HBPmeds)) | data$HTN_probablemeds
data$selfrepmeds[is.na(data$HBPmeds) & is.na(data$NumberMedications)] <-NA


#--------------------------------------------------------------------------------------------------------------
# Hypertension exclusions
#--------------------------------------------------------------------------------------------------------------

# Exclude individuals who have withdrawn from the study
withdrawn <- read.csv("K:\\TEU\\APOE on Dementia\\Data Management\\WithdrawnIDs.csv", header=FALSE)
data <- data[!data$ID %in% withdrawn$V1,]

excl <- list(initial=nrow(data))

# Exclude those outside the 40-70 age range
# n = 502506 - 500011 = 2495
data <- data[data$age >= 40 & data$age < 70,]

excl$agerange=nrow(data)

# Exclude individuals with missing BP data
# or missing answers to BP questions on touchscreen questionnaire
# n = 500011 - 498698 = 1313
data <- data[!is.na(data$SBP),]
data <- data[!is.na(data$DBP),]
data <- data[!is.na(data$selfrephyp),]
data <- data[!is.na(data$selfrepmeds),]

excl$BP=nrow(data)

#--------------------------------------------------------------------------------------------------------------
# Generate hypertension category variables
#--------------------------------------------------------------------------------------------------------------

# Indicator variable for "some evidence of hypertension" vs "no evidence of hypertension"
data$evidenceHTN <- (data$selfrephyp==TRUE | data$selfrepmeds==TRUE | data$measuredhyp==TRUE)
unique(data$evidenceHTN)

# Indicator variable for hypertension awareness - 
# did they say they had hypertension or say that they were taking BP meds
data$aware <- data$selfrephyp==TRUE | data$selfrepmeds==TRUE
data$aware[data$evidenceHTN==FALSE | is.na(data$evidenceHTN)] <- NA

# Indicator variable for hypertension treatment status
# Return to this after finalising incorporation of VI medication data
data$treated <- data$selfrepmeds
data$treated[data$aware==FALSE | is.na(data$aware)] <- NA
data$hypmedsno[data$treated==FALSE | is.na(data$treated)] <- 0

# Control
data$controlled[data$treated==FALSE | is.na(data$treated)] <- NA

# Center age on the minimum
data$c_age <- data$age - 40

# Make the key dichotomous variables into factors to improve readability of outputs
data$prevHBP_ <- factor(as.numeric(data$prevHBP), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (touchscreen)", "Self-reported prior HTN diagnosis in touchscreen questionnaire"))
data$VIhyp_ <- factor(as.numeric(data$VIhyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (VI)", "Self-reported prior HTN diagnosis in verbal interview"))
data$selfrephyp_ <- factor(as.numeric(data$selfrephyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis", "Self-reported prior HTN diagnosis"))
data$measuredhyp_ <- factor(as.numeric(data$measuredhyp), levels=c(0,1), labels=c("Measured BP < 140/90 at baseline", "Measured BP >= 140/90 at baseline"))
data$controlled_ <- factor(as.numeric(data$controlled), levels=c(0,1), labels=c("Inadequately controlled", "Successfully controlled"))
data$aware_ <- factor(as.numeric(data$aware), levels=c(0,1), labels=c("Unaware of hypertension", "Aware of hypertension"))
data$treated_ <- factor(as.numeric(data$treated), levels=c(0,1), labels=c("Did not report BP medication", "Self-reported BP medication"))
data$evidenceHTN_ <- factor(as.numeric(data$evidenceHTN), levels=c(0,1), labels=c("No evidence of hypertension", "Evidence of hypertension"))


# Remaining n = 474,343
saveRDS(data, file=paste0(config$exclusions$apoe, "apoe_excl.rds"))