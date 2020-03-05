#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------
data <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_raw.rds")

#--------------------------------------------------------------------------------------------------------------
# Create some more complex variables
#--------------------------------------------------------------------------------------------------------------

# Duration of hypertension diagnosis
data$HTNdx_TQ <- data$age - data$HBPAge
data$HTNdx_VI <- decimal_date(data$recdate) - data$VIhypdx_yr
data$HTNdx <- coalesce(data$HTNdx_VI, data$HTNdx_TQ)

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

# Control
data$controlled[data$treated==FALSE | is.na(data$treated)] <- NA

# Center age on the minimum
data$c_age <- data$age - 50

# Make the key dichotomous variables into factors to improve readability of outputs
data$prevHBP_ <- factor(as.numeric(data$prevHBP), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (touchscreen)", "Self-reported prior HTN diagnosis in touchscreen questionnaire"))
data$VIhyp_ <- factor(as.numeric(data$VIhyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (VI)", "Self-reported prior HTN diagnosis in verbal interview"))
data$selfrephyp_ <- factor(as.numeric(data$selfrephyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis", "Self-reported prior HTN diagnosis"))
data$measuredhyp_ <- factor(as.numeric(data$measuredhyp), levels=c(0,1), labels=c("Measured BP < 140/90 at baseline", "Measured BP >= 140/90 at baseline"))
data$controlled_ <- factor(as.numeric(data$controlled), levels=c(0,1), labels=c("Inadequately controlled", "Successfully controlled"))
data$aware_ <- factor(as.numeric(data$aware), levels=c(0,1), labels=c("Unaware of hypertension", "Aware of hypertension"))
data$treated_ <- factor(as.numeric(data$treated), levels=c(0,1), labels=c("Did not report BP medication", "Self-reported BP medication"))
data$evidenceHTN_ <- factor(as.numeric(data$evidenceHTN), levels=c(0,1), labels=c("No evidence of hypertension", "Evidence of hypertension"))


saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_excl.rds")


data <- readRDS(file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_excl.rds")
#--------------------------------------------------------------------------------------------------------------
# Further exclusions for Neo's covariates of interest
#--------------------------------------------------------------------------------------------------------------

# Exclude individuals without highest level of education
# n = 498698 - 489006 = 
data <- data[!is.na(data$edu_highest),]

excl$education <- nrow(data)

# Exclude individuals who have serious health conditions
# n = 489006 - 483794 = 5212
seriouscomorbid <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypExclude.rds")
data <- data[!data$ID %in% seriouscomorbid$ID[!is.na(seriouscomorbid$Yes)],]

excl$seriouscomorb <- nrow(data)

# And individuals with cancer (except for skin cancer?)
# n = 483794 - 443782 = 40,012
data <- data[data$NumberCancers==0 & !is.na(data$NumberCancers),]

excl$cancer <- nrow(data)

# Exclude individuals with severely limited function (hand grip, reaction time)
# n = 443782 - 443627 = 155
data <- data[data$mean_reacttime<1500 | is.na(data$mean_reacttime),]

excl$limitfunc <- nrow(data)

excl$hypert <- nrow(data[data$evidenceHTN==TRUE,])

# Create exclusion flowchart
export_svg(DiagrammeR::grViz("K:\\TEU\\APOE on Dementia\\Statistical Analysis\\NeoHypertension\\RMarkdown\\ExclusionFlowchart.gv")
           ) %>% charToRaw %>% rsvg %>% png::writePNG("K:\\TEU\\APOE on Dementia\\Statistical Analysis\\NeoHypertension\\RMarkdown\\ExclFlowchart.png")

#--------------------------------------------------------------------------------------------------------------
# Neo's specific variants of covariate variables
#--------------------------------------------------------------------------------------------------------------

# Collapse ethnic groups into broader categories
data$ethnicity <- as.character(data$eth_group)
data$ethnicity[data$ethnicity=="Chinese"] <- "Asian"
data$ethnicity[data$ethnicity=="Asian or Asian British"] <- "Asian"
data$ethnicity[data$ethnicity=="Black or Black British"] <- "Black"
data$ethnicity[data$ethnicity=="Other ethnic group"] <- "Other"
data$ethnicity[data$ethnicity=="Prefer not to answer"] <- "Other"
data$ethnicity[data$ethnicity=="Do not know"] <- "Other"
data$ethnicity[data$ethnicity=="Mixed"] <- "Other"
data$ethnicity <- factor(data$ethnicity, levels=c("White", "Black", "Asian", "Other"))

# # Categorise age of leaving education into primary, secondary or tertiary education
# data$education[data$Edu_Age.0==-2 & !is.na(data$Edu_Age.0)] <- "None"
# data$education[data$Edu_Age.0 %in% c(5:12) & !is.na(data$Edu_Age.0)] <- "Primary"
# data$education[data$Edu_Age.0 %in% c(13:18) & !is.na(data$Edu_Age.0)] <- "Secondary"
# data$education[data$Edu_Age.0>18 & !is.na(data$Edu_Age.0)] <- "Tertiary or higher"
# data$education[data$Edu_Age.0==-1 | data$Edu_Age.0==-3 | is.na(data$Edu_Age.0)] <- "Unknown/unanswered"
# data$education <- factor(data$education, levels=c("None", "Primary", "Secondary", "Tertiary or higher", "Unknown/unanswered"))

# Convert UKB qualification categories into ISCED education categories
data$ISCED <- ifelse(data$edu_highest=="College or University degree", "ISCED 5: First stage of tertiary education",
                     ifelse(data$edu_highest=="NVQ or HND or HNC or equivalent", "ISCED 5: First stage of tertiary education",
                            ifelse(data$edu_highest=="Other professional qualifications eg: nursing, teaching", "ISCED 4: Post-secondary non-tertiary education",
                                   ifelse(data$edu_highest=="A levels/AS levels or equivalent", "ISCED 3: Upper secondary education",
                                          ifelse(data$edu_highest=="O levels/GCSEs or equivalent", "ISCED 2: Lower secondary education",
                                                 ifelse(data$edu_highest=="CSEs or equivalent", "ISCED 2: Lower secondary education",
                                                        ifelse(data$edu_highest=="None of the above", "ISCED 1: Primary education",
                                                               ifelse(data$edu_highest=="Prefer not to answer", NA, NA))))))))

data$ISCED <- factor(data$ISCED, 
                     levels=c("ISCED 5: First stage of tertiary education", "ISCED 4: Post-secondary non-tertiary education", 
                              "ISCED 3: Upper secondary education", "ISCED 2: Lower secondary education",
                              "ISCED 1: Primary education"))

# Convert "missing" employment to "unemployed" so it doesn't interfere with Cox regression
levels(data$employment) <- c(levels(data$employment), "Unemployed/retired/other")
data$employment[is.na(data$employment)] <- "Unemployed/retired/other"

# Categorise age into 10-yr groups
data$agegrp <- cut(data$age, breaks=c(40, 50, 60, 70), right=FALSE)

# Categorise BMI into labelled categories
data$BMIcat <- cut(data$bl_BMI, breaks=c(0, 18.5, 25, 30, 200), right=FALSE)
data$BMIcat <- factor(data$BMIcat, levels=c("[0,18.5)", "[18.5,25)", "[25,30)", "[30,200)"), labels=c("Underweight", "Normal", "Overweight", "Obese"))

# Define "binge" levels of alcohol consumption
data$alc_binge[data$gender=="Female"] <- data[data$gender=="Female",]$weekly_alcunits>7 & !is.na(data[data$gender=="Female",]$weekly_alcunits)
data$alc_binge[data$gender=="Male"] <- data[data$gender=="Male",]$weekly_alcunits>14 & !is.na(data[data$gender=="Male",]$weekly_alcunits)

# Add mapping to comorbidities of interest
comorbidities <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VI_ComorbidityCategories.rds")
data <- merge(data, comorbidities, by="ID", all.x=TRUE)

# Add mapping to comorbidity groups B and C
comorbgrps <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypGroupBC.rds")
data <- merge(data, comorbgrps[,c("ID", "Group_B", "Group_C")], by="ID", all.x=TRUE) 


saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTN_excl.rds")
