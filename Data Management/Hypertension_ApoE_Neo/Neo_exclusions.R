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
# Further exclusions for covariates of interest
#--------------------------------------------------------------------------------------------------------------

# Exclude individuals without highest level of education
# n = 498696 - 489006 = 
data <- data[!is.na(data$edu_highest),]

# Exclude individuals who have serious health conditions
# n = 489006 - 483794 = 
seriouscomorbid <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypExclude.rds")
data <- data[!data$ID %in% seriouscomorbid$ID[!is.na(seriouscomorbid$Yes)],]

# Exclude individuals with cancer (except for skin cancer?)
# n = 483752 - 443748 = 
data <- data[data$NumberCancers==0,]

# Exclude individuals with severely limited function (hand grip, reaction time)
# n = 443748 - 
data <- data[data$mean_reacttime<1500,]

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

# Add mapping to comorbidities of interest
comorbidities <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VI_ComorbidityCategories.rds")
data <- merge(data, comorbidities, by="ID", all.x=TRUE)

# Add mapping to comorbidity groups B and C
comorbgrps <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypGroupBC.rds")
data <- merge(data, comorbgrps[,c("ID", "Group_B", "Group_C")], by="ID", all.x=TRUE) 


saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTN_excl.rds")
