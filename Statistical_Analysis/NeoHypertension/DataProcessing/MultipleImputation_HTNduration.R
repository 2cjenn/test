#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 12/03/2020
# First attempt at multiple imputation to impute hypertension diagnosis durations (~23k missing)
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(lubridate)
library(mice)

#--------------------------------------------------------------------------------------------------------------
# Load the data
#--------------------------------------------------------------------------------------------------------------

data <- readRDS(file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTN_excl.rds")

varlist <- c("controlled", "age", "gender", "BMIcat", "Smo_Status", 
             "CVD", "Diabetes", "Asthma_or_COPD", "Back_pain", "Depression", "Migraine", "Other_chronic_comorbidities", 
             "HTNdx_duration", "hypmedsno", "townsend_depind", "income", "ISCED", "employment", 
             "BirthCountryIncomeLevel", "BowelCancerScreening")
data <- data[data$treated==TRUE & !is.na(data$treated),c("ID", varlist)]

model <- glm(controlled~age + HTNdx_duration, data=data, family="binomial")
model

imputeset <- data[,-c(which(names(data) %in% c("controlled")))]
imputeset <- data[,c("ID", "age", "gender", "controlled", "HTNdx_duration")]
imputed_data <- mice(imputeset, m=5, maxit=50, method="pmm", seed=500)
imputed_data2 <- mice(imputeset, m=5, maxit=5, method="pmm", seed=500)
summary(imputed_data2)

modelfit <- with(imputed_data2, glm(controlled ~ age + gender + HTNdx_duration, family="binomial"))
summary(pool(modelfit))
