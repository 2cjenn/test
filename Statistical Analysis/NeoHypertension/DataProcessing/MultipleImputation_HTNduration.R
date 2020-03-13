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

varlist <- c("age", "controlled", "gender", "BMIcat", "Smo_Status", "GroupB", "GroupC", 
             "HTNdx_duration", "hypmedsno", "townsend_depind", "income", "ISCED", "employment", 
             "BirthCountryIncomeLevel", "BowelCancerScreening")
data <- data[data$treated==TRUE & !is.na(data$treated),c("ID", varlist)]

model <- glm(controlled~age + HTNdx_duration, data=data, family="binomial")
model

imputed_data <- mice(data, m=5, maxit=50, method="pmm", seed=500)
summary(imputed_data)