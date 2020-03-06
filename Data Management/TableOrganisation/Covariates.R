#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(forcats)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
#--------------------------------------------------------------------------------------------------------------
# Smoking
smoking <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Smo_base.rds")
smoking$Smo_Status <- factor(smoking$Smo_Status, levels=c("Never", "Previous", "Current", "Prefer not to answer"), ordered=FALSE)

#--------------------------------------------------------------------------------------------------------------
# Alcohol
alcohol <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Alc_base.rds")
alcohol$Alc_Status <- factor(alcohol$Alc_Status, levels=c("Never", "Previous", "Current", "Prefer not to answer"), ordered=FALSE)

alcohol$weekly_alcunits <- alcohol$Alc_RedWineWk + alcohol$Alc_WhiteWineWk + alcohol$Alc_BeerCiderWk + alcohol$Alc_SpiritsWk + alcohol$Alc_FortWineWk + alcohol$Alc_OtherAlcWk

covars <- merge(smoking[,c("ID", "Smo_Status", "Smo_TobaccoCurr", "Smo_TobaccoPast")], 
                alcohol[,c("ID", "Alc_Status", "Alc_Freq", "weekly_alcunits")], by="ID", all=TRUE)
#--------------------------------------------------------------------------------------------------------------
# Household
household <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\HoH_base.rds")
household$income <- as.character(household$HoH_PreTaxInc.0)
household$income[is.na(household$income)] <- as.character(household$HoH_PreTaxInc_P.0[is.na(household$income)])
household$income[household$income=="Prefer not to answer"] <- NA
household$income <- fct_collapse(household$income, 
                                 "Less than 18,000" = "Less than 18,000",
                                 "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
                                 "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
                                 "Greater than 100,000" = "Greater than 100,000",
                                 "Do not know" = "Do not know"
                                 )
household$income <- factor(household$income, levels=c("Less than 18,000", "18,000 to 30,999",
                                                      "31,000 to 51,999", "Greater than 100,000", "Do not know"))                      

covars <- merge(covars, household[,c("ID", "income", "HoH_HouseholdSize.0")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Sleep
sleep <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Sle_base.rds")

covars <- merge(covars, sleep, by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Body measurements
body <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\BSM_base.rds")
body$bl_BMI <- body$BSM_BMI

covars <- merge(covars, body[,c("ID", "BSM_HeightStand", "BSM_Weight", "bl_BMI")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Education
education <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Edu_base.rds")
education$uni <- apply(education[,grep("Edu_Qualif", colnames(education), fixed=TRUE)], 1, function(x) any(x == "College or University degree" & !is.na(x)))
education$eduNA <- apply(education[,grep("Edu_Qualif", colnames(education), fixed=TRUE)], 1, function(x) any(x == "Prefer not to answer" & !is.na(x)))

education$edu_highest <- NA
for(qual in c("College or University degree",
              "NVQ or HND or HNC or equivalent",
              "Other professional qualifications eg: nursing, teaching",
              "A levels/AS levels or equivalent",
              "O levels/GCSEs or equivalent",
              "CSEs or equivalent",
              "None of the above")) {
  education$edu_highest[is.na(education$edu_highest)] <- ifelse(
    apply(education[is.na(education$edu_highest),grep("Edu_Qualif", colnames(education), fixed=TRUE)], 1, 
          function(x) any(x == qual & !is.na(x))), 
    qual, NA
  )
}
education$edu_highest <- factor(education$edu_highest, 
                                levels=c("College or University degree", "NVQ or HND or HNC or equivalent", "Other professional qualifications eg: nursing, teaching", "A levels/AS levels or equivalent", "O levels/GCSEs or equivalent", "CSEs or equivalent", "None of the above"),
                                ordered=TRUE)


covars <- merge(covars, education[,c("ID", "uni", "eduNA", "edu_highest", "Edu_Age.0")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Employment
employment <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Jobcodes_pts.rds")
employment$employment <- employment$TL

covars <- merge(covars, employment[,c("ID", "employment")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Physical Activity
phys_act <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Pha_base.rds")

covars <- merge(covars, phys_act[,c("ID", "PhA_METsWkAllAct")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Other from verbal interview
veint <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeIcovars_base.rds")
names(veint)[names(veint)=="VeI_Ncancer.0"] <- "NumberCancers"
names(veint)[names(veint)=="VeI_NNonCancer.0"] <- "NumberDiagnoses"
names(veint)[names(veint)=="VeI_NOperation.0"] <- "NumberOperations"
names(veint)[names(veint)=="VeI_Ntreatments"] <- "NumberMedications"

veint$BirthCountry[is.na(veint$BirthCountry)] <- "United Kingdom"
veint$BirthContinent[is.na(veint$BirthContinent)] <- "Europe"
veint$BirthCountryIncomeLevel[is.na(veint$BirthCountryIncomeLevel)] <- "H"
covars <- merge(covars, veint[,c("ID", "BirthContinent", "BirthCountry", "BirthCountryIncomeLevel", 
                                 "NumberCancers", "NumberDiagnoses", "NumberOperations", "NumberMedications")],
                by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Save the resulting conglomerate
saveRDS(covars, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\covars.rds")