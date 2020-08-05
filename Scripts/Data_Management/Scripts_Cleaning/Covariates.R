#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(forcats)
library(yaml)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
#--------------------------------------------------------------------------------------------------------------
# Smoking
smoking <- readRDS(paste0(config$data$received, "Smo_base.rds"))
smoking$Smo_Status <- factor(smoking$Smo_Status, levels=c("Never", "Previous", "Current", "Prefer not to answer"), 
                             labels=c("Never", "Previous", "Current", "Unanswered"), ordered=FALSE)
smoking$Smo_Status[is.na(smoking$Smo_Status)] <- "Unanswered"

#--------------------------------------------------------------------------------------------------------------
# Alcohol
alcohol <- readRDS(paste0(config$data$received, "Alc_base.rds"))
alcohol$Alc_Status <- factor(alcohol$Alc_Status, levels=c("Never", "Previous", "Current", "Prefer not to answer"), 
                             ordered=FALSE)
alcohol$Alc_Freq <- factor(alcohol$Alc_Freq, levels=c("Never", "Special occasions only", "One to three times a month",
                                                      "Once or twice a week", "Three or four times a week",
                                                      "Daily or almost daily",  "Prefer not to answer"),
                           ordered=FALSE)

for(var in c("Alc_RedWineWk", "Alc_WhiteWineWk", "Alc_BeerCiderWk", "Alc_SpiritsWk", "Alc_FortWineWk", "Alc_OtherAlcWk")){
  # alcohol[[var]] <- ifelse(alcohol[[var]]<0|is.na(alcohol[[var]]), 0, alcohol[[var]])
  alcohol[[var]][alcohol$Alc_Freq %in% c("Never", "Special occasions only", "One to three times a month")] <- 0
  alcohol[[var]][alcohol[[var]]<0|is.na(alcohol[[var]])] <- 0
}
# https://www.nhs.uk/live-well/alcohol-support/calculating-alcohol-units/
# strength (ABV) x volume (ml) ? 1,000 = units
alcohol$weekly_alcunits <- 
  #	Red wine (1 glass, 125ml, ABV 12% = 1.5 units) 
  (1.5 * alcohol$Alc_RedWineWk) + 
  # White wine, champagne (1 glass, 125ml, ABV 12% = 1.5 units) 
  (1.5 * alcohol$Alc_WhiteWineWk) + 
  #	Fortified wines: e.g. sherry, port (1 measure, 50ml, ABV 20% = 1 unit)
  (1.0 * alcohol$Alc_FortWineWk) + 
  #	Beer, cider including bitter, lager, stout, ale, Guinness (1 pint, 568ml, ABV 3.6% = 2 units) 
  (2.0 * alcohol$Alc_BeerCiderWk) +
  #	Spirits, liquors (1 measure or shot, 25ml, ABV 40% = 1 unit) 
  (1.0 * alcohol$Alc_SpiritsWk) +
  #	For "other" types of alcohol, will use alcopops as proxy ( 1 drink, 275ml, ABV 5.5% = 1.5 units)
  (1.5 * alcohol$Alc_OtherAlcWk)

covars <- merge(smoking[,c("ID", "Smo_Status", "Smo_TobaccoCurr", "Smo_TobaccoPast")], 
                alcohol[,c("ID", "Alc_Status", "Alc_Freq", "weekly_alcunits")], by="ID", all=TRUE)
#--------------------------------------------------------------------------------------------------------------
# Household
household <- readRDS(paste0(config$data$received, "HoH_base.rds"))
household$income <- as.character(household$HoH_PreTaxInc.0)
household$income[is.na(household$income)] <- as.character(household$HoH_PreTaxInc_P.0[is.na(household$income)])
household$income[household$income=="Prefer not to answer" | is.na(household$income)] <- "Unanswered"
household$income <- fct_collapse(household$income, 
                                 "Less than 18,000" = "Less than 18,000",
                                 "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
                                 "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
                                 "52,000 to 100,000" = "52,000 to 100,000",
                                 "Greater than 100,000" = "Greater than 100,000",
                                 "Do not know" = "Do not know",
                                 "Unanswered" = "Unanswered"
)
household$income <- factor(household$income, levels=c("Greater than 100,000", "52,000 to 100,000",
                                                      "31,000 to 51,999", "18,000 to 30,999",
                                                      "Less than 18,000", 
                                                      "Do not know", "Unanswered"))                      

covars <- merge(covars, household[,c("ID", "income", "HoH_HouseholdSize.0")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Sleep
sleep <- readRDS(paste0(config$data$received, "Sle_base.rds"))

covars <- merge(covars, sleep, by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Body measurements
body <- readRDS(paste0(config$data$received, "BSM_base.rds"))
body$BMI <- body$BSM_BMI
body$WaistCirc <- body$BSM_Waist

covars <- merge(covars, body[,c("ID", "BSM_HeightStand", "BSM_Weight", "BMI", "WaistCirc")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Education
education <- readRDS(paste0(config$data$received, "Edu_base.rds"))
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
jobcodes <- readRDS(paste0(config$data$derived, "Jobcodes_pts.rds"))
jobcodes$employment <- jobcodes$TL

employment <- readRDS(paste0(config$data$received, "Emp_base.rds"))
employment$shiftwork <- employment$Emp_ShiftWrk.0

covars <- merge(covars, jobcodes[,c("ID", "employment", "TEU_EmpCat")], by="ID", all=TRUE)
covars <- merge(covars, employment[,c("ID", "shiftwork")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Physical Activity
phys_act <- readRDS(paste0(config$data$received, "Pha_base.rds"))

covars <- merge(covars, phys_act[,c("ID", "PhA_METsWkAllAct")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Other from verbal interview
veint <- readRDS(paste0(config$data$derived, "VeIcovars_base.rds"))
names(veint)[names(veint)=="VeI_Ncancer.0"] <- "NumberCancers"
names(veint)[names(veint)=="VeI_NNonCancer.0"] <- "NumberDiagnoses"
names(veint)[names(veint)=="VeI_NOperation.0"] <- "NumberOperations"
names(veint)[names(veint)=="VeI_Ntreatments"] <- "NumberMedications"

veint$BirthCountry[is.na(veint$BirthCountry)] <- "United Kingdom"
veint$BirthContinent[is.na(veint$BirthContinent)] <- "Europe"
veint$BirthCountryIncomeLevel[is.na(veint$BirthCountryIncomeLevel)] <- "HUK"
covars <- merge(covars, veint[,c("ID", "BirthContinent", "BirthCountry", "BirthCountryIncomeLevel", "VeI_PregnantNow",
                                 "NumberCancers", "NumberDiagnoses", "NumberOperations", "NumberMedications")],
                by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Save the resulting conglomerate
saveRDS(covars, file=paste0(config$data$derived, "covars.rds"))
