#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(forcats)

#--------------------------------------------------------------------------------------------------------------
inpath <- readChar("./data/raw/filepath.txt", file.info("./data/raw/filepath.txt")$size)
outfile <- "./data/clean/covars.rds"
#--------------------------------------------------------------------------------------------------------------

# Read in the baseline characteristics
covars <- readRDS("./data/clean/basechar.rds")

# Rename variables
names(covars)[names(covars)=="BaC_RsnLostFU"] <- "lfureason"
names(covars)[names(covars)=="BaC_DeprivInd"] <- "townsend_depind"
names(covars)[names(covars)=="DateLostFU"] <- "lfudate"
names(covars)[names(covars)=="DV_DateOfBirth"] <- "dob"
names(covars)[names(covars)=="DV_AgeAtRec"] <- "age"
names(covars)[names(covars)=="BaC_Sex"] <- "gender"
names(covars)[names(covars)=="DV_DateAssess"] <- "recdate"

# Categorise age into 10-yr groups
covars$agegrp <- cut(covars$age, breaks=c(40, 50, 60, 70), right=FALSE)

#--------------------------------------------------------------------------------------------------------------
# Ethnicity
ethnicity <- readRDS(paste0(inpath, "Eth_base.rds"))

# Reorder the ethnicity factor levels to make more sense when tabulating
ethnicity$Eth_Ethnicity <- factor(ethnicity$Eth_Ethnicity, levels=c("White", "British", "Irish", "Any other white background",
                                                                   "Mixed", "White and Black Caribbean", "White and Black African", 
                                                                   "White and Asian", "Any other mixed background",
                                                                   "Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", 
                                                                   "Any other Asian background",
                                                                   "Black or Black British", "Caribbean", "African", "Any other Black background",
                                                                   "Chinese", "Other ethnic group", "Do not know", "Prefer not to answer"))

# Collapse ethnic groups into broader categories
ethnicity$ethnicity <- as.character(ethnicity$eth_group)
ethnicity$ethnicity <- ifelse(ethnicity$ethnicity=="White", "White", 
                         ifelse(ethnicity$ethnicity=="Prefer not to answer" | is.na(ethnicity$ethnicity), "Unknown",
                                "Non-white"))
ethnicity$ethnicity <- factor(ethnicity$ethnicity, levels=c("White", "Non-white", "Unknown"))

covars <- merge(covars, ethnicity[,c("ID", "Eth_Ethnicity", "ethnicity")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Smoking
smoking <- readRDS(paste0(inpath, "Smo_base.rds"))

smoking$Smo_Status <- factor(smoking$Smo_Status, levels=c("Never", "Previous", "Current", "Prefer not to answer"), 
                             labels=c("Never", "Previous", "Current", "Unanswered"), ordered=FALSE)
smoking$Smo_Status[is.na(smoking$Smo_Status)] <- "Unanswered"

covars <- merge(covars, smoking[,c("ID", "Smo_Status", "Smo_TobaccoCurr", "Smo_TobaccoPast")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Alcohol
alcohol <- readRDS(paste0(inpath, "Alc_base.rds"))
alcohol$Alc_Status <- factor(alcohol$Alc_Status, levels=c("Never", "Previous", "Current", "Prefer not to answer"), ordered=FALSE)

for(var in c("Alc_RedWineWk", "Alc_WhiteWineWk", "Alc_BeerCiderWk", "Alc_SpiritsWk", "Alc_FortWineWk", "Alc_OtherAlcWk")){
  alcohol[[var]] <- ifelse(alcohol[[var]]<0|is.na(alcohol[[var]]), 0, alcohol[[var]])
}
alcohol$weekly_alcunits <- alcohol$Alc_RedWineWk + alcohol$Alc_WhiteWineWk + alcohol$Alc_BeerCiderWk + alcohol$Alc_SpiritsWk + alcohol$Alc_FortWineWk + alcohol$Alc_OtherAlcWk

# Truncate alcohol consumption at upper 95th percentile
upper95 <- quantile(data$weekly_alcunits, 0.95, na.rm=TRUE)
alcohol$weekly_alcunits[alcohol$weekly_alcunits>upper95] <- upper95
alcohol$weekly_alcunits[is.na(alcohol$weekly_alcunits)] <- 0

# Define "binge" levels of alcohol consumption
alcohol$alc_binge[alcohol$gender=="Female"] <- alcohol[alcohol$gender=="Female",]$weekly_alcunits>7 & !is.na(data[data$gender=="Female",]$weekly_alcunits)
alcohol$alc_binge[alcohol$gender=="Male"] <- alcohol[alcohol$gender=="Male",]$weekly_alcunits>14 & !is.na(data[data$gender=="Male",]$weekly_alcunits)
alcohol$alc_binge_ <- factor(as.numeric(data$alc_binge), levels=c(0,1), labels=c("Safe alcohol use", "Harmful alcohol use"))

covars <- merge(covars, alcohol[,c("ID", "Alc_Status", "Alc_Freq", "weekly_alcunits")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Household
household <- readRDS(paste0(inpath, "HoH_base.rds"))
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
household$income <- factor(household$income, levels=c("Less than 18,000", "18,000 to 30,999",
                                                      "31,000 to 51,999", "52,000 to 100,000", "Greater than 100,000", 
                                                      "Do not know", "Unanswered"))                      

covars <- merge(covars, household[,c("ID", "income", "HoH_HouseholdSize.0")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Sleep
sleep <- readRDS(paste0(inpath, "Sle_base.rds"))

covars <- merge(covars, sleep, by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Body measurements
body <- readRDS(paste0(inpath, "BSM_base.rds"))

# Rename some variables
names(body)[names(body)=="BSM_BMI"] <- "BMI"
names(body)[names(body)=="BSM_Waist"] <- "WaistCirc"

# Categorise BMI into labelled categories
body$BMIcat <- as.character(cut(body$BMI, breaks=c(0, 18.5, 25, 30, 200), right=FALSE))
body$BMIcat[is.na(body$BMIcat)] <- "Unknown"
body$BMIcat <- factor(body$BMIcat, levels=c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)", "Unknown"), 
                      labels=c("Normal", "Underweight", "Overweight", "Obese", "Unknown"))

# Categorise waist circ into labelled categories
body$WaistCircCat <- dplyr::case_when(
  body$gender=="Female" & body$WaistCirc>=88 ~ "Obese",
  body$gender=="Female" & body$WaistCirc>=80 ~ "Overweight",
  body$gender=="Male" & body$WaistCirc>=102 ~ "Obese",
  body$gender=="Male" & body$WaistCirc>=94 ~ "Overweight",
  is.na(body$WaistCirc) ~ "Unknown",
  TRUE ~ "Normal"
)
body$WaistCircCat <- factor(body$WaistCircCat, levels=c("Normal", "Overweight", "Obese", "Unknown"))


covars <- merge(covars, body[,c("ID", "BMI", "BMIcat", "WaistCirc", "WaistCircCat")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Education
education <- readRDS(paste0(inpath, "Edu_base.rds"))
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

# Convert UKB qualification categories into ISCED education categories
education$ISCED <- dplyr::case_when(
  education$edu_highest == "College or University degree" ~ "ISCED 5: First stage of tertiary education",
  education$edu_highest == "NVQ or HND or HNC or equivalent" ~ "ISCED 5: First stage of tertiary education",
  education$edu_highest == "Other professional qualifications eg: nursing, teaching" ~ "ISCED 4: Post-secondary non-tertiary education",
  education$edu_highest == "A levels/AS levels or equivalent" ~ "ISCED 3: Upper secondary education",
  education$edu_highest == "O levels/GCSEs or equivalent" ~ "ISCED 2: Lower secondary education",
  education$edu_highest == "CSEs or equivalent" ~ "ISCED 2: Lower secondary education",
  education$edu_highest == "None of the above" ~ "ISCED 1: Primary education",
  education$edu_highest == "Prefer not to answer" ~ "Unanswered",
  is.na(education$edu_highest) ~ "Unanswered"
)

education$ISCED <- factor(education$ISCED, 
                     levels=c("ISCED 5: First stage of tertiary education", "ISCED 4: Post-secondary non-tertiary education", 
                              "ISCED 3: Upper secondary education", "ISCED 2: Lower secondary education",
                              "ISCED 1: Primary education", "Unanswered")) 

covars <- merge(covars, education[,c("ID", "uni", "eduNA", "edu_highest", "Edu_Age.0", "ISCED")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Employment
jobcodes <- readRDS(paste0(inpath, "Jobcodes_pts.rds"))
jobcodes$employment <- jobcodes$TL

employment <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Emp_base.rds")
employment$shiftwork <- employment$Emp_ShiftWrk.0

covars <- merge(covars, jobcodes[,c("ID", "employment")], by="ID", all=TRUE)
covars <- merge(covars, employment[,c("ID", "shiftwork")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Physical Activity
phys_act <- readRDS(paste0(inpath, "Pha_base.rds"))

covars <- merge(covars, phys_act[,c("ID", "PhA_METsWkAllAct")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Cognitive Function
cogfunc <- readRDS(paste0(inpath, "CoF_base.rds"))

covars <- merge(covars, cogfunc[,c("ID", "CoF_RTTTimeID")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Family History
fam <- readRDS(paste0(inpath, "FaH_base.rds"))

fam$FaH_CVD <- apply(fam[,c(grep("FaH_FatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_MotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_SibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Heart disease", "Stroke")))

covars <- merge(covars, fam[,c("ID", "FaH_CVD")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Other from verbal interview
veint <- readRDS(paste0(inpath, "VeIcovars_base.rds"))

veint$BirthCountry[is.na(veint$BirthCountry)] <- "United Kingdom"
veint$BirthContinent[is.na(veint$BirthContinent)] <- "Europe"
veint$BirthCountryIncomeLevel[is.na(veint$BirthCountryIncomeLevel)] <- "HUK"
covars <- merge(covars, veint[,c("ID", "BirthContinent", "BirthCountry", "BirthCountryIncomeLevel", 
                                 "VeI_Ncancer.0", "VeI_NNonCancer.0", "VeI_NOperation.0", "VeI_Ntreatments")],
                by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Save the resulting conglomerate
saveRDS(covars, file=outfile)