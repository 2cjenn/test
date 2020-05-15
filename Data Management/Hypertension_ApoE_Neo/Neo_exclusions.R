#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(lubridate)
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(svglite)
library(rsvg)
library(png)

#--------------------------------------------------------------------------------------------------------------
data <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_raw.rds")

#--------------------------------------------------------------------------------------------------------------
# Create some more complex variables
#--------------------------------------------------------------------------------------------------------------

# Duration of hypertension diagnosis
data$HTNdx_TQ <- round(data$age - data$HBPAge,1)
data$HTNdx_VI <- round(decimal_date(data$recdate) - data$VIhypdx_yr,1)
data$HTNdx_duration <- coalesce(data$HTNdx_VI, data$HTNdx_TQ)
# Any durations less than 0 are clearly errors (only 2)
# Any diagnoses before the age of about 20 are probably due to other causes
data$HTNdx_duration[(data$HTNdx_duration < 0 | data$HTNdx_duration > data$HBPAge-20)& !is.na(data$HTNdx_duration)] <- NA

# Single variable for self-reported hypertension
# If reported in either touchscreen questionnaire or verbal interview
data$selfrephyp <- (data$prevHBP==TRUE & !is.na(data$prevHBP)) | data$VIhyp==TRUE
data$selfrephyp[is.na(data$prevHBP) & is.na(data$NumberDiagnoses)] <- NA

# data$selfrephyp.sens <- data$VIhyp==TRUE
# data$selfrephyp.sens[is.na(data$NumberDiagnoses)] <- NA
# 
# data$selfrephyp <- data$selfrephyp.sens

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

excl$BPmiss=nrow(data)

# Exclude individuals with implausible BP data
data <- data[data$SBP >= 70 & data$SBP <= 270,]
data <- data[data$DBP >= 50 & data$DBP <= 150,]

excl$BPimp=nrow(data)

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
data$c40_age <- data$age - 40

# Make the key dichotomous variables into factors to improve readability of outputs
data$prevHBP_ <- factor(as.numeric(data$prevHBP), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (touchscreen)", "Self-reported prior HTN diagnosis in touchscreen questionnaire"))
data$VIhyp_ <- factor(as.numeric(data$VIhyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis (VI)", "Self-reported prior HTN diagnosis in verbal interview"))
data$selfrephyp_ <- factor(as.numeric(data$selfrephyp), levels=c(0,1), labels=c("Did not report prior HTN diagnosis", "Self-reported prior HTN diagnosis"))
data$measuredhyp_ <- factor(as.numeric(data$measuredhyp), levels=c(0,1), labels=c("Measured BP < 140/90 at baseline", "Measured BP >= 140/90 at baseline"))
data$controlled_ <- factor(as.numeric(data$controlled), levels=c(0,1), labels=c("Sub-optimally treated", "Controlled"))
data$aware_ <- factor(as.numeric(data$aware), levels=c(0,1), labels=c("Unaware of hypertension", "Aware of hypertension"))
data$treated_ <- factor(as.numeric(data$treated), levels=c(0,1), labels=c("Did not report BP medication", "Self-reported BP medication"))
data$evidenceHTN_ <- factor(as.numeric(data$evidenceHTN), levels=c(0,1), labels=c("No evidence of hypertension", "Normotensive"))


# saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_excl.rds")


# data <- readRDS(file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_excl.rds")
#--------------------------------------------------------------------------------------------------------------
# Further exclusions for Neo's covariates of interest
#--------------------------------------------------------------------------------------------------------------

# Exclude individuals without highest level of education
# n = 498698 - 489006 = 
# data <- data[!is.na(data$edu_highest),]

# excl$education <- nrow(data)

# Exclude individuals who have serious health conditions
# n = 489006 - 483794 = 5212
seriouscomorbid <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypExclude.rds")
data <- data[!data$ID %in% seriouscomorbid$ID[!is.na(seriouscomorbid$Yes)],]

excl$seriouscomorb <- nrow(data)

# And individuals with cancer (except for skin cancer?)
# n = 483794 - 443782 = 40,012
cancer <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Cancer_pts.rds")
exceptskincancer <- cancer$ID[cancer$TL!="skin cancer"]
data <- data[!data$ID %in% exceptskincancer,]

excl$cancer <- nrow(data)

# # Exclude individuals with severely limited function (hand grip, reaction time)
# # n = 443782 - 443627 = 155
# data <- data[data$mean_reacttime<1500 | is.na(data$mean_reacttime),]
# 
# excl$limitfunc <- nrow(data)

excl$hypert <- nrow(data[data$evidenceHTN==TRUE,])
excl$aware <- nrow(data[data$aware==TRUE & !is.na(data$aware),])
excl$treat <- nrow(data[data$treated==TRUE & !is.na(data$treated),])

# Create exclusion flowchart
export_svg(DiagrammeR::grViz("K:\\TEU\\APOE on Dementia\\Statistical Analysis\\NeoHypertension\\RMarkdown\\ExclusionFlowchart.gv")
           ) %>% charToRaw %>% rsvg %>% png::writePNG("K:\\TEU\\APOE on Dementia\\Statistical Analysis\\NeoHypertension\\RMarkdown\\ExclFlowchart.png")

#--------------------------------------------------------------------------------------------------------------
# Neo's specific variants of covariate variables
#--------------------------------------------------------------------------------------------------------------

# Collapse ethnic groups into broader categories
data$ethnicity <- as.character(data$eth_group)
data$ethnicity <- ifelse(data$ethnicity=="White", "White", 
                         ifelse(data$ethnicity=="Do not know", "Do not know",
                                ifelse(data$ethnicity=="Prefer not to answer" | is.na(data$ethnicity), "Unanswered",
                                       "Non-white")))
data$ethnicity <- factor(data$ethnicity, levels=c("White", "Non-white", "Do not know", "Unanswered"))

# Convert UKB qualification categories into ISCED education categories
data$ISCED <- dplyr::case_when(
  data$edu_highest == "College or University degree" ~ "5: Tertiary",
  data$edu_highest == "NVQ or HND or HNC or equivalent" ~ "5: Tertiary",
  data$edu_highest == "Other professional qualifications eg: nursing, teaching" ~ "4: Post-secondary non-tertiary",
  data$edu_highest == "A levels/AS levels or equivalent" ~ "2-3: Secondary",
  data$edu_highest == "O levels/GCSEs or equivalent" ~ "2-3: Secondary",
  data$edu_highest == "CSEs or equivalent" ~ "2-3: Secondary",
  data$edu_highest == "None of the above" ~ "1: Primary",
  data$edu_highest == "Prefer not to answer" ~ "Unanswered",
  is.na(data$edu_highest) ~ "Unanswered"
  )

data$ISCED <- factor(data$ISCED, 
                     levels=c("5: Tertiary", "4: Post-secondary non-tertiary", 
                              "2-3: Secondary" , "1: Primary" , "Unanswered")) # Excluding "Unanswered" from factor levels codes it as NA

# Convert "missing" employment to "unemployed" so it doesn't interfere with Cox regression
levels(data$employment) <- c(levels(data$employment), "Unemployed/retired/other")
data$employment[is.na(data$employment)] <- "Unemployed/retired/other"
data$employment <- factor(data$employment, levels=c("Managers and Senior Officials", "Professional Occupations",
                                                    "Associate Professional and Technical Occupations",
                                                    "Administrative and Secretarial Occupations",
                                                    "Skilled Trades Occupations", "Personal Service Occupations",
                                                    "Sales and Customer Service Occupations", "Process, Plant and Machine Operatives",
                                                    "Elementary Occupations", "Other job (free text entry)", "Unemployed/retired/other"))

data$employcat <- dplyr::case_when(
  data$employment %in% c("Managers and Senior Officials", "Professional Occupations",
                         "Associate Professional and Technical Occupations",
                         "Administrative and Secretarial Occupations") ~ "White collar",
  data$employment == "Skilled Trades Occupations" ~ "Skilled trades",
  data$employment %in% c("Personal Service Occupations",
                         "Sales and Customer Service Occupations") ~ "Services",
  data$employment %in% c("Process, Plant and Machine Operatives",
                         "Elementary Occupations") ~ "Blue collar",
  data$employment == "Other job (free text entry)" ~ "Other",
  data$employment == "Unemployed/retired/other" ~ "Unemployed/retired/unanswered",
  TRUE ~ "Error?"
)
data$employcat <- factor(data$employcat, 
                         levels=c("White collar", "Skilled trades", "Services", 
                                  "Blue collar", "Other", "Unemployed/retired/unanswered"), 
                         labels=c("Professional and Administrative", "Skilled trades", "Services", 
                                  "Manual and Industrial", "Other", "Unemployed/retired/unanswered"),
                         ordered=FALSE)

# Categorise age into 10-yr groups
data$agegrp <- cut(data$age, breaks=c(40, 50, 60, 70), right=FALSE,
                   labels=c("40-49", "50-59", "60-69"))

# Categorise BMI into labelled categories
data$BMIcat <- as.character(cut(data$BMI, breaks=c(0, 18.5, 25, 30, 200), right=FALSE))
data$BMIcat[is.na(data$BMIcat)] <- "Unanswered"
data$BMIcat <- factor(data$BMIcat, levels=c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)", "Unanswered"), 
                      labels=c("Normal (ref)", "Underweight", "Overweight", "Obese", "Unanswered"))

# Categorise waist circ into labelled categories
data$WaistCircCat <- dplyr::case_when(
  data$gender=="Female" & data$WaistCirc>=88 ~ "Obese",
  data$gender=="Female" & data$WaistCirc>=80 ~ "Overweight",
  data$gender=="Male" & data$WaistCirc>=102 ~ "Obese",
  data$gender=="Male" & data$WaistCirc>=94 ~ "Overweight",
  is.na(data$WaistCirc) ~ "Unanswered",
  TRUE ~ "Normal"
)
data$WaistCircCat <- factor(data$WaistCircCat, levels=c("Normal", "Overweight", "Obese", "Unanswered"))

# Truncate alcohol consumption at upper 95th percentile
upper95 <- quantile(data$weekly_alcunits, 0.95, na.rm=TRUE)
data$weekly_alcunits[data$weekly_alcunits>upper95] <- upper95
data$weekly_alcunits[is.na(data$weekly_alcunits)] <- 0


# Categorise alcohol consumption
data$weekly_alccat <- cut(data$weekly_alcunits, breaks=c(-1, 0, 5, 10, 20, 30, 100),
                             labels=c("None reported", "Less than 5 units", "5 to 10 units", 
                                      "10 to 20 units", "20 to 30 units", "More than 30 units"))


# Define "binge" levels of alcohol consumption
data$alc_heavyuse[data$gender=="Female"] <- data[data$gender=="Female",]$weekly_alcunits>7 & !is.na(data[data$gender=="Female",]$weekly_alcunits)
data$alc_heavyuse[data$gender=="Male"] <- data[data$gender=="Male",]$weekly_alcunits>14 & !is.na(data[data$gender=="Male",]$weekly_alcunits)
data$alc_heavyuse_ <- factor(as.numeric(data$alc_heavyuse), levels=c(0,1), labels=c("No", "Yes"))

# Indicator variable for whether physical activity > or <= 150 METs per day
data$METs_over1200 <- dplyr::case_when(
  data$PhA_METsWkAllAct > 1200 & !is.na(data$PhA_METsWkAllAct) ~ "High (METs > 1200)",
  data$PhA_METsWkAllAct <= 1200 & !is.na(data$PhA_METsWkAllAct) ~ "Low (METs <= 1200)",
  TRUE ~ "Unanswered")
data$METs_over1200 <- factor(data$METs_over1200, levels=c("High (METs > 1200)", "Low (METs <= 1200)", "Unanswered"))

# Convert bowel cancer screening to a factor
data$BowelCancerScreening <- as.character(data$BowelCancerScreening)
data$BowelCancerScreening[data$BowelCancerScreening %in% c("Prefer not to answer") | is.na(data$BowelCancerScreening)] <- "Unanswered"
data$BowelCancerScreening[data$BowelCancerScreening %in% c("Do not know")] <- "Do not know"
data$BowelCancerScreening <- factor(data$BowelCancerScreening, levels=c("Yes", "No", "Do not know", "Unanswered"), ordered=FALSE)

# Convert family history to a factor
data$FamilyHist_CVD_ <- factor(as.numeric(data$FaH_CVD), levels=c(0,1), labels=c("No", "Yes"))

# Convert income level of birth country to a factor
data$BirthCountryIncomeLevel[data$BirthCountryIncomeLevel %in% c("LM", "UM")] <- "M"
data$BirthCountryIncomeLevel <- factor(data$BirthCountryIncomeLevel, levels=c("HUK", "H", "M", "L"), 
                                       labels=c("UK", "Other high income", "Middle income", "Low income"))

# Convert Townsend deprivation index to quintiles (and a factor)
quintiles <- quantile(data$townsend_depind, probs=seq(0, 1, 0.2), na.rm=TRUE)
data$townsend_quint <- dplyr::case_when(
  data$townsend_depind <= quintiles[2] ~ "Q1: Least deprived",
  data$townsend_depind > quintiles[2] & data$townsend_depind <= quintiles[3] ~ "Q2",
  data$townsend_depind > quintiles[3] & data$townsend_depind <= quintiles[4] ~ "Q3",
  data$townsend_depind > quintiles[4] & data$townsend_depind <= quintiles[5] ~ "Q4",
  data$townsend_depind > quintiles[5] & data$townsend_depind <= quintiles[6] ~ "Q5: Most deprived",
  TRUE ~ "Unanswered"
)
data$townsend_quint <- factor(data$townsend_quint, 
                              levels=c("Q1: Least deprived", "Q2", "Q3", "Q4", "Q5: Most deprived", "Unanswered"))

# Convert assessment centre code to a country
data$countryResidence <- dplyr::case_when(
  data$assess_centre %in% c(10003, 11001, 11002, 11006, 11007, 11008, 11009, 
           11010, 11011, 11012, 11013, 11014, 11016, 11017, 
           11018, 11020, 11021, 11024, 11025, 11026, 11027, 11028) ~ "England",
  data$assess_centre %in% c(11004, 11005) ~ "Scotland",
  data$assess_centre %in% c(11003, 11022, 11023) ~ "Wales",
  TRUE ~ "Other"
)
data$countryResidence <- factor(data$countryResidence)

# Add hypertension severity indicator
data$HTNdx_severity <- dplyr::case_when(
  data$SBP>=180 | data$DBP>=110 ~ "Stage 3",
  between(data$SBP, 160, 180) | between(data$DBP, 100, 110) ~ "Stage 2",
  between(data$SBP, 140, 160) | between(data$DBP, 90, 100) ~ "Stage 1",
  TRUE ~ "Normotensive"
)
data$HTNdx_severity <- factor(data$HTNdx_severity, levels=c("Normotensive", "Stage 1", "Stage 2", "Stage 3"))

# Convert number of hypertensive medications to a categorical variables
data$antiHTNmedsno <- dplyr::case_when(
  data$hypmedsno==0 | is.na(data$hypmedsno) ~ "0",
  data$hypmedsno==1 ~ "1",
  data$hypmedsno==2 ~ "2",
  data$hypmedsno>=3 ~ ">=3",
  TRUE ~ as.character(data$hypmedsno)
)
data$antiHTNmedsno <- factor(data$antiHTNmedsno, levels=c("0", "1", "2", ">=3"))

# The aide_memoir shouldn't be an ordered factor
data$aide_memoir <- factor(data$aide_memoir, levels=c("Yes", "No"), ordered=FALSE,
                           labels=c("Brought an aide-memoir", "Did not bring an aide-memoir"))

# HTN duration categories
data$HTNdx_durcat <- as.character(cut(data$HTNdx_duration, breaks=c(0, 1, 2, 5, 10, 20, 100), right=FALSE))
data$HTNdx_durcat[is.na(data$HTNdx_durcat)] <- "Unanswered"
data$HTNdx_durcat <- factor(data$HTNdx_durcat, levels=c("[0,1)", "[1,2)", "[2,5)", "[5,10)", "[10,20)", "[20,100)", "Unanswered"),
                            labels=c("Less than 1 year", "1 to 2 years", "2 to 5 years", 
                                     "5 to 10 years", "10 to 20 years", "More than 20 years", "Unanswered")
                            )


# Add mapping to comorbidities of interest
comorbs <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VI_HTNcomorb.rds")
colnames(comorbs) <- c("ID", paste0("VI_", colnames(comorbs)[-1]))
data <- merge(data, comorbs, by="ID", all.x=TRUE)
for(comorb in names(comorbs)[-1]){
  data[[comorb]][is.na(data[[comorb]])] <- 0
  data[[comorb]][data[[comorb]]>1] <- 1
  data[[paste0(comorb,"_")]] <- data[[comorb]]>0 & !is.na(data[[comorb]])
  data[[paste0(comorb,"_")]] <- factor(as.numeric(data[[paste0(comorb,"_")]]), levels=c(0,1), labels=c("No", "Yes"))
}

data$comorbNumber <- rowSums(data[,colnames(comorbs)[-1]])
data$comorbNumber_ <- dplyr::case_when(
  data$comorbNumber==0 ~ "0",
  data$comorbNumber==1 ~ "1",
  data$comorbNumber==2 ~ "2",
  data$comorbNumber>=3 ~ ">=3",
  TRUE ~ "Error"
)
data$comorbNumber_<- factor(data$comorbNumber_, levels=c("0", "1", "2",">=3"))
data$comorbNone <- data$comorbNumber==0

# Construct a single PRS for BP by averaging the PRS for SBP and DBP
data$PRS <- rowMeans(data[,c("PRS_DBP", "PRS_SBP")])

quintiles <- quantile(data$PRS, probs=seq(0, 1, 0.2), na.rm=TRUE)
data$PRS_quint <- dplyr::case_when(
  data$PRS <= quintiles[2] ~ "Q1: Lowest score",
  data$PRS > quintiles[2] & data$PRS <= quintiles[3] ~ "Q2",
  data$PRS > quintiles[3] & data$PRS <= quintiles[4] ~ "Q3",
  data$PRS > quintiles[4] & data$PRS <= quintiles[5] ~ "Q4",
  data$PRS > quintiles[5] & data$PRS <= quintiles[6] ~ "Q5: Highest score",
  TRUE ~ "Unanswered"
)
data$PRS_quint <- factor(data$PRS_quint, 
                              levels=c("Q3", "Q1: Lowest score", "Q2", "Q4", "Q5: Highest score", "Unanswered"))


#--------------------------------------------------------------------------------------------------------------
# Datasets
#--------------------------------------------------------------------------------------------------------------

# For the SES descriptive analyses, we want to consider two subsets: all hypertensives and all treated individuals
hypertensives <- data[data$evidenceHTN==TRUE & !is.na(data$evidenceHTN),]
treated <- data[data$treated==TRUE & !is.na(data$treated),]

# Among the treated, those who did not list any HTN meds in VI are assumed to be taking unlisted meds
treated$antiHTNmedsno <- factor(treated$antiHTNmedsno, levels=c("1", "2", ">=3", "0"),
                                labels=c("1", "2", ">=3",  "Medication list unavailable"))

# Need Townsend quintiles to be calculated within treated population only
# Convert Townsend deprivation index to quintiles (and a factor)
quintiles <- quantile(treated$townsend_depind, probs=seq(0, 1, 0.2), na.rm=TRUE)
treated$townsend_quint <- dplyr::case_when(
  treated$townsend_depind <= quintiles[2] ~ "Q1: Least deprived",
  treated$townsend_depind > quintiles[2] & treated$townsend_depind <= quintiles[3] ~ "Q2",
  treated$townsend_depind > quintiles[3] & treated$townsend_depind <= quintiles[4] ~ "Q3",
  treated$townsend_depind > quintiles[4] & treated$townsend_depind <= quintiles[5] ~ "Q4",
  treated$townsend_depind > quintiles[5] & treated$townsend_depind <= quintiles[6] ~ "Q5: Most deprived",
  TRUE ~ "Unanswered"
)
treated$townsend_quint <- factor(treated$townsend_quint, 
                              levels=c("Q1: Least deprived", "Q2", "Q3", "Q4", "Q5: Most deprived", "Unanswered"))


saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTN_excl.rds")
saveRDS(treated, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTN_trt.rds")
