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


saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_excl.rds")


data <- readRDS(file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\HTN_excl.rds")
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
data <- data[data$NumberCancers==0 & !is.na(data$NumberCancers),]

excl$cancer <- nrow(data)

# # Exclude individuals with severely limited function (hand grip, reaction time)
# # n = 443782 - 443627 = 155
# data <- data[data$mean_reacttime<1500 | is.na(data$mean_reacttime),]
# 
# excl$limitfunc <- nrow(data)

excl$hypert <- nrow(data[data$evidenceHTN==TRUE,])

# Create exclusion flowchart
export_svg(DiagrammeR::grViz("K:\\TEU\\APOE on Dementia\\Statistical Analysis\\NeoHypertension\\RMarkdown\\ExclusionFlowchart.gv")
           ) %>% charToRaw %>% rsvg %>% png::writePNG("K:\\TEU\\APOE on Dementia\\Statistical Analysis\\NeoHypertension\\RMarkdown\\ExclFlowchart.png")

#--------------------------------------------------------------------------------------------------------------
# Neo's specific variants of covariate variables
#--------------------------------------------------------------------------------------------------------------






# Convert "missing" employment to "unemployed" so it doesn't interfere with Cox regression
levels(data$employment) <- c(levels(data$employment), "Unemployed/retired/other")
data$employment[is.na(data$employment)] <- "Unemployed/retired/other"
data$employment <- factor(data$employment, levels=c("Managers and Senior Officials", "Professional Occupations",
                                                    "Associate Professional and Technical Occupations",
                                                    "Administrative and Secretarial Occupations",
                                                    "Skilled Trades Occupations", "Personal Service Occupations",
                                                    "Sales and Customer Service Occupations", "Process, Plant and Machine Operatives",
                                                    "Elementary Occupations", "Other job (free text entry)", "Unemployed/retired/other"))








# Indicator variable for whether physical activity > or <= 150 METs per day
data$METs_over150 <- dplyr::case_when(
  is.na(data$PhA_METsWkAllAct) ~ "Unknown",
  data$PhA_METsWkAllAct/7 > 150 ~ "Average daily METs > 150",
  TRUE ~ "Average daily METs <= 150")
data$METs_over150 <- factor(data$METs_over150, levels=c("Average daily METs <= 150", "Average daily METs > 150", "Unknown"))

# Convert bowel cancer screening to a factor
data$BowelCancerScreening <- as.character(data$BowelCancerScreening)
data$BowelCancerScreening[data$BowelCancerScreening %in% c("Prefer not to answer", "Do not know") | is.na(data$BowelCancerScreening)] <- "Unanswered"
data$BowelCancerScreening <- factor(data$BowelCancerScreening, levels=c("No", "Yes", "Unanswered"), 
                                    labels=c("Not screened", "Screened for bowel cancer", "Unanswered"), ordered=FALSE)

# Convert family history to a factor
data$FamilyHist_CVD_ <- factor(as.numeric(data$FaH_CVD), levels=c(0,1), labels=c("No family history of CVD", "Family history of CVD"))

# Convert income level of birth country to a factor
data$BirthCountryIncomeLevel[data$BirthCountryIncomeLevel %in% c("LM", "UM")] <- "M"
data$BirthCountryIncomeLevel <- factor(data$BirthCountryIncomeLevel, levels=c("HUK", "H", "M", "L"), 
                                       labels=c("UK", "Other high income", "Middle income", "Low income"))

# Add hypertension severity indicator
data$HTNdx_severity <- dplyr::case_when(
  data$SBP>=180 | data$DBP>=110 ~ "Stage 3",
  between(data$SBP, 160, 180) | between(data$DBP, 100, 110) ~ "Stage 2",
  between(data$SBP, 140, 160) | between(data$DBP, 90, 100) ~ "Stage 1",
  TRUE ~ "Normotensive"
)
data$HTNdx_severity <- factor(data$HTNdx_severity, levels=c("Normotensive", "Stage 1", "Stage 2", "Stage 3"))

# HTN duration categories
data$HTNdx_durcat <- as.character(cut(data$HTNdx_duration, breaks=c(0, 1, 2, 5, 10, 20, 100), right=FALSE))
data$HTNdx_durcat[is.na(data$HTNdx_durcat)] <- "Unanswered"
data$HTNdx_durcat <- factor(data$HTNdx_durcat, levels=c("[0,1)", "[1,2)", "[2,5)", "[5,10)", "[10,20)", "[20,100)", "Unanswered"),
                            labels=c("Less than 1 year", "1 to 2 years", "2 to 5 years", 
                                     "5 to 10 years", "10 to 20 years", "More than 20 years", "Unanswered")
                            )

# Indicator variable for missing hypertension duration
# We set HTN duration to 0 (or mean, or anything) when it's missing, and interact it with the missingness indicator in the regression
data$HTNdx_durind <- as.numeric(is.na(data$HTNdx_duration))
# data$HTNdx_duration[is.na(data$HTNdx_duration)] <- 0


# # Add mapping to comorbidities of interest
# comorbidities <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VI_ComorbidityCategories.rds")
# data <- merge(data, comorbidities, by="ID", all.x=TRUE)

# Add mapping to comorbidity groups B and C
comorbgrps <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypGroupBC.rds")
data <- merge(data, comorbgrps, by="ID", all.x=TRUE)
for(comorb in names(comorbgrps)[-1]){
  data[[paste0(comorb,"_")]] <- data[[comorb]]>0 & !is.na(data[[comorb]])
  data[[paste0(comorb,"_")]] <- factor(as.numeric(data[[paste0(comorb,"_")]]), levels=c(0,1), labels=c("No", "Yes"))
}

comorbtype <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypGroupBC_type.rds")
data <- merge(data, comorbtype, all.x=TRUE)
for(col in names(comorbtype)[-1]){
  data[[col]] <- data[[col]]>0 & !is.na(data[[col]])
  data[[col]] <- factor(as.numeric(data[[col]]), levels=c(0,1), labels=c("None", gsub("_", " ", col)))
}

saveRDS(data, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\HTN_excl.rds")
