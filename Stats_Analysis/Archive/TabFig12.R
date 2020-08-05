#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/01/2020
# Get data into format for Tables 1 and 2 and Figures 1 and 2
# K:\TEU\APOE on Dementia\Statistical Analysis\NeoHypertension\UKBTapela_HTN_AnalysisConcept_v0.2_NT_20191217.docx
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
# Load and merge the data
#--------------------------------------------------------------------------------------------------------------

bp <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\bp.rds")
ethnicity <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\ethnicity.rds")
basechar <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\basechar.rds")
medhist <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\tq_medhist.rds")
deathdate <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\deathdate.rds")
covars <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\covars.rds")
veint <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\veint_medhist.rds")


# Combine the APOE genetic data with the baseline characteristics
bp <- merge(
  merge(
    merge(
      bp, basechar, by="ID", all=TRUE
    ), ethnicity, by="ID", all=TRUE
  ), deathdate, by="ID", all=TRUE
)
dupes <- bp[duplicated(bp$ID),]
dupedata <- bp[bp$ID %in% (bp$ID),]
# And with all other selected covariates
bp <- merge(bp, covars, by="ID", all=TRUE)
# And with verbal interview data
bp <- merge(bp, veint, by="ID", all=TRUE)
# And with medical history from touchscreen questionnaire
bp <- merge(bp, medhist, by="ID", all=TRUE)

saveRDS(bp, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\neohyp_raw.rds")


#--------------------------------------------------------------------------------------------------------------
# Exclusions and Figure 1
#--------------------------------------------------------------------------------------------------------------

bp <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\neohyp_raw.rds")
nrow(bp)

# Individuals eligible for inclusion in the analysis will be those who completed the self-administered touch-screen questionnaire
# and nurse-administered interviews, and have non-missing values for age, gender, highest level of education, 
# whether have been diagnosed with a medical condition, and Blood pressure (BP) measurement. 

# Exclude individuals outside the age range
bp <- bp[bp$age >= 40 & bp$age < 70,]
nrow(bp)


# Exclude individuals with incomplete blood pressure data
bp <- bp[!is.na(bp$SBP) & !is.na(bp$DBP),]
nrow(bp)
# And those who didn't answer what medications they were taking
bp <- bp[!is.na(bp$medication) & !bp$medication %in% c("Do not know", "Prefer not to answer"),]
nrow(bp)
# And those who didn't give medical history
bp <- bp[!is.na(bp$vasc_cond) & bp$vasc_cond!="Prefer not to answer" 
         & !is.na(bp$diabetes) & bp$diabetes!="Prefer not to answer" ,]
nrow(bp)


# Exclude individuals with missing education level
bp <- bp[bp$eduNA==FALSE,]
nrow(bp)
# There are no individuals missing age or gender
nrow(bp[is.na(bp$age),])
nrow(bp[is.na(bp$gender),])

# Include only individuals who completed touchscreen questionnaire and verbal interview

saveRDS(bp, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\neohyp_excl.rds")


bp <- readRDS(file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\neohyp_excl.rds")

#--------------------------------------------------------------------------------------------------------------
# Hypertension will be defined as self-report of being on anti-hypertensives in the previous two weeks, 
# and/or having elevated BP (mean systolic BP>=140 mmHg or mean diastolic BP>=90 mmHg) during survey measurement. 
# Three further binary hypertension status outcomes were of interest in this analysis: awareness, treatment, control. 
# Awareness will be defined as report of ever being previously informed of a hypertension diagnosis by a health professional. 
# Treatment is report of being on anti-hypertensives in the previous two weeks. 
# Control is having measured mean systolic BP<140 mmHg and mean DBP<90 mmHg. 
# These outcomes produce mutually exclusive subgroups of uncontrolled hypertensives previously described in other studies10-13: 
# those who were Unaware, Untreated, or  Sub-optimally treated (Figure 2). 
#--------------------------------------------------------------------------------------------------------------

# Indicator variables for hypertension prevalence, awareness, treatment and control
bp$hypertension <- (bp$HBPmeds==TRUE | bp$hyp==TRUE | bp$prevHBP==TRUE | bp$VIhyp==TRUE)
bp$aware <- (bp$hypertension==TRUE & (bp$prevHBP==TRUE | bp$VIhyp==TRUE | bp$HBPmeds==TRUE))
bp$aware[bp$hypertension==FALSE] <- NA
bp$trt <- (bp$aware==TRUE & bp$HBPmeds==TRUE)
bp$trt[bp$aware==FALSE | is.na(bp$aware)] <- NA
bp$control <- (bp$trt==TRUE & bp$hyp==FALSE)
bp$control[bp$trt==FALSE | is.na(bp$trt)] <- NA

#--------------------------------------------------------------------------------------------------------------
# Hypertension severity will be categorized into three stages following thresholds from the Seventh Report of the 
# Joint National Committee on Prevention, Detection, Evaluation, and Treatment of High Blood Pressure14: 
# Stage I corresponds to systolic BP 140-159 mmHg or diastolic BP 90-99 mmHg, 
# stage II mean systolic BP 160-179 mmHg or diastolic BP 100-110 mmHg, 
# and stage III mean systolic BP>180 mmHg or diastolic BP>110 mmHg (also referred to as hypertensive crisis). 
#--------------------------------------------------------------------------------------------------------------

# Factor variable indicating hypertension stage
bp$severity <- ifelse(between(bp$SBP, 140, 159) | between(bp$DBP, 90, 99), "Stage I",
                      ifelse(between(bp$SBP, 160, 179) | between(bp$DBP, 100,109), "Stage II",
                             ifelse(bp$SBP>=180 | bp$DBP >= 110, "Stage III", NA)))

#--------------------------------------------------------------------------------------------------------------
# Figure 2
#--------------------------------------------------------------------------------------------------------------

table(bp$hypertension, useNA='ifany')
table(bp$aware, useNA='ifany')
table(bp$trt, useNA='ifany')
table(bp$control, useNA='ifany')

#--------------------------------------------------------------------------------------------------------------
# Table 1
#--------------------------------------------------------------------------------------------------------------
# Characteristics of participants by gender
# Include missing values
# Include explanatory variables (ethnicity, morbidity) and all covariates:
# Sociodemographic: age, gender, education, country of birth, household income (continuous/quintiles),
# Townsend index, employment shift type, occupation type
# Lifestyle: tobacco, alcohol, physical activity, sleep duration
# Function: raction time, handgrip strength
# Hypertension disease characteristics: duration, severity, history of stroke, history of ischemic coronary disease, family history of hypertension
# Physical measurements: BMI, waist circumference, BP1/2
# Baseline blood chemistry tests: glucose, total cholesterol, HDL cholesterol, LDL direct, triglycerides


#--------------------------------------------------------------------------------------------------------------
# Table 2
#--------------------------------------------------------------------------------------------------------------

# Hypertension prevalence among individuls by gender, age-group, education, ethnicity, number of co-morbidities








