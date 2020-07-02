# Jennifer Collister
# Code to combine the medication classifications with the actual data and categorise individuals by whether we think they are
# definitely, probably or not taking medication for their BP
library(yaml)

config = yaml.load_file("K:/TEU/APOE on Dementia/config.yml")

altdiag <- readRDS(file=paste0(config$exclusions$htn, "VIhypAltDiagnoses.rds"))
altdiag[altdiag==0] <- NA
load(paste0(config$cleaning$organised, "VIhypmeds.RData"))
bac <- readRDS(paste0(config$cleaning$organised, "basechar.rds"))
eth <- readRDS(paste0(config$cleaning$organised, "ethnicity.rds"))

# Keep all individuals with medication data, we may not have diagnosis data for some of them
VImeds <- merge(VImeds, altdiag[,-which(names(altdiag)=="other")], by="ID", all.x=TRUE)
VImeds <- merge(VImeds, bac[,c("ID", "gender", "age")], by="ID", all.x=TRUE)
VImeds <- merge(VImeds, eth[,c("ID", "eth_group")], by="ID", all.x=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Define the groups of drugs
#--------------------------------------------------------------------------------------------------------------
# Line 1 drugs: Ace Inhibitors (ACEI), Angiotensin II receptor blockers (ARBs) and Calcium Channel Blockers (CCB)
VImeds$line1 <- rowSums(VImeds[,c("ACEI", "ARBs", "CCB")], na.rm=TRUE)>0
# Line 2 drugs: Beta blocker (BB), Alpha blocker (AB) or Spironolactone
VImeds$line2 <- rowSums(VImeds[,c("AB", "BB", "Potassium-sparing diuretic")], na.rm=TRUE)>0
# Only taking one medication - a flag to speed stuff up
VImeds$onlyone <- rowSums(VImeds[,hypclasslist], na.rm=TRUE)==1

#--------------------------------------------------------------------------------------------------------------
# Standard treatment algorithms according to NICE guidelines
#--------------------------------------------------------------------------------------------------------------
# Step 1/2: Taking one or more of ACEI, ARB or CCB WITHOUT thiazide-like diuretic or BB
VImeds$step12 <- VImeds$line1 & is.na(VImeds[["Thiazide"]]) & !VImeds$line2 & is.na(VImeds[["Other"]])
# Step 3: Taking a step 1/2 drug AND a thiazide-like diuretic
VImeds$step3 <- VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & !VImeds$line2 & is.na(VImeds[["Other"]])
# Step 4: Taking a step 3 combination of drugs AND AB, BB or other diuretic
# AND does not have diagnosis of heart failure
VImeds$step4 <- VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & VImeds$line2 & is.na(VImeds[["Other"]]) & 
  is.na(VImeds$heart_failure)
# Step 5 'resistant hypertension': Taking at least 3 meds from steps 1-4
# AND any of hydralazine, minoxidil, clonidine
# AND does not have diagnosis of heart failure
VImeds$step5 <- VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & VImeds$line2 & !is.na(VImeds[["Other"]]) & 
  is.na(VImeds$heart_failure)


#--------------------------------------------------------------------------------------------------------------
# Standard exceptions to treatment algorithms
#--------------------------------------------------------------------------------------------------------------
# Thiazides only: atypical step 1 if Black OR age < 55
VImeds$step1_b55 <- !is.na(VImeds[["Thiazide"]]) & VImeds$onlyone & (VImeds$eth_group=="Black" | VImeds$age<55)
# BB only: atypical step 1 if female age <45 (childbearing age) AND does not have diagnosis of heart failure
VImeds$step1_f45 <- !is.na(VImeds[["BB"]]) & VImeds$onlyone & VImeds$gender=="Female" & VImeds$age<45 & 
  is.na(VImeds$heart_arrhythmia) & is.na(VImeds$heart_failure) & is.na(VImeds$angina)


#--------------------------------------------------------------------------------------------------------------
# Atypical treatment algorithm AND no alternative diagnosis
#--------------------------------------------------------------------------------------------------------------
# other diuretic AND does not have heart failure

# Thiazide only AND does not have:
# liver failure, kidney failure, pulmonary oedema, lymphoedema
VImeds$thiaonly <- !is.na(VImeds[["Thiazide"]]) & VImeds$onlyone & 
  is.na(VImeds$liver_failure) & is.na(VImeds$kidney_failure) & is.na(VImeds$heart_failure) & is.na(VImeds$lymphoedema)
# ACEI/ARB AND does not have any of the following:
# diabetes, heart failure, prior heart attack
VImeds$ACEARBonly <- (!is.na(VImeds[["ACEI"]]) | !is.na(VImeds[["ARBs"]])) & VImeds$onlyone & 
  is.na(VImeds$diabetes) & is.na(VImeds$heart_failure) & is.na(VImeds$heart_attack)
# BB AND does not have any of the following:
# heart arrhythmia, heart failure, angina
VImeds$BBonly <- !is.na(VImeds[["BB"]]) & VImeds$onlyone & 
  is.na(VImeds$heart_arrhythmia) & is.na(VImeds$heart_failure) & is.na(VImeds$angina)
# VImeds$weird1 <- VImeds$line1 & is.na(VImeds[["Thiazide"]]) & VImeds$line2 & is.na(VImeds[["Other"]])
# VImeds$weird2 <- !VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & VImeds$line2 & is.na(VImeds[["Other"]])

#--------------------------------------------------------------------------------------------------------------
# If-else to create a single factor variable with all the categories
#--------------------------------------------------------------------------------------------------------------
VImeds$HTN_txalg <- dplyr::case_when(
  VImeds$step12==TRUE ~ "Step 1/2",
  VImeds$step3==TRUE ~ "Step 3",
  VImeds$step4==TRUE ~ "Step 4",
  VImeds$step5==TRUE ~ "Step 5",
  VImeds$step1_b55==TRUE ~ "Step 1 for Black OR age<55",
  VImeds$step1_f45==TRUE ~ "Step 1 for female of childbearing age",
  VImeds$thiaonly==TRUE ~ "Thiazide only, no alternative diagnosis",
  VImeds$ACEARBonly==TRUE ~ "ACEI/ARB only, no alternative diagnosis",
  VImeds$BBonly==TRUE ~ "BB only, no alternative diagnosis",
  TRUE ~ VImeds$hypmeds
  )


#--------------------------------------------------------------------------------------------------------------
# Definitely not on medication for hypertension
#--------------------------------------------------------------------------------------------------------------
## Heart, liver and kidney failure are excluded so this doesn't change resulting dataset
# Has heart failure AND is on ACEI/ARB and BB without thiazide
VImeds$defnot[!is.na(VImeds$heart_failure) &
                (!is.na(VImeds[["ACEI"]]) | !is.na(VImeds[["ARBs"]])) & !is.na(VImeds[["BB"]]) &
                is.na(VImeds[["Thiazide"]])
              ] <- TRUE
# Has liver failure/lymphoedema AND is on a (non-thiazide) diuretic
VImeds$defnot[(!is.na(VImeds$liver_failure) | !is.na(VImeds$lymphoedema)) &
                !is.na(VImeds[["Potassium-sparing diuretic"]]) & VImeds$onlyone
              ] <- TRUE
# Has kidney failure AND is on a (non-thiazide) diuretic +/- ACEI/ARB
VImeds$defnot[!is.na(VImeds$kidney_failure) &
                !is.na(VImeds[["Potassium-sparing diuretic"]]) &
                VImeds$onlyone
              ] <- TRUE
VImeds$defnot[!is.na(VImeds$kidney_failure) &
                !is.na(VImeds[["Potassium-sparing diuretic"]]) & (!is.na(VImeds[["ACEI"]]) | !is.na(VImeds[["ARBs"]])) &
                rowSums(VImeds[,hypclasslist], na.rm=TRUE)==2
              ] <- TRUE
# These are relevant
# Has arrhythmia AND is on BB only
VImeds$defnot[!is.na(VImeds$heart_arrhythmia) &
                !is.na(VImeds[["BB"]]) & VImeds$onlyone
              ] <- TRUE
# Has angina AND is on BB
VImeds$defnot[!is.na(VImeds$angina) &
                !is.na(VImeds[["BB"]]) & VImeds$onlyone
              ] <- TRUE

# Anything else is not definitely not
VImeds$defnot[is.na(VImeds$defnot)] <- FALSE

#
VImeds$HTN_txalg[VImeds$defnot==TRUE] <- "Taking potential BP medication for other indications"


#--------------------------------------------------------------------------------------------------------------
# Once the potential BP medication has been categorised, any remaining are non-standard combinations
#--------------------------------------------------------------------------------------------------------------
VImeds$HTN_txalg[VImeds$HTN_txalg=="Taking potential BP medication"] <- "Probably not BP medication"
# Make the groups a factor, with appropriate ordering of levels
VImeds$HTN_txalg <- factor(VImeds$HTN_txalg, levels=c("Step 1/2", "Step 3", "Step 4", "Step 5",
                                                      "Step 1 for Black OR age<55", "Step 1 for female of childbearing age",
                                                      "Thiazide only, no alternative diagnosis",
                                                      "ACEI/ARB only, no alternative diagnosis",
                                                      "BB only, no alternative diagnosis",
                                                      "Taking potential BP medication for other indications",
                                                      "Probably not BP medication",
                                                      "Taking non-BP medication"))

table(VImeds$HTN_txalg, useNA='ifany')


VImeds$HTN_probablemeds[
  VImeds$HTN_txalg %in% c("Step 1/2", "Step 3", "Step 4", "Step 5",
                            "Step 1 for Black OR age<55", "Step 1 for female of childbearing age",
                            "Thiazide only, no alternative diagnosis",
                            "ACEI/ARB only, no alternative diagnosis",
                            "BB only, no alternative diagnosis")
  ] <- TRUE
VImeds$HTN_probablemeds[is.na(VImeds$HTN_probablemeds)] <- FALSE
table(VImeds$HTN_probablemeds, useNA='ifany')



# saveRDS(medsdata, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\ClassifiedHypMedGroups.rds")
saveRDS(VImeds[,c("ID", hypdruglist, hypclasslist, "hypmedsno", "hypmeds", "HTN_txalg", "HTN_probablemeds")], 
        file=paste0(config$exclusions$htn, "HTNMedsRubric.rds"))
