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
library(yaml)

#--------------------------------------------------------------------------------------------------------------

config = yaml.load_file("config.yml")
source(config$functions)
source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))
old_data <- readRDS(file.path(config$data$derived, "HTN_raw.rds"))

# Exclusions

exclusions <- function(data){
  
  # Exclude individuals who have withdrawn from the study
  withdrawn <- read.csv(config$cleaning$withdrawals, header=FALSE)
  data <- data[!data$ID %in% withdrawn$V1,]
  
  # Globally assign a list to track exclusion counts
  excl <<- list(initial=nrow(data))
  
  # Exclude those outside the 40-70 age range
  data <- data[data$TEU_BaC_AgeAtRec >= 40 & data$TEU_BaC_AgeAtRec < 70,]

  excl$agerange <<- nrow(data)

  # Exclude individuals with missing BP data
  # or missing answers to BP questions on touchscreen questionnaire
  data <- data[!is.na(data$TEU_BlP_SBP.avg),]
  data <- data[!is.na(data$TEU_BlP_DBP.avg),]
  # data <- data[!is.na(data$selfrephyp),]
  # data <- data[!is.na(data$selfrepmeds),]
  # Exclude participants who only had BP measured once
  data <- data[data$TEU_BlP_nSBP == 2 & data$TEU_BlP_nDBP == 2,]

  excl$BPmiss <<- nrow(data)

  # Exclude individuals with implausible BP data
  data <- data[data$TEU_BlP_SBP.avg >= 70 & data$TEU_BlP_SBP.avg <= 270,]
  data <- data[data$TEU_BlP_DBP.avg >= 50 & data$TEU_BlP_DBP.avg <= 150,]

  excl$BPimp <<- nrow(data)

  # Exclude pregnant women ("Yes" or "Unsure")
  data <- data[is.na(data$VeI_PregnantNow) | data$VeI_PregnantNow == "No",]

  excl$pregnant <<- nrow(data)

  # Exclude individuals who have serious health conditions
  seriouscomorbid <- readRDS(file.path(config$data$derived, "VIhypExclude.rds"))
  data <- data[!data$ID %in% seriouscomorbid$ID[!is.na(seriouscomorbid$Yes)],]

  excl$seriouscomorb <<- nrow(data)

  # And individuals with cancer (except for skin cancer?)
  cancer <- readRDS(file.path(config$data$derived, "Cancer_pts.rds"))
  exceptskincancer <- cancer$ID[cancer$TL!="skin cancer"]
  data <- data[!data$ID %in% exceptskincancer,]

  excl$cancer <<- nrow(data)
  
  
  # Any durations less than 0 are clearly errors (only 2)
  # Any diagnoses before the age of about 20 are probably due to other causes
  data$HTNdx_duration[(data$HTNdx_duration < 0 | data$HTNdx_duration > data$age-20)& !is.na(data$HTNdx_duration)] <- NA
  
  # Hypertension care cascade
  # excl$hypert <- nrow(data[data$evidenceHTN==TRUE,])
  # excl$aware <- nrow(data[data$aware==TRUE & !is.na(data$aware),])
  # excl$treat <- nrow(data[data$treated==TRUE & !is.na(data$treated),])
  return(data)
}

data <- evalWithMemoization(derive_variables(database = config$data$database, 
                                             field_definitions = TEU_SPECS$HTN_control,
                                             exclusions = exclusions),
                            key = TEU_SPECS$HTN_control)

#--------------------------------------------------------------------------------------------------------------
# Create some more complex variables
#--------------------------------------------------------------------------------------------------------------
Neo_HTN_all <- function(data){
  # Duration of hypertension diagnosis
  data$HTNdx_TQ <- round(data$age - data$HBPAge,1)
  data$HTNdx_VI <- round(decimal_date(data$recdate) - data$VIhypdx_yr,1)
  data$HTNdx_duration <- coalesce(data$HTNdx_VI, data$HTNdx_TQ)
  # Any durations less than 0 are clearly errors (only 2)
  # Any diagnoses before the age of about 20 are probably due to other causes
  data$HTNdx_duration[(data$HTNdx_duration < 0 | data$HTNdx_duration > data$age-20)& !is.na(data$HTNdx_duration)] <- NA
  
  
  # Single variable for self-reported meds
  # If participant answered the question in touchscreen questionnaire, use this
  # If they did not answer ("Do not know"/Prefer not to answer"/skipped question) then use the "probable BP meds"
  data$HTN_probablemeds[is.na(data$HTN_probablemeds)] <- FALSE
  data$selfrepmeds <- (data$HBPmeds==TRUE & !is.na(data$HBPmeds)) | data$HTN_probablemeds
  data$selfrepmeds[is.na(data$HBPmeds) & is.na(data$NumberMedications)] <-NA

  
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
  data$evidenceHTN_ <- factor(as.numeric(data$evidenceHTN), levels=c(0,1), labels=c("No evidence of hypertension", "Hypertensive"))
  
  
  #--------------------------------------------------------------------------------------------------------------
  # Create exclusion flowchart
  #--------------------------------------------------------------------------------------------------------------
  
  # Incorporate HTN prevalence tree into flowchart
  
  nlist <<- list(wholepop=nrow(data))
  pctlist <<- list()
  cilist <<- list()
  for(variable in c("evidenceHTN", "aware", "treated", "controlled")){
    var <- paste0(variable, "_")
    tab <- table(data[[var]])
    pct <- prop.table(tab)
    z <- qnorm(1-(0.05/2))
    sd <- sqrt(pct[1]*pct[2]/sum(tab))
    uci <- pct + z*sd
    lci <- pct - z*sd
    nlist[[variable]] <- tab
    pctlist[[variable]] <- pretty_dp(100*pct,1)
    cilist[[variable]] <- paste0(pretty_dp(100*lci,1), ", ", pretty_dp(100*uci,1))
  }
  
  # Create exclusion flowchart
  export_svg(DiagrammeR::grViz(file.path(config$outputs$flowcharts, "ExclusionFlowchartTree.gv"))
             ) %>% charToRaw %>% rsvg %>% png::writePNG(file.path(config$outputs$figures, "ExclFlowchartTree.png"))
  
  
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
    data$TEU_EmpCat %in% c("Managers and Senior Officials", "Professional Occupations",
                           "Associate Professional and Technical Occupations",
                           "Administrative and Secretarial Occupations") ~ "White collar",
    data$TEU_EmpCat == "Skilled Trades Occupations" ~ "Skilled trades",
    data$TEU_EmpCat %in% c("Personal Service Occupations",
                           "Sales and Customer Service Occupations") ~ "Services",
    data$TEU_EmpCat %in% c("Process, Plant and Machine Operatives",
                           "Elementary Occupations") ~ "Blue collar",
    data$TEU_EmpCat %in% c("Other job (free text entry)",
                           "In paid employment or self-employed") ~ "Other employment",
    data$TEU_EmpCat == "Retired" ~ "Retired",
    data$TEU_EmpCat == "Unable to work because of sickness or disability" ~ "Disability",
    data$TEU_EmpCat %in% c("Looking after home and/or family",
                           "Unemployed", "Full or part-time student",
                           "Doing unpaid or voluntary work",
                           "None of the above", "Prefer not to answer") ~ "Unemployed/unanswered",
    TRUE ~ "Error?"
  )
  data$employcat <- factor(data$employcat, 
                           levels=c("White collar", "Skilled trades", "Services", 
                                    "Blue collar", "Other employment", "Retired", 
                                    "Disability", "Unemployed/unanswered"), 
                           labels=c("Professional and Administrative", "Skilled trades", "Services", 
                                    "Manual and Industrial", "Other employment", "Retired", 
                                    "Unable to work because of sickness or disability", 
                                    "Unemployed/unanswered"),
                           ordered=FALSE)
  
  data$FamilyHist_HeartDisease_ <- factor(as.numeric(data$FaH_HeartDisease), levels=c(0,1), labels=c("No", "Yes"))
  data$FamilyHist_Stroke_ <- factor(as.numeric(data$FaH_Stroke), levels=c(0,1), labels=c("No", "Yes"))
  data$FamilyHist_Hypertension_ <- factor(as.numeric(data$FaH_Hypertension), levels=c(0,1), labels=c("No", "Yes"))
  
  data$AdoptHist_CVD_ <- factor(as.numeric(data$FaH_Ad_CVD), levels=c(0,1), labels=c("No", "Yes"))
  
  data$AdoptHist_HeartDisease_ <- factor(as.numeric(data$FaH_Ad_HeartDisease), levels=c(0,1), labels=c("No", "Yes"))
  data$AdoptHist_Stroke_ <- factor(as.numeric(data$FaH_Ad_Stroke), levels=c(0,1), labels=c("No", "Yes"))
  data$AdoptHist_Hypertension_ <- factor(as.numeric(data$FaH_Ad_Hypertension), levels=c(0,1), labels=c("No", "Yes"))
  
  # Convert income level of birth country to a factor
  data$BirthCountryIncomeLevel[data$BirthCountryIncomeLevel %in% c("LM", "UM")] <- "M"
  data$BirthCountryIncomeLevel <- factor(data$BirthCountryIncomeLevel, levels=c("HUK", "H", "M", "L"), 
                                         labels=c("UK", "Other high income", "Middle income", "Low income"))
  

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
  comorbs <- readRDS(file.path(config$data$derived, "VI_HTNcomorb.rds"))
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

  return(data)
}

Neo_HTN_trt <- function(data){

  #--------------------------------------------------------------------------------------------------------------
  # Datasets
  #--------------------------------------------------------------------------------------------------------------
  
  # For the SES descriptive analyses, we want to consider two subsets: all hypertensives and all treated individuals
  hypertensives <- data[data$evidenceHTN==TRUE & !is.na(data$evidenceHTN),]
  treated <- data[data$treated==TRUE & !is.na(data$treated),]
  
  # Among the treated, those who did not list any HTN meds in VI are assumed to be taking unlisted meds
  treated$antiHTNmedsno <- factor(treated$antiHTNmedsno, levels=c("1", "2", ">=3", "0"),
                                  labels=c("1", "2", ">=3",  "Medication list unavailable"))
  
  return(treated)
}
