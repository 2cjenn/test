
FN_famhist <- function(conditions, label){
  function(data){
    y <- apply(data[,c(grep("FaH_FatherIll.", colnames(data), fixed=TRUE),
                       grep("FaH_MotherIll.", colnames(data), fixed=TRUE),
                       grep("FaH_SibIll.", colnames(data), fixed=TRUE)
                       )
                    ], 1, function(x) any(x %in% conditions)
               )
    y <- factor(as.numeric(y), levels=c(0,1), 
                labels=c(paste0("No family history of ", label), paste0("Family history of ", label)))
    return(y)
  }
}

FN_HMHmeds <- function(medtype, string){
  function(data){
    # Combine the first medication field across males and females
    medcombine <- coalesce(data[["HMH_MedCholBPDiabHorm.0"]], data[["HMH_MedCholBPDiab.0"]])
    # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
    medlist <- c("Cholesterol lowering medication", "Blood pressure medication", "Oral contraceptive pill or minipill", "Hormone replacement therapy", "Insulin")
    x <- dplyr::case_when(
      medcombine=="None of the above" ~ "No",
      medcombine=="Do not know" ~ "Do not know",
      medcombine=="Prefer not to answer" ~ "Prefer not to answer",
      is.na(medcombine) ~ "NA",
      medcombine %in% medlist ~ "Yes",
      TRUE ~ "Unexpected answer"
    )
    x <- factor(x, levels=c("Yes", "No", "Do not know", "Prefer not to answer", "NA"))
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested medication across the columns
    y <- apply(data[,c(grep("HMH_MedCholBPDiab.", colnames(data), fixed=TRUE),
                       grep("HMH_MedCholBPDiabHorm.", colnames(data), fixed=TRUE))], 1, function(x) any(x==medtype))
    # And incorporate the info on whether this participant is taking any other medication
    y[x %in% c("Prefer not to answer", "Do not know", "NA")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report", string), "Unanswered"))
    return(y)
  }
}

FN_MissingCategory <- function(missingvals, categ_name){
  function(x){
    # Categorise missing data - change levels so Prefer not to answer and NA are both "Unanswered"
    labels <- c(levels(x)[-which(levels(x) %in% missingvals)], categ_name)
    y <- as.character(x)
    y[y %in% missingvals] <- categ_name
    y[is.na(y)] <- categ_name
    y <- factor(y, levels=labels, ordered=FALSE)
    return(y)
  }
}

agecat <- list(
  name="TEU_BaC_AgeCat", 
  source="TEU_BaC_AgeAtRec", 
  mapper=FN_buckets(breaks=c(40, 50, 60, 70), right=FALSE), 
  display_name="AgeCategory", 
  description="Categorised age in years"
)

HMHmeds.any <- list(
  name="TEU_HMH_Med.any", 
  source=c(paste0("HMH_MedCholBPDiabHorm.", c(0:3)), paste0("HMH_MedCholBPDiab.", c(0:2))), 
  mapper=function(data){
    # Combine the first medication field across males and females
    medcombine <- coalesce(data[["HMH_MedCholBPDiabHorm.0"]], data[["HMH_MedCholBPDiab.0"]])
    # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
    medlist <- c("Cholesterol lowering medication", "Blood pressure medication", "Oral contraceptive pill or minipill", "Hormone replacement therapy", "Insulin")
    y <- dplyr::case_when(
      medcombine=="None of the above" ~ "No",
      medcombine=="Do not know" ~ "Do not know",
      medcombine=="Prefer not to answer" ~ "Prefer not to answer",
      is.na(med.combine) ~ "NA",
      medcombine %in% medlist ~ "Yes",
      TRUE ~ "Unexpected answer"
    )
    y <- factor(y, levels=c("Yes", "No", "Do not know", "Prefer not to answer", "NA"))
    return(y)
  }, 
  display_name="HMH_Med", 
  description="Did the participant self-report taking medication for cholesterol, blood pressure, diabetes or HRT?"
)


HMHmeds.BP <- list(
  name="TEU_HMH_Med.BP", 
  source=c(paste0("HMH_MedCholBPDiabHorm.", c(0:3)), paste0("HMH_MedCholBPDiab.", c(0:2))), 
  mapping=FN_HMHmeds(medtype="Blood pressure medication", string="BP meds"), 
  display_name="HBPmeds", 
  description="Participant self-reported taking BP medication in the touchscreen questionnaire"
)


Neoethnicity <- list(name="Eth_ethnicgrp",
                     source="Eth_ethnicity",
                     mapper=function(x){
                       ifelse(x %in% c("White", "British", "Irish", "Any other white background"), 
                              "White",
                              ifelse(x %in% c("Do not know", "Prefer not to answer") | is.na(x), 
                                     "Unknown", 
                                     "Non-white"
                              )
                       )
                     },
                     display_name="ethnic_group",
                     description="The participant's self-reported ethnicity"
)


ethnicity$ethnicity <- as.character(ethnicity$eth_group)
ethnicity$ethnicity <- ifelse(ethnicity$ethnicity=="White", "White", 
                              ifelse(ethnicity$ethnicity=="Prefer not to answer" | is.na(ethnicity$ethnicity), "Unknown",
                                     "Non-white"))
ethnicity$ethnicity <- factor(ethnicity$ethnicity, levels=c("White", "Non-white", "Unknown"))


alcohol <- list(
  name="TEU_Alc_Status", 
  source="Alc_Status", 
  mapper=FN_MissingCategory(missingvals=c("Prefer not to answer"), categ_name="Unanswered"), 
  display_name="AlcoholStatus", 
  description="Self-reported alcohol drinking status"
)

smoking <- list(
  name="TEU_Smo_Status", 
  source="Smo_Status", 
  mapper=FN_MissingCategory(missingvals=c("Prefer not to answer"), categ_name="Unanswered"), 
  display_name="SmokingStatus", 
  description="Self-reported smoking status"
)


weeklyalcohol <- list(
  name="TEU_Alc_WeeklyAlcUnits", 
  source=c("Alc_RedWineWk", "Alc_WhiteWineWk", "Alc_BeerCiderWk", "Alc_SpiritsWk", "Alc_FortWineWk", "Alc_OtherAlcWk"), 
  mapper=function(data){
    alcservings <- list()
    for(alc in c("Alc_RedWineWk", "Alc_WhiteWineWk", "Alc_BeerCiderWk", 
                 "Alc_SpiritsWk", "Alc_FortWineWk", "Alc_OtherAlcWk")){
      alcservings[[alc]] <- ifelse(data[[alc]]<0|is.na(data[[alc]]), 0, data[[alc]])
    }
    
    # Note - need to add scaling here as not sure servings of each type of alcohol translate to units
    weeklyunits <- alcservings[["Alc_RedWineWk"]] + alcservings[["Alc_WhiteWineWk"]] +
                    alcservings[["Alc_BeerCiderWk"]] + alcservings[["Alc_SpiritsWk"]] +
                    alcservings[["Alc_FortWineWk"]] + alcservings[["Alc_OtherAlcWk"]]
    
    # Truncate alcohol consumption at upper 95th percentile
    upper95 <- quantile(weeklyunits, 0.95, na.rm=TRUE)
    weeklyunits[weeklyunits>upper95] <- upper95
    weeklyunits[is.na(weeklyunits)] <- 0
    
    return(weeklyunits)
  }, 
  display_name="WeeklyAlcUnits", 
  description="Total weekly units of alcohol, derived from self-reported average weekly consumption of each different type alcohol"
)

bingealcohol <- list(
  name="TEU_Alc_Binge", 
  source=c("TEU_Alc_WeeklyAlcUnits", "BaC_Sex"), 
  mapper=function(data){
    y <- dplyr::case_when(
      data[["BaC_Sex"]]=="Female" & data[["TEU_Alc_WeeklyAlcUnits"]] > 7 ~ TRUE,
      data[["BaC_Sex"]]=="Male" & data[["TEU_Alc_WeeklyAlcUnits"]] > 14 ~ TRUE,
      TRUE ~ FALSE)
  }, 
  display_name="HarmfulAlcohol", 
  description="Does the patient's self-reported weekly alcohol consumption exceed the threshold for binge drinking"
)

METsover150 <- list(
  name="TEU_Pha_METsover150", 
  source="PhA_METsWkAllAct", 
  mapper=function(x){
    y <- dplyr::case_when(
      is.na(x) ~ "Unknown",
      x/7 > 150 ~ "Average daily METs > 150",
      TRUE ~ "Average daily METs <= 150")
    y <- factor(y, levels=c("Average daily METs <= 150", "Average daily METs > 150", "Unknown"))
    return(y)
  }, 
  display_name="METsover150", 
  description="Indicates whether the participant's average daily METs exceed 150"
)

ISCED <- list(
  name="TEU_Edu_ISCED", 
  source="TEU_Edu_HighestQual", 
  mapper=function(x){
    # Convert UKB qualification categories into ISCED education categories
    y <- dplyr::case_when(
      x == "College or University degree" ~ "ISCED 5: First stage of tertiary education",
      x == "NVQ or HND or HNC or equivalent" ~ "ISCED 5: First stage of tertiary education",
      x == "Other professional qualifications eg: nursing, teaching" ~ "ISCED 4: Post-secondary non-tertiary education",
      x == "A levels/AS levels or equivalent" ~ "ISCED 3: Upper secondary education",
      x == "O levels/GCSEs or equivalent" ~ "ISCED 2: Lower secondary education",
      x == "CSEs or equivalent" ~ "ISCED 2: Lower secondary education",
      x == "None of the above" ~ "ISCED 1: Primary education",
      x == "Prefer not to answer" ~ "Unanswered",
      is.na(x) ~ "Unanswered"
    )
    y <- factor(y, levels=c("ISCED 5: First stage of tertiary education", 
                            "ISCED 4: Post-secondary non-tertiary education",
                            "ISCED 3: Upper secondary education",
                            "ISCED 2: Lower secondary education",
                            "ISCED 1: Primary education", 
                            "Unanswered")) 
    return(y)
  }, 
  display_name="ISCED", 
  description="ISCED category of participant's highest attained qualification"
)

hypertensionseverity <- list(
  name="TEU_Blp_HTNseverity", 
  source=c("TEU_Blp_SBP.avg", "TEU_Blp_DBP.avg"), 
  mapper=function(data){
    y <- dplyr::case_when(
      is.na(data[["TEU_Blp_SBP.avg"]]) | is.na(data[["TEU_Blp_DBP.avg"]]) ~ "Unmeasured",
      data[["TEU_Blp_SBP.avg"]]>=180 | data[["TEU_Blp_DBP.avg"]]>=110 ~ "Stage 3",
      between(data[["TEU_Blp_SBP.avg"]], 160, 180) | between(data[["TEU_Blp_DBP.avg"]], 100, 110) ~ "Stage 2",
      between(data[["TEU_Blp_SBP.avg"]], 140, 160) | between(data[["TEU_Blp_DBP.avg"]], 90, 100) ~ "Stage 1",
      TRUE ~ "Normotensive"
    )
    y <- factor(y, levels=c("Normotensive", "Stage 1", "Stage 2", "Stage 3", "Unmeasured"))
    return(y)
  }, 
  display_name="", 
  description=""
)


bowelscreen <- list(
  name="TEU_HMH_BowelCancerScreen", 
  source="HMH_BowelSc", 
  mapper=function(x){
    y <- as.character(x)
    y[y %in% c("Prefer not to answer", "Do not know") | is.na(y)] <- "Unanswered"
    y <- factor(y, levels=c("Yes", "No", "Unanswered"), 
                labels=c("Screened for bowel cancer", "Not screened for bowel cancer", "Unanswered"), ordered=FALSE)
    return(y)
  }, 
  display_name="BowelCancerScreen", 
  description="Whether the individual has been screened for bowel cancer - used as a proxy for engagement with healthcare"
)

familyhistoryCVD <- list(
  name="TEU_FaH_CVD", 
  source=c(paste0("FaH_FatherIll.", c(0:9)), 
           paste0("FaH_MotherIll.", c(0:10)),
           paste0("FaH_SibIll.", c(0:11))), 
  mapper=FN_FamHist(conditions=c("Heart disease", "High blood pressure", "Stroke"), label="CVD"), 
  display_name="FamilyHistoryCVD", 
  description="Family history of CVD (Heart disease, high blood pressure, stroke)"
)

neo_derived <- c(Neoethnicity, METsover150, alcohol, smoking, weeklyalcohol, bingealcohol, ISCED,
                 hypertensionseverity, bowelscreen)