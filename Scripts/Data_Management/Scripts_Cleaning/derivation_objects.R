# Jennifer Collister
# 22/09/20

library(glue)
library(lubridate)
library(readxl)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Source the function definitions
# XL: Remove 'Reorganise' in the file path
source(file.path(config$scripts$cleaning, "basic_functions.R"), local = TRUE)

# makeEnum <- function(inputList) {
#   # Borrowed from https://stackoverflow.com/a/41509345
#   myEnum <- as.list(inputList)
#   enumNames <- names(myEnum)
#   if (is.null(enumNames)) {
#     names(myEnum) <- myEnum
#   } else if ("" %in% enumNames) {
#     stop("The inputList has some but not all names assigned. They must be all assigned or none assigned")
#   }
#   return(myEnum)
# }
# visits <- makeEnum(list(baseline = c("0", "baseline assessment"), 
#                     repeat_visit = c("1", "repeat visit"), 
#                     imaging = c("2", "imaging visit"), 
#                     repeat_imaging = c("3","repeat imaging visit")))

# Formatting of existing UKB variables

ID <- function() {
  list(
    name = "ID",
    source = "ID",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ID",
    description = "The unique participant identifier"
  )
}

PsF_VisitFreq <- function(instance = visits$baseline) {
  list(
    name = glue("PsF_VisitFreq.{instance[1]}.0"),
    source = glue("PsF_VisitFreq.{instance[1]}.0"),
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Family/friend visit frequency at {instance[2]}"),
    description = glue("Frequency of family/friend visits (recorded at {instance[2]})")
  )
}

BaC_Sex <- function() {
  list(
    name = "BaC_Sex",
    source = "BaC_Sex.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "gender",
    description = "Participant's self-reported gender"
  )
}

Rec_DateAssess <- function() {
  list(
    name = "Rec_DateAssess",
    source = c("Rec_DateAssess.0.0"),
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = "DateBaselineAssess",
    description = "Date of baseline assessment"
  )
}

Eth_Ethnicity <- function() {
  list(
    name = "Eth_Ethnicity",
    source = "Eth_Ethnicity.0.0",
    mapper = FN_factor(
      levelorder = c(
        "White",
        "British",
        "Irish",
        "Any other white background",
        "Mixed",
        "White and Black Caribbean",
        "White and Black African",
        "White and Asian",
        "Any other mixed background",
        "Asian or Asian British",
        "Indian",
        "Pakistani",
        "Bangladeshi",
        "Any other Asian background",
        "Black or Black British",
        "Caribbean",
        "African",
        "Any other Black background",
        "Chinese",
        "Other ethnic group",
        "Do not know",
        "Prefer not to answer"
      )
    ),
    post_exclusion = FALSE,
    display_name = "ethnicity",
    description = "The participant's self-reported ethnicity (raw UKB categories)"
  )
}

BaC_RsnLostFU <- function() {
  list(
    name = "BaC_RsnLostFU.0.0",
    source = "BaC_RsnLostFU.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "lfu_reason",
    description = "The reported reason for loss to follow-up"
  )
}

TEU_BaC_DateOfBirth <- function() {
  list(
    name = "TEU_BaC_DateOfBirth",
    source = c("BaC_BirthMonth.0.0", "BaC_BirthYear.0.0"),
    mapper = FN_MYtoDate(
      day = 15,
      monthField = "BaC_BirthMonth.0.0",
      yearField = "BaC_BirthYear.0.0"
    ),
    post_exclusion = FALSE,
    display_name = "DateOfBirth",
    description = "The participant's approximate date of birth, derived from self-reported month and year with date estimated as 15th"
  )
}

TEU_BaC_AgeAtRec <- function() {
  list(
    name = "TEU_BaC_AgeAtRec",
    source = c("TEU_BaC_DateOfBirth", "Rec_DateAssess"),
    mapper = function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess"]], data[["TEU_BaC_DateOfBirth"]], unit =
                                  "days") / 365.25,
                       digits = 2))
    },
    post_exclusion = FALSE,
    display_name = "AgeAtRecruitment",
    description = "The participant's approximate age at recruitment, derived from date of assessment centre visit and self-reported month and year of birth (date of birth estimated as 15th of the month)"
  )
}

TEU_BlP_SBP.0.0 <- function() {
  list(
    name = "TEU_BlP_SBP.0.0",
    source = c("BlP_SBPAuto.0.0", "BlP_SBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.0"]], data[["BlP_SBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First SBP at baseline",
    description = "First SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_SBP.0.1 <- function() {
  list(
    name = "TEU_BlP_SBP.0.1",
    source = c("BlP_SBPAuto.0.1", "BlP_SBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.1"]], data[["BlP_SBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second SBP at baseline",
    description = "Second SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.0 <- function() {
  list(
    name = "TEU_BlP_DBP.0.0",
    source = c("BlP_DBPAuto.0.0", "BlP_DBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.0"]], data[["BlP_DBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First DBP at baseline",
    description = "First DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.1 <- function() {
  list(
    name = "TEU_BlP_DBP.0.1",
    source = c("BlP_DBPAuto.0.1", "BlP_DBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.1"]], data[["BlP_DBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second DBP at baseline",
    description = "Second DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_nSBP <- function() {
  list(
    name = "TEU_BlP_nSBP",
    source = c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. SBP",
    description = "Number of SBP measurements taken at baseline"
  )
}

TEU_BlP_nDBP <- function() {
  list(
    name = "TEU_BlP_nDBP",
    source = c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. DBP",
    description = "Number of DBP measurements taken at baseline"
  )
}

TEU_BlP_SBP.avg <- function() {
  list(
    name = "TEU_BlP_SBP.avg",
    source = c("TEU_BlP_SBP.0.0",
               "TEU_BlP_SBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_SBP.0.0",
                                     "TEU_BlP_SBP.0.1")),
    post_exclusion = FALSE,
    display_name = "SBP",
    description = "The average systolic blood pressure at baseline"
  )
}

TEU_BlP_DBP.avg <- function() {
  list(
    name = "TEU_BlP_DBP.avg",
    source = c("TEU_BlP_DBP.0.0",
               "TEU_BlP_DBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_DBP.0.0",
                                     "TEU_BlP_DBP.0.1")),
    post_exclusion = FALSE,
    display_name = "DBP",
    description = "The average diastolic blood pressure at baseline"
  )
}

TEU_BlP_measuredHTN <- function(SBPthreshold = 140, DBPthreshold = 90) {
  list(
    name = "TEU_BlP_measuredHTN",
    source = c("TEU_BlP_SBP.avg", "TEU_BlP_DBP.avg"),
    mapper = function(data) {
      data[["TEU_BlP_SBP.avg"]] >= SBPthreshold |
        data[["TEU_BlP_DBP.avg"]] >= DBPthreshold
    },
    post_exclusion = FALSE,
    display_name = "measuredHTN",
    description = paste0(
      "Whether the participant had hypertensive BP (>=",
      SBPthreshold,
      "/",
      DBPthreshold,
      ") measured at baseline"
    )
  )
}

Alc_Status <- function() {
  list(
    name = "Alc_Status",
    source = "Alc_Status.0.0",
    mapper = FN_factor(
      levelorder = c("Never", "Previous", "Current", "Prefer not to answer")
    ),
    post_exclusion = FALSE,
    display_name = "Alc_Status",
    description = "Self-reported alcohol status"
  )
}

Smo_Status <- function() {
  list(
    name = "Smo_Status",
    source = "Smo_Status.0.0",
    mapper = FN_factor(
      levelorder = c("Never", "Previous", "Current", "Prefer not to answer")
    ),
    post_exclusion = FALSE,
    display_name = "Smo_Status",
    description = "Self-reported smoking status"
  )
}

TEU_HoH_PreTaxInc <- function() {
  list(
    name = "TEU_HoH_PreTaxInc",
    source = c("HoH_PreTaxInc.0.0", "HoH_PreTaxInc_P.0.0"),
    mapper = function(data) {
      y <- ifelse(is.na(data[["HoH_PreTaxInc.0.0"]]),
                  as.character(data[["HoH_PreTaxInc_P.0.0"]]),
                  as.character(data[["HoH_PreTaxInc.0.0"]]))
      y <- fct_collapse(
        y,
        "Less than 18,000" = "Less than 18,000",
        "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
        "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
        "52,000 to 100,000" = "52,000 to 100,000",
        "Greater than 100,000" = "Greater than 100,000",
        "Do not know" = "Do not know",
        "Prefer not to answer" = "Prefer not to answer"
      )
      y <-
        factor(
          y,
          levels = c(
            "Less than 18,000",
            "18,000 to 30,999",
            "31,000 to 51,999",
            "52,000 to 100,000",
            "Greater than 100,000",
            "Do not know",
            "Prefer not to answer"
          )
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "HouseholdIncome",
    description = "Participant's pre-tax household income"
  )
}

Sle_Duration <- function() {
  list(
    name = "Sle_Duration",
    source = "Sle_Duration.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "SleepDuration_h",
    description = "Participant's self-reported average sleep duration in hours"
  )
}

BSM_BMI<-function(){
  list(
    name = 'BSM_BMI',
    source = 'BSM_BMI.0.0',
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'BMI',
    description = 'Body mass index (BMI) Kg/m2'
  )
}

TEU_BSM_BMIcat <- function() {
  list(
    name = "TEU_BSM_BMIcat",
    source = "BSM_BMI.0.0",
    mapper = function(x) {
      y <-
        as.character(cut(x, breaks = c(0, 18.5, 25, 30, 200), right = FALSE))
      y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)", "Unknown"),
          labels = c("Normal", "Underweight", "Overweight", "Obese", "Unknown")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "BMIcat",
    description = "BMI below 18.5 was considered “underweight”, between 18.5 and 25 was “normal”, between 25 and 30 was “overweight” and above 30 was “obese”"
  )
}

TEU_BSM_WaistCircCat <- function() {
  list(
    name = "TEU_BSM_WaistCircCat",
    source = c("BSM_Waist.0.0", "BaC_Sex.0.0"),
    mapper = function(data) {
      # Categorise waist circ into labelled categories
      y <- dplyr::case_when(
        data[["BaC_Sex.0.0"]] == "Female" & data[["BSM_Waist.0.0"]] >= 88 ~ "Obese",
        data[["BaC_Sex.0.0"]] == "Female" & data[["BSM_Waist.0.0"]] >= 80 ~ "Overweight",
        data[["BaC_Sex.0.0"]] == "Male" & data[["BSM_Waist.0.0"]] >= 102 ~ "Obese",
        data[["BaC_Sex.0.0"]] == "Male" & data[["BSM_Waist.0.0"]] >= 94 ~ "Overweight",
        is.na(data[["BSM_Waist.0.0"]]) ~ "Unknown",
        TRUE ~ "Normal"
      )
      y <-
        factor(y, levels = c("Normal", "Overweight", "Obese", "Unknown"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "WaistCirc",
    description = "Categorised waist circumference.\nFemales with a waist circumference between 80 and 88cm were considered overweight, greater than 88cm was considered obese.\nMales with a waist circumference between 94 and 102cm were considered overweight, greater than 102cm was considered obese."
  )
}

PhA_METsWkAllAct <- function() {
  list(
    name = "PhA_METsWkAllAct",
    source = "PhA_METsWkAllAct.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "WeeklyMETs",
    description = paste0("Summed MET minutes per week for all activity, derived from participant self-reported weekly exercise. This variable was generated as part of ",
                         text_spec("UKB application 12184", link = "http://bmjopen.bmj.com/content/6/3/e010038"), 
                         " and made available on the Data Showcase.")
  )
}

CoF_RTTTimeID <- function() {
  list(
    name = "CoF_RTTTimeID",
    source = "CoF_RTTTimeID.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ReactionTime",
    description = "Reaction time in a game of snap, in seconds"
  )
}

# XL add: 17/11/2020
Edu_HighestQual<-function(){
  list(
    name = "Edu_HighestQual",
    source = c(paste0("Edu_Qualif.0.", seq(0, 5, by=1)),
               paste0("Edu_Qualif_p.0.", seq(0, 4, by=1))),
    mapper = function(data){
      qual_list <- c(
        "College or University degree",
        "NVQ or HND or HNC or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "A levels/AS levels or equivalent",
        "O levels/GCSEs or equivalent",
        "CSEs or equivalent",
        "None of the above",
        "Prefer not to answer"
      )
      for(i in seq(length(qual_list), 1, by=-1)) {
        data[data == qual_list[i]] <- as.character(i)
      }
      y <- do.call(pmax, c(data, list(na.rm=TRUE)))
      #y[is.na(y)] <- 1
      y <- factor(y,
                  levels = seq(1, length(qual_list), by=1),
                  labels = qual_list
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'HighestQualification',
    description = "Highest of a participant's self-reported educational qualidications"
  )
}

TEU_Edu_HighestQual <- function() {
  list(
    name = "TEU_Edu_HighestQual",
    source = c(paste0("Edu_Qualif.0.", seq(0, 5, by=1)),
               paste0("Edu_Qualif_p.0.", seq(0, 4, by=1))),
    mapper = function(data) {
      qual_list <- c(
        "College or University degree",
        "NVQ or HND or HNC or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "A levels/AS levels or equivalent",
        "O levels/GCSEs or equivalent",
        "CSEs or equivalent",
        "None of the above",
        "Unanswered"
      )
      for(i in seq(length(qual_list), 1, by=-1)) {
        data[data == qual_list[i]] <- as.character(i)
      }
      y <- do.call(pmax, c(data, list(na.rm=TRUE)))
      y[is.na(y)] <- 1
      y <- factor(y,
                  levels = seq(1, length(qual_list), by=1),
                  labels = qual_list
                  )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "HighestQualification",
    description = "Highest of a participant's self-reported educational qualifications"
  )
}

GAC_AideMem <- function() {
  list(
    name = "GAC_AideMem",
    source = "GAC_AideMem.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "AideMemoir",
    description = "Did the participant bring the requested aide-memoir with a note of their medications and previous operations?"
  )
}

TEU_Rec_AssessCentre <- function() {
  list(
    name = "TEU_Rec_AssessCentre",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      map <- read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding10.csv")
      # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=10
      y <- merge(x,
                 map,
                 by.x = "x",
                 by.y = "value",
                 all.x = TRUE)
      y <- y[["meaning"]]
    },
    post_exclusion = FALSE,
    display_name = "AssessCentre",
    description = "Which assessment centre did the participant attend"
  )
}

TEU_Rec_Country <- function() {
  list(
    name = "TEU_Rec_Country",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c(
          10003,
          11001,
          11002,
          11006,
          11007,
          11008,
          11009,
          11010,
          11011,
          11012,
          11013,
          11014,
          11016,
          11017,
          11018,
          11020,
          11021,
          11024,
          11025,
          11026,
          11027,
          11028
        ) ~ "England",
        x %in% c(11004, 11005) ~ "Scotland",
        x %in% c(11003, 11022, 11023) ~ "Wales",
        TRUE ~ x
      )
      if (!all(y %in% c("England", "Scotland", "Wales"))) {
        warning(paste0("Unrecognised centre code: ", y[!y %in% c("England", "Scotland", "Wales")]))
      }
      y <- factor(y, levels=c("England", "Scotland", "Wales"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "CountryResidence",
    description = "Which country does the participant live in"
  )
}

TownsendDepInd <- function(){
  list(
    name = 'TownsendDepInd',
    source = c("BaC_DeprivInd.0.0"),
    mapper = FN_id,
    post_exclusion =TRUE,
    display_name = 'Townsend Deprivation Index',
    description = 'Townsend Deprivation Index'
  )
}

TEU_TownsendDepInd_Quint <- function() {
  list(
    name = "TEU_TownsendDepInd_Quint",
    source = c("BaC_DeprivInd.0.0"),
    mapper = FN_quantiles(
      quant = 5,
      labels = c("Q1: least deprived", "Q2", "Q3", "Q4", "Q5: most deprived")
    ),
    post_exclusion = TRUE,
    display_name = "Townsend Deprivation Index, quintiles",
    description = "Quintiles of the Townsend Deprivation Index: 1st is least deprived, 5th is most deprived."
  )
}

TEU_BaC_AgeCat <- function() {
  list(
    name = "TEU_BaC_AgeCat",
    source = "TEU_BaC_AgeAtRec",
    mapper = FN_buckets(
      breaks = c(40, 50, 60, 70),
      labels = c("40-49", "50-59", "60-69"),
      right = FALSE
    ),
    post_exclusion = FALSE,
    display_name = "AgeCategory",
    description = "Categorised age in years"
  )
}

# XL add: 17/11/2020
HMH_Meds_any <- function() {
  list(
    name = "HMH_Meds_any",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_any_raw,
    post_exclusion = FALSE,
    display_name = "HMH_Meds",
    description = "Did the participant self-report taking medication for cholesterol, blood pressure, diabetes or HRT?"
  )
}



TEU_HMH_Meds_any <- function() {
  list(
    name = "TEU_HMH_Meds_any",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
               ),
    mapper = FN_HMHmeds_any,
    post_exclusion = FALSE,
    display_name = "HMH_Meds",
    description = "Did the participant self-report taking medication for cholesterol, blood pressure, diabetes or HRT?"
  )
}

TEU_HMH_Meds_BP <- function() {
  list(
    name = "TEU_HMH_Meds_BP",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
               ),
    mapper = FN_HMHmeds_type(medtype = "Blood pressure medication", string = "BP meds"),
    post_exclusion = FALSE,
    display_name = "HBPmeds",
    description = "Participant self-reported taking BP medication in the touchscreen questionnaire"
  )
}

TEU_HMH_Meds_Chol <- function() {
  list(
    name = "TEU_HMH_Meds_Chol",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_type(medtype = "Cholesterol lowering medication", string = "cholesterol meds"),
    post_exclusion = FALSE,
    display_name = "Cholmeds",
    description = "Participant self-reported taking cholesterol lowering medication in the touchscreen questionnaire"
  )
}

TEU_ethnicgrp <- function() {
  list(
    name = "TEU_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x %in% c("Do not know", "Prefer not to answer") ~ "Unanswered",
        is.na(x) ~ "Unanswered",
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other", "Unanswered")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "ethnic_group",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}

# XL add: Same as above except leave 'Do not know' 'Prefer not to answer' and NA as it is 
Eth_ethnicgrp<-function() {
  list(
    name = "Eth_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x == 'Do not know' ~ 'Do not know',
        x == 'Prefer not to answer' ~ 'Prefer not to answer',
        is.na(x) ~ NA_character_,
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other", "Do not know", "Prefer not to answer")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "ethnic_group",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}

TEU_Alc_Status <- function() {
  list(
    name = "TEU_Alc_Status",
    source = "Alc_Status.0.0",
    mapper = FN_MissingCategory(
      missingvals = c("Prefer not to answer"),
      categ_name = "Unanswered"
    ),
    post_exclusion = FALSE,
    display_name = "AlcoholStatus",
    description = "Self-reported alcohol drinking status"
  )
}

TEU_Smo_Status <- function() {
  list(
    name = "TEU_Smo_Status",
    source = "Smo_Status.0.0",
    mapper = FN_MissingCategory(
      missingvals = c("Prefer not to answer"),
      categ_name = "Unanswered"
    ),
    post_exclusion = FALSE,
    display_name = "SmokingStatus",
    description = "Self-reported smoking status"
  )
}

TEU_Alc_WeeklyAlcUnits <- function() {
  list(
    name = "TEU_Alc_WeeklyAlcUnits",
    source = c(
      "Alc_RedWineWk.0.0",
      "Alc_WhiteWineWk.0.0",
      "Alc_BeerCiderWk.0.0",
      "Alc_SpiritsWk.0.0",
      "Alc_FortWineWk.0.0",
      "Alc_OtherAlcWk.0.0"
    ),
    mapper = function(data) {
      alcservings <- data
      for (alc in c(
        "Alc_RedWineWk.0.0",
        "Alc_WhiteWineWk.0.0",
        "Alc_BeerCiderWk.0.0",
        "Alc_SpiritsWk.0.0",
        "Alc_FortWineWk.0.0",
        "Alc_OtherAlcWk.0.0"
      )) {
        alcservings[[alc]][alcservings[[alc]] < 0 | is.na(alcservings[[alc]])] <-  0
      }
      
      weekly_alcunits <-
        #	Red wine (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_RedWineWk.0.0) +
        # White wine, champagne (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_WhiteWineWk.0.0) +
        #	Fortified wines: e.g. sherry, port (1 measure, 50ml, ABV 20% = 1 unit)
        (1.0 * alcservings$Alc_FortWineWk.0.0) +
        #	Beer, cider including bitter, lager, stout, ale, Guinness (1 pint, 568ml, ABV 3.6% = 2 units)
        (2.0 * alcservings$Alc_BeerCiderWk.0.0) +
        #	Spirits, liquors (1 measure or shot, 25ml, ABV 40% = 1 unit)
        (1.0 * alcservings$Alc_SpiritsWk.0.0) +
        #	For "other" types of alcohol, will use alcopops as proxy ( 1 drink, 275ml, ABV 5.5% = 1.5 units)
        (1.5 * alcservings$Alc_OtherAlcWk.0.0)
      
      # Truncate alcohol consumption at upper 95th percentile
      upper95 <- quantile(weekly_alcunits, 0.95, na.rm = TRUE)
      weekly_alcunits[weekly_alcunits > upper95 & !is.na(weekly_alcunits)] <- upper95
      weekly_alcunits[is.na(weekly_alcunits)] <- 0
      
      return(weekly_alcunits)
    },
    post_exclusion = FALSE,
    display_name = "WeeklyAlcUnits",
    description = "Total weekly units of alcohol, derived from self-reported average weekly consumption of each different type alcohol and truncated at the upper 95th percentile. This data was available for participants who said they drank alcohol more than once or twice a week."
  )
}

TEU_Alc_WeeklyCat <- function() {
  list(
    name = "TEU_Alc_WeeklyCat", 
    source = c("TEU_Alc_WeeklyAlcUnits"), 
    mapper = FN_buckets(breaks=c(-1, 0, 5, 10, 20, 30, 100),
                        labels=c("None reported", "Less than 5 units", "5 to 10 units", 
                                 "10 to 20 units", "20 to 30 units", "30 units or more"),
                        right=FALSE),
    post_exclusion = FALSE,
    display_name = "Weekly alcohol, categorical",
    description = "Categorised weekly alcohol intake, derived from self-reported average weekly consumption of different types of alcohol. This data was available for participants who said they drank alcohol more than once or twice a week."
  )
}

TEU_Alc_Binge <- function() {
  list(
    name = "TEU_Alc_Binge",
    source = c("TEU_Alc_WeeklyAlcUnits", "BaC_Sex"),
    mapper = function(data) {
      y <- dplyr::case_when(data[["BaC_Sex"]] == "Female" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 7 ~ TRUE,
                            data[["BaC_Sex"]] == "Male" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 14 ~ TRUE,
                            TRUE ~ FALSE)
    },
    post_exclusion = FALSE,
    display_name = "HarmfulAlcohol",
    description = "Does the patient's self-reported weekly alcohol consumption exceed the threshold for binge drinking. Data on weekly alcohol consumption was available for participants who said they drank alcohol more than once or twice a week, those who drank less frequently were not considered to have harmful alcohol consumption."
  )
}

# XL add: 19/11/2020
PhA_METsWkAllAct <- function(){
  list(
    name = 'PhA_METsWkAllAct',
    source = "PhA_METsWkAllAct.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'Summed_MET',
    description = 'Summed MET minutes per week for all activity'
  )
}

TEU_Pha_METsover1200 <- function() {
  list(
    name = "TEU_Pha_METsover1200",
    source = "PhA_METsWkAllAct.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        # XL add: Change 'Unknown' to 'Unanswered'
        is.na(x) ~ "Unanswered",
        x > 1200 ~ "High (MET minutes > 1200)",
        x <= 1200 ~ "Low (MET minutes <= 1200)",
        TRUE ~ "Other"
      )
      y <-
        factor(y,
               levels = c("High (MET minutes > 1200)", "Low (MET minutes <= 1200)", "Unanswered"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Sufficient_METs",
    description = paste0("Indicates whether the participant is exceeding 1200 MET minutes per week. The source variable (summed MET minutes per week for all activity, derived from participant self-reported weekly exercise) was generated as part of ",
                         text_spec("UKB application 12184", link = "http://bmjopen.bmj.com/content/6/3/e010038"), 
                         " and made available on the Data Showcase.")
  )
}

TEU_Edu_ISCED <- function() {
  list(
    name = "TEU_Edu_ISCED",
    source = "TEU_Edu_HighestQual",
    mapper = function(x) {
      # Convert UKB qualification categories into ISCED education categories
      y <- dplyr::case_when(
        x == "College or University degree" ~ "5: Tertiary",
        x == "NVQ or HND or HNC or equivalent" ~ "5: Tertiary",
        x == "Other professional qualifications eg: nursing, teaching" ~ "4: Post-secondary non-tertiary",
        x == "A levels/AS levels or equivalent" ~ "2-3: Secondary",
        x == "O levels/GCSEs or equivalent" ~ "2-3: Secondary",
        x == "CSEs or equivalent" ~ "2-3: Secondary",
        x == "None of the above" ~ "1: Primary",
        x == "Prefer not to answer" ~ "Unanswered",
        is.na(x) ~ "Unanswered"
      )
      y <- factor(
        y,
        levels = c(
          "5: Tertiary",
          "4: Post-secondary non-tertiary",
          "2-3: Secondary" ,
          "1: Primary" ,
          "Unanswered"
        )
      )
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "ISCED",
    description = "ISCED category of participant's highest attained qualification"
  )
}

TEU_BlP_HTNseverity <- function() {
  list(
    name = "TEU_BlP_HTNseverity",
    source = c("TEU_BlP_SBP.avg", "TEU_BlP_DBP.avg"),
    mapper = function(data) {
      y <- dplyr::case_when(
        is.na(data[["TEU_BlP_SBP.avg"]]) |
          is.na(data[["TEU_BlP_DBP.avg"]]) ~ "Unmeasured",
        data[["TEU_BlP_SBP.avg"]] >= 180 |
          data[["TEU_BlP_DBP.avg"]] >= 110 ~ "Stage 3",
        between(data[["TEU_BlP_SBP.avg"]], 160, 180) |
          between(data[["TEU_BlP_DBP.avg"]], 100, 110) ~ "Stage 2",
        between(data[["TEU_BlP_SBP.avg"]], 140, 160) |
          between(data[["TEU_BlP_DBP.avg"]], 90, 100) ~ "Stage 1",
        TRUE ~ "Normotensive"
      )
      y <-
        factor(y,
               levels = c("Normotensive", "Stage 1", "Stage 2", "Stage 3", "Unmeasured"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Hypertension severity",
    description = "Stage I corresponds to systolic BP 140-159 mmHg or diastolic BP 90-99 mmHg, stage II mean systolic BP 160-179 mmHg or diastolic BP 100-110 mmHg, and stage III mean systolic BP>180 mmHg or diastolic BP>110 mmHg (also referred to as hypertensive crisis)."
  )
}

TEU_HMH_BowelCancerScreen <- function() {
  list(
    name = "TEU_HMH_BowelCancerScreen",
    source = "HMH_BowelSc.0.0",
    mapper = function(x) {
      y <- as.character(x)
      y[y %in% c("Prefer not to answer", "Do not know") |
          is.na(y)] <- "Unanswered"
      y <- factor(
        y,
        levels = c("Yes", "No", "Unanswered"),
        labels = c(
          "Screened for bowel cancer",
          "Not screened for bowel cancer",
          "Unanswered"
        ),
        ordered = FALSE
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "BowelCancerScreen",
    description = "Whether the individual has been screened for bowel cancer - used as a proxy for engagement with healthcare"
  )
}

TEU_FaH_CVD <- function() {
  list(
    name = "TEU_FaH_CVD",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Heart disease", "High blood pressure", "Stroke"),
      label = "CVD"
    ),
    post_exclusion = FALSE,
    display_name = "FamilyHistoryCVD",
    description = "Family history of CVD (Heart disease, high blood pressure, stroke), derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

VeI_PregnantNow <- function() {
  list(
    name = "VeI_PregnantNow", 
    source = c("VeI_PregnantNow.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Pregnant",
    description = "Was the participant pregnant at baseline"
  )
}

TEU_SBP_PRS <- function() {
  list(
    name = "TEU_SBP_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/bmrc-ukb-repo/prs/projects/htn-evangelou2018/outputs/htn-evangelou2018_PRS_QC1.rds",
                        colname="PRS_SBP"),
    post_exclusion = FALSE,
    display_name = "SBP polygenic risk score",
    description = "SBP polygenic risk score from Evangelou 2018 paper"
  )
}

TEU_DBP_PRS <- function() {
  list(
    name = "TEU_DBP_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/bmrc-ukb-repo/prs/projects/htn-evangelou2018/outputs/htn-evangelou2018_PRS_QC1.rds",
                        colname="PRS_DBP"),
    post_exclusion = FALSE,
    display_name = "DBP polygenic risk score",
    description = "DBP polygenic risk score from Evangelou 2018 paper"
  )
}

TEU_BP_PRS <- function() {
  list(
    name = "TEU_BP_PRS", 
    source = c("TEU_DBP_PRS", "TEU_SBP_PRS"), 
    mapper = FN_average(colnames=c("TEU_DBP_PRS", "TEU_SBP_PRS")),
    post_exclusion = FALSE,
    display_name = "Mean BP PRS score",
    description = "Average of SBP and DBP PRS scores"
  )
}

TEU_BP_PRS_quintiles <- function() {
  list(
    name = "TEU_BP_PRS_quintiles", 
    source = c("TEU_BP_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "BP PRS Quintiles",
    description = "Quintiles of the BP PRS score"
  )
}

TEU_LDL_C_PRS <- function() {
  list(
    name = "TEU_LDL_C_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/bmrc-ukb-repo/prs/projects/PGS000115/outputs/PGS000115_PRS_QC1.rds",
                        colname="PRS"),
    post_exclusion = FALSE,
    display_name = "LDL Cholesterol polygenic risk score",
    description = "LDL Cholesterol polygenic risk score, from Trinder 2020 paper"
  )
}

TEU_LDL_C_PRS_quintiles <- function() {
  list(
    name = "TEU_LDL_C_PRS_quintiles", 
    source = c("TEU_LDL_C_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "LDL Cholesterol PRS Quintiles",
    description = "Quintiles of the LDL Cholesterol PRS score"
  )
}

TEU_LDL_C_PRS_deciles <- function() {
  list(
    name = "TEU_LDL_C_PRS_deciles", 
    source = c("TEU_LDL_C_PRS"), 
    mapper = FN_quantiles(quant=10),
    post_exclusion = TRUE,
    display_name = "LDL Cholesterol PRS deciles",
    description = "Deciles of the LDL Cholesterol PRS score"
  )
}

TEU_LDL_C_PRS_centiles <- function() {
  list(
    name = "TEU_LDL_C_PRS_centiles", 
    source = c("TEU_LDL_C_PRS"), 
    mapper = FN_quantiles(quant=100),
    post_exclusion = TRUE,
    display_name = "LDL Cholesterol PRS centiles",
    description = "Centiles of the LDL Cholesterol PRS score"
  )
}

# XL add: 17/11/2020
HMH_VascCond <- function() {
  list(
    name = "HMH_VascCond", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_any_raw,
    post_exclusion = FALSE,
    display_name = "Vascular condition",
    description = "Did the participant self-report any vascular conditions in the touchscreen questionnaire"
  )
}



TEU_HMH_VascCond <- function() {
  list(
    name = "TEU_HMH_VascCond", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_any,
    post_exclusion = FALSE,
    display_name = "Vascular condition",
    description = "Did the participant self-report any vascular conditions in the touchscreen questionnaire"
  )
}

TEU_HMH_prevHTN <- function() {
  list(
    name = "TEU_HMH_prevHTN", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_condition(condition="High blood pressure", string="hypertension"),
    post_exclusion = FALSE,
    display_name = "Self-reported HTN on TQ",
    description = "Self-reported high blood pressure on touchscreen questionnaire"
  )
}

TEU_HMH_prevstroke <- function() {
  list(
    name = "TEU_HMH_prevstroke", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_condition(condition="Stroke", string="stroke"),
    post_exclusion = FALSE,
    display_name = "Self-reported stroke on TQ",
    description = "Self-reported prior stroke on touchscreen questionnaire"
  )
}

TEU_HMH_prevCVD <- function() {
  list(
    name = "TEU_HMH_prevCVD", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_condition(condition=c("Angina", "Heart attack"), string="cardiovascular disease"),
    post_exclusion = FALSE,
    display_name = "Self-reported CVD on TQ",
    description = "Self-reported prior angina or MI on touchscreen questionnaire"
  )
}

HMH_IllDisab <- function() {
  list(
    name = "HMH_IllDisab", 
    source = c("HMH_IllDisab.0.0"), 
    mapper = FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion = FALSE,
    display_name = "Self-reported illness or disability",
    description = "Participant self-reported longstanding illness, disability or infirmity on the touchscreen questionnaire"
  )
}

HMH_Diabetes <- function() {
  list(
    name = "HMH_Diabetes", 
    source = c("HMH_Diabetes.0.0"), 
    mapper = FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion = FALSE,
    display_name = "Self-reported diabetes",
    description = "Participant self-reported diabetes on the touchscreen questionnaire"
  )
}

HMH_HTNAge <- function() {
  list(
    name = "HTN_HTNAge", 
    source = c("HMH_HBPAge.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Age HTN diagnosed",
    description = "Participant self-reported age of hypertension diagnosis on the touchscreen questionnaire"
  )
}

# XL add: 17/11/2020
HMH_BowelSc <- function(){
  list(
    name='HMH_BowelSc',
    source=c('HMH_BowelSc.0.0'),
    mapper= FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion=FALSE,
    display_name='Self-reported ever had bowel cancer screening',
    description='Participant self-reported ever had bowel cancer screening on the touchscreen questionnaire'
  )
}

BBC_CHOL_Result <- function() {
  list(
    name = "BBC_CHOL_Result", 
    source = c("BBC_CHOL_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Total cholesterol level at baseline",
    description = "Total cholesterol assay from baseline blood serum"
  )
}

BBC_HDL_Result <- function() {
  list(
    name = "BBC_HDL_Result", 
    source = c("BBC_HDL_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "HDL cholesterol level at baseline",
    description = "HDL cholesterol assay from baseline blood serum"
  )
}

BBC_LDL_Result <- function() {
  list(
    name = "BBC_LDL_Result", 
    source = c("BBC_LDL_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "LDL cholesterol level at baseline",
    description = "LDL cholesterol assay from baseline blood serum"
  )
}


GeP_PC <- function(pc=1) {
  list(
    name = paste0("GeP_PC_", pc), 
    source = glue("GeP_PC.0.{pc}"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Principal component {pc}"),
    description = glue("Genetic principal component {pc}, from Bycroft")
  )
}

GeP_Batch <- function() {
  list(
    name = "GeP_Batch", 
    source = "GeP_Batch.0.0", 
    mapper = FN_toNumeric,
    post_exclusion = FALSE,
    display_name = "Genotype measurement batch",
    description = "Genotype measurement batch"
  )
}

GeP_ethnic <- function() {
  list(
    name = "GeP_ethnic", 
    source = "GeP_ethnic.0.0", 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Genotype ethnic grouping",
    description = "Genotype ethnic grouping"
  )
}


TEU_VeI_HTN_prevalent <- function(dx_codes = c(1065, 1072)) {
  list(
    list(
      name = "TEU_VeI_HTN",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding6_noncancerVI.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported HTN, verbal interview",
      description = "Whether hypertension was reported in verbal interview at baseline"
    ),
    list(
      name = "TEU_VeI_HTN_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0, 
                                 return_label = "duration",
                                 mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding6_noncancerVI.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported duration of hypertension, verbal interview",
      description = "Duration of hypertension reported in verbal interview at baseline"
    )
  )
}

TEU_selfrepHTN_dx <- function() {
  list(
    name = "TEU_selfrepHTN_dx", 
    source = c("TEU_VeI_HTN", "TEU_HMH_prevHTN"), 
    mapper = function(data) {
      VI <- !is.na(data[["TEU_VeI_HTN"]])
      TQ <- (data[["TEU_HMH_prevHTN"]] == "Self-reported hypertension")
      y <- (VI | TQ)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Self-reported hypertension",
    description = "Whether the participant self-reported a previous diagnosis of hypertension, either in the touchscreen questionnaire or verbal interview"
  )
}

TEU_selfrepHTN_meds <- function() {
  list(
    name = "TEU_selfrepHTN_meds", 
    source = c("TEU_VeI_HTNmeds_rubric", "TEU_HMH_Meds_BP"), 
    mapper = function(data) {
      VI <- data[["TEU_VeI_HTNmeds_rubric"]]
      TQ <- (data[["TEU_HMH_Meds_BP"]] == "Self-reported BP meds")
      y <- (VI | TQ)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Self-reported hypertension medication",
    description = "Whether the participant self-reported that they were taking 'blood pressure medication' either in the touchscreen questionnaire or verbal interview"
  )
}

TEU_VeI_HTNmeds_rubric <- function() {
  list(
    name = "TEU_VeI_HTNmeds_rubric",
    source = "ID",
    mapper = function(x) {
      rubric <- readRDS(file.path(config$data$derived, "HTNMedsRubric.rds"))
      y <- rubric[["HTN_probablemeds"]][match(x, rubric$ID)]
    },
    post_exclusion = FALSE,
    display_name = "Self-reported BP meds in VI",
    description = "Self-reported medications that correspond to a hypertension treatment pathway under our rubric"
  )
}


TEU_evidenceHTN <- function() {
  list(
    name = "TEU_evidenceHTN", 
    source = c("TEU_selfrepHTN_dx", "TEU_selfrepHTN_meds", "TEU_BlP_measuredHTN"), 
    mapper = function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]] | data[["TEU_BlP_measuredHTN"]])
    },
    post_exclusion = FALSE,
    display_name = "Evidence of hypertension",
    description = "Did the participant have evidence of hypertension, defined as self-reported HTN, self-reported HTN meds, or measured HTN at baseline"
  )
}

TEU_awareHTN <- function() {
  list(
    name = "TEU_awareHTN", 
    source = c("TEU_selfrepHTN_dx", "TEU_selfrepHTN_meds", "TEU_evidenceHTN"), 
    mapper = function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]])
      y[(data[["TEU_evidenceHTN"]] == FALSE | is.na(data[["TEU_evidenceHTN"]]))] <- NA
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Awareness of hypertension",
    description = "Was the participant aware of their hypertension, defined as self-reported HTN or self-reported HTN meds among those with evidence of hypertension"
  )
}

TEU_treatedHTN <- function() {
  list(
    name = "TEU_treatedHTN", 
    source = c("TEU_selfrepHTN_meds", "TEU_awareHTN"), 
    mapper = function(data) {
      y <- data[["TEU_selfrepHTN_meds"]]
      y[(data[["TEU_awareHTN"]] == FALSE | is.na(data[["TEU_awareHTN"]]))] <- NA
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Treated hypertension",
    description = "Was the participant taking treatment for their hypertension, defined as self-reported HTN meds among those who were aware of their hypertension"
  )
}

TEU_VeI_CVD_prevalent <- function() {
  list(
    list(
      name = "TEU_VeI_CVD_prevalent_type",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = c(1065, 1074),
                                    colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding6_noncancerVI.csv")),
      post_exclusion = FALSE,
      display_name = "Category of CVD reported in verbal interview at baseline",
      description = "Type of CVD"
    ),
    list(
      name = "TEU_VeI_CVD_prevalent_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = c(1065, 1074),
                                    colname = "VeI_NonCancer",
                                   instance = 0, 
                                   return_label = "duration",
                                 mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding6_noncancerVI.csv")),
      post_exclusion = FALSE,
      display_name = "Duration of CVD reported in verbal interview at baseline",
      description = "Time since CVD diagnosis"
    )
  )
}

TEU_HES_CVD_prevalent <- function(ICD10_codes, ICD9_codes) {
  list(
    list(
      name = "TEU_HES_CVD_prevalent_type",
      source = c("ID", "Rec_DateAssess",
                 paste0("HES_ICD10Diag.0.", seq(0, 65, by=1)),
                 paste0("HES_ICD9Diag.0.", seq(0, 27, by=1))),
      mapper = FN_HES_first(ICD10_codes = ICD10_codes,
                            ICD9_codes = ICD9_codes,
                            instance = 0,
                            return_label = "dx"),
      post_exclusion = FALSE,
      display_name = "Category of CVD recorded in HES data prior to baseline",
      description = "Type of CVD"
    ),
    list(
      name = "TEU_HES_CVD_prevalent_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("HES_ICD10Diag.0.", seq(0, 65, by=1)),
                 paste0("HES_ICD9Diag.0.", seq(0, 27, by=1))),
      mapper = FN_HES_first(ICD10_codes = ICD10_codes,
                            ICD9_codes = ICD9_codes,
                            instance = 0,
                            return_label = "duration"),
      post_exclusion = FALSE,
      display_name = "Duration of CVD recorded in HES data prior to baseline",
      description = "Time since CVD diagnosis"
    )
  )
}

ADO_DateFirstMI <- function() {
  list(
    name = "ADO_DateFirstMI", 
    source = c("ADO_DateFirstMI.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Date of first MI",
    description = paste0("Date of first myocardial infarction, algorithmically defined by UKB. See ",
                         text_spec("Resource 461", link = "https://biobank.ndph.ox.ac.uk/showcase/showcase/docs/alg_outcome_mi.pdf"),
                         " for more information. Note values of 1900-01-01 are unknown.")
  )
}

ADO_DateFirstIStroke <- function() {
  list(
    name = "ADO_DateFirstIStroke", 
    source = c("ADO_DateFirstIStroke.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Date of first ischaemic stroke",
    description = paste0("Date of first ischaemic stroke, algorithmically defined by UKB. See ",
                         text_spec("Resource 462", link = "https://biobank.ndph.ox.ac.uk/showcase/showcase/docs/alg_outcome_stroke.pdf"),
                         " for more information. Note values of 1900-01-01 are unknown.")
  )
}

TEU_VeI_CVD_operation <- function(dx_codes) {
  list(
    list(
      name = "TEU_VeI_CVD_operation_type",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_OperationCode.0.", seq(0, 31, by=1)),
                 paste0("VeI_OperationYear.0.", seq(0, 31, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                    colname = "VeI_Operation",
                                    instance = 0,
                                    return_label = "dx",
                                    mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding5_operationsVI.csv")
                                    ),
      post_exclusion = FALSE,
      display_name = "Category of CVD operation reported in verbal interview at baseline",
      description = "Type of CVD operation"
    ),
    list(
      name = "TEU_VeI_CVD_operation_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_OperationCode.0.", seq(0, 31, by=1)),
                 paste0("VeI_OperationYear.0.", seq(0, 31, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                    colname = "VeI_Operation",
                                    instance = 0, 
                                    return_label = "duration",
                                    mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding5_operationsVI.csv")
                                    ),
      post_exclusion = FALSE,
      display_name = "Duration of CVD operation reported in verbal interview at baseline",
      description = "Time since CVD operation"
    )
  )
}

# XL add: 19/11/2020
TEU_LDLctrl_v1 <- function(threshold=3) {
  list(
    name = "TEU_LDLctrl_v1", 
    source = c("BBC_LDL_Result.0.0"), 
    mapper = function(x) {
      y <- ifelse(x<threshold, 1, 0)
      y <- factor(as.numeric(y), levels=c(0,1), 
                  labels=c("Not controlled", "Controlled"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "LDL control status",
    description = paste0("Whether participant had LDL controlled (i.e. LDL<",threshold,"mmol/L at baseline)")
  )
}

# XL add: statin taken status and number of statin taken at baseline
TEU_VeI_statin <- function(){
  list(
    list(
      name = 'TEU_VeI_statin',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = c(1140861958, 1140888594, 1140888648,
                                                  1141146234, 1141192410),
                                    med_name = 'statin',
                                    colname = "VeI_Med", 
                                    instance = 0,
                                    return_label = 'statin',
                                    mapper = read.csv(paste0(config$cleaning$coding,'coding4.csv'))%>%
                                      rename(Code=value)%>%select(Code,meaning)),
      post_exclusion = FALSE,
      display_name = 'Statin taken status at baseline',
      description = 'Whether people self reported taking statin at baseline during verbal interview'
    ),
    list(
      name = 'TEU_VeI_statin_num',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = c(1140861958, 1140888594, 1140888648,
                                                  1141146234, 1141192410),
                                    med_name = 'statin',
                                    colname = "VeI_Med", 
                                    instance = 0,
                                    return_label = 'statin_num',
                                    mapper = read.csv(paste0(config$cleaning$coding,'coding4.csv'))%>%
                                      rename(Code=value)%>%select(Code,meaning)),
      post_exclusion = FALSE,
      display_name = 'Number of statin taken at baseline',
      description = 'Number of statin people self reported taking at baseline during verbal interview'
    )
  )
  
}

# XL add: serious comorbidities (exclusion criteria)
TEU_VeI_seriouscomb <- function(){
  list(
    name = 'TEU_VeI_seriouscomb',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = function(data){
      # read in finalised excel mapping
      noncancer=read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')
      # Coding list for serious comorbidities
      serious_comb<-noncancer[which(noncancer$Exclude=='Yes'),]$coding
      y<- FN_VI_filtercodes(dx_codes = serious_comb,
                        colname = "VeI_NonCancer",
                        instance = 0,
                        return_label = "dx",
                        mapper = read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding6_noncancerVI.csv"))(data)
      
      return(y)
    },      
    post_exclusion = FALSE,
    display_name = "Type of serious comorbidities",
    description = "Category of serious comorbidities reported in verbal interview at baseline"
  )
}

# XL add: cancer except skin cancer (exclusion criteria)
TEU_VeI_cancer<- function(){
  list(
    name = 'TEU_VeI_cancer',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_CancerCode.0.", c(0:5)),
               paste0("VeI_CancerYear.0.",c(0:5))),
    mapper =function(data){
      
      cancer_mapper=read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding3_flattened.csv")%>%
        rename(dx=meaning)
      cancer_codes=cancer_mapper[-which(cancer_mapper$meaning_TL%in% c('skin cancer')),]$Code
      
      y<- FN_VI_filtercodes(dx_codes = cancer_codes,
                            colname = "VeI_Cancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = cancer_mapper)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Type of cancer (except skin cancer)',
    description = 'Category of cancer (except skin cancer) reported in verbal interview at baseline'
  )
}


# XL add: each comorbidity condition
## CVD
TEU_VeI_CVD <- function(condition='CVD'){
  list(
    name = 'TEU_VeI_CVD',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
  ,
  post_exclusion=FALSE,
  display_name = 'CVD status at baseline (VI)',
  description = 'Whether participant self reported CVD at baseline in verbal interview'
  )
}


## diabetes
TEU_VeI_diab <- function(condition='diabetes'){
  list(
    name = 'TEU_VeI_diab',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'diabetes status at baseline (VI)',
    description = 'Whether participant self reported CVD at baseline in verbal interview'
  )
}

## afib or aflutter
TEU_VeI_arrhy <- function(condition="afib or aflutter"){
  list(
    name = 'TEU_VeI_arrhy',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Arrhythmia status at baseline (VI)',
    description = 'Whether participant self reported Arrhythmia (afib/flutter) at baseline in verbal interview'
  )
}

## Osteoarthritis
TEU_VeI_osteo <- function(condition="Osteoarthritis"){
  list(
    name = 'TEU_VeI_osteo',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Osteoarthritis status at baseline (VI)',
    description = 'Whether participant self reported Osteoarthritis at baseline in verbal interview'
  )
}



## Other joint disorder
TEU_VeI_joint <- function(condition="Other joint disorder"){
  list(
    name = 'TEU_VeI_joint',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Other joint disorder status at baseline (VI)',
    description = 'Whether participant self reported other joint disorder at baseline in verbal interview'
  )
}


## Epilepsy
TEU_VeI_epil <- function(condition="epilepsy"){
  list(
    name = 'TEU_VeI_epil',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Epilepsy status at baseline (VI)',
    description = 'Whether participant self reported epilepsy at baseline in verbal interview'
  )
}

## Migraine
TEU_VeI_mig <- function(condition="migraine"){
  list(
    name = 'TEU_VeI_mig',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Migraine status at baseline (VI)',
    description = 'Whether participant self reported migraine at baseline in verbal interview'
  )
}


## Anxiety or stress
TEU_VeI_anx <-function(condition="anxiety or stress"){
  list(
    name = 'TEU_VeI_anx',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Anxiety or stress status at baseline (VI)',
    description = 'Whether participant self reported anxiety or stress at baseline in verbal interview'
  )
}

## depression or bipolar
TEU_VeI_dep <-function(condition="depression or bipolar"){
  list(
    name = 'TEU_VeI_dep',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Depression or bipolar status at baseline (VI)',
    description = 'Whether participant self reported depression or bipolar at baseline in verbal interview'
  )
}


## asthma or COPD
TEU_VeI_asthCOPD <- function(condition="asthma or COPD"){
  list(
    name = 'TEU_VeI_asthCOPD',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read_excel('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\UKBHtn_NonCancerIllness_Mapping.xlsx')%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Asthma or COPD status at baseline (VI)',
    description = 'Whether participant self reported asthma or COPD at baseline in verbal interview'
  )
}


# XL add: Number of comorbidities
HTN_comorb_num<- function(){
  list(
    name = 'HTN_comorb_num',
    source = c('TEU_VeI_CVD','TEU_VeI_diab','TEU_VeI_arrhy','TEU_VeI_asthCOPD','TEU_VeI_mig','TEU_VeI_epil',
               'TEU_VeI_anx','TEU_VeI_dep','TEU_VeI_osteo','TEU_VeI_joint'),
    mapper = function(data){
      rowSums(sapply(select(data,everything()),function(x) grepl("Yes",x)))
    },
    post_exclusion = FALSE,
    display_name = 'Number of comorbidities',
    description = 'Number of comorbidities self-reported at baseline'
  )
}

HTN_comorb_numcat<- function(){
  list(
    name = 'HTN_comorb_numcat',
    source = c('HTN_comorb_num'),
    mapper = function(x){
      factor(ifelse(x>=3,'>=3',x),levels = c('0','1','2','>=3'),ordered = FALSE)
    },
    post_exclusion = FALSE,
    display_name = 'Number of comorbidities (Categorical)',
    description = 'Number of comorbidities participants self-reported at baseline'
  )
}

# From JC: Job variable 
TEU_Emp_CurrStat <- function() {
  list(
    name = "TEU_Emp_CurrStat",
    source = c("Emp_CurrStatUnc.0.0", "Emp_CurrStat.0.0"),
    mapper = function(data) {
      # XL change: Changed the order of uncorrected and corrected
      y <- factor(coalesce(as.character(data[["Emp_CurrStat.0.0"]]), 
                           as.character(data[["Emp_CurrStatUnc.0.0"]])),
                  ordered=FALSE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Employment status",
    description = "Employment status at baseline, derived by taking the employment status from TQ and applying corrections made by UKB in light of participant jobs self-reported in VI"
  )
}

TEU_Emp_JobCode_v2 <- function() {
  list(
    name = "Emp_JobCode.0.0",
    source = "ID",
    mapper = function(x) {
      v2_emp <- DBfunc$DB_extract(extract_cols = c("ID", "Emp_JobCode.0.0"),
                                  db = "K:/TEU/UKB33952_Data/Data_Downloads/V2_database_duckdb0.2.1/ukb_v2.db",
                                  name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")
      y <- v2_emp[["Emp_JobCode.0.0"]][match(x, v2_emp$ID)]
      y <- as.numeric(y)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Job code",
    description = "Job code of participant at baseline self-reported in verbal interview"
  )
}

TEU_HTN_Emp_category <- function() {
  list(
    name = "TEU_Emp_category",
    source = c("Emp_JobCode.0.0", "TEU_Emp_CurrStat"),
    mapper = function(data) {
      map <- read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding2_flattened.csv")
      data <- left_join(data, map[,c("Code", "meaning_TL")], by=c("Emp_JobCode.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["meaning_TL"]]), 
                                  as.character(data[["TEU_Emp_CurrStat"]]))
      
      y <- dplyr::case_when(
        is.na(data$TEU_EmpCat) ~ "Unemployed/unanswered",
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
      y <- factor(y, 
                  levels=c("White collar", "Skilled trades", "Services", 
                           "Blue collar", "Other employment", "Retired", 
                           "Disability", "Unemployed/unanswered"), 
                  labels=c("Professional and Administrative", "Skilled trades", "Services", 
                           "Manual and Industrial", "Other employment", "Retired", 
                           "Unable to work because of sickness or disability", 
                           "Unemployed/unanswered"),
                  ordered=FALSE)
      
    },
    post_exclusion = FALSE,
    display_name = "Employment category",
    description = "Employment category at baseline, collapsed from job codes self-reported in verbal interview"
  )
}

# XL add: Same as above except leave NA as it is and leave prefer not to answer as it is
Emp_category <- function() {
  list(
    name = "Emp_category",
    source = c("Emp_JobCode.0.0", "TEU_Emp_CurrStat"),
    mapper = function(data) {
      map <- read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding2_flattened.csv")
      data <- left_join(data, map[,c("Code", "meaning_TL")], by=c("Emp_JobCode.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["meaning_TL"]]), 
                                  as.character(data[["TEU_Emp_CurrStat"]]))
      
      y <- dplyr::case_when(
        is.na(data$TEU_EmpCat) ~ NA_character_,
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
                               "None of the above") ~ "Unemployed",
        data$TEU_EmpCat == "Prefer not to answer" ~ "Prefer not to answer",
        TRUE ~ "Error?"
      )
      y <- factor(y, 
                  levels=c("White collar", "Skilled trades", "Services", 
                           "Blue collar", "Other employment", "Retired", 
                           "Disability", "Unemployed","Prefer not to answer"), 
                  labels=c("Professional and Administrative", "Skilled trades", "Services", 
                           "Manual and Industrial", "Other employment", "Retired", 
                           "Unable to work because of sickness or disability", 
                           "Unemployed", "Prefer not to answer"),
                  ordered=FALSE)
      
    },
    post_exclusion = FALSE,
    display_name = "Employment category",
    description = "Employment category at baseline, collapsed from job codes self-reported in verbal interview"
  )
}

# XL add: Country of birth (by income)
TEU_CountryIncome=function(){
  list(
    name = 'TEU_CountryIncome',
    source = c('ELF_BirthCountry.0.0','VeI_BirthCountry.0.0'),
    mapper = function(data){
      
      # Generate mapping first 
      UKB_mapper=read.csv(file.path(config$cleaning$coding,'coding89_flattened.csv'))
      Income_mapper=read_xlsx('K:\\TEU\\TEU_Members\\Xiaonan_Liu\\Projects\\PRSonCholstrl\\Data\\Mappings\\CountryIncomeCategory.xlsx',sheet = 'UKB_CountryNames')
      
      mapper=left_join(UKB_mapper[,c('Code','meaning')],Income_mapper[,c('coding','IncomeLevel')],by=c('Code'='coding'))%>%
        mutate(TEU_Incomelevel=case_when(meaning=='United Kingdom' ~ 'UK',
                                         IncomeLevel=='H' ~ 'Other high income',
                                         IncomeLevel %in% c('UM','LM') ~ 'Middle income',
                                         IncomeLevel=='L' ~ 'Low income'))
      
      # Merge with data
      data=data%>%
        rename(TQ=ELF_BirthCountry.0.0,VI=VeI_BirthCountry.0.0)%>%
        mutate(VI=as.numeric(VI))%>%
        left_join(.,mapper,by=c('VI'='Code'))%>%
        # create column to derive Country of birth (by income)
        mutate(CountryIncome=case_when(!is.na(VI) ~ TEU_Incomelevel,
                                       is.na(VI) & TQ %in% c('England','Wales','Scotland','Northern Ireland') ~ 'UK',
                                       is.na(VI) & TQ=='Republic of Ireland' ~ mapper[which(mapper$meaning=='Ireland'),]$TEU_Incomelevel,
                                       is.na(VI) & TQ %in% c('Elsewhere','Prefer not to answer','Do not know') ~ 'Unknown',
                                       is.na(VI) & is.na(TQ) ~ 'Unknown',
                                       TRUE ~ 'Error?'))
      
      y<-factor(data[['CountryIncome']],levels = c('UK','Other high income','Middle income','Low income','Unknown'),
                labels = c('UK','Other high income','Middle income','Low income','Unknown'),ordered = FALSE)
      
      return(y)
      
      
    },
    post_exclusion = FALSE,
    display_name = 'Country of birth, by income level',
    description = 'Categorise self-reported country of birth from touchscreen and verbal interview by income level'
  )
}


