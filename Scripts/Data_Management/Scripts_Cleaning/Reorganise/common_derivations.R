# Jennifer Collister
# 22/09/20

library(glue)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Source the function definitions
source(file.path(config$scripts$cleaning, "Reorganise", "basic_functions.R"),
       local = TRUE)

makeEnum <- function(inputList) {
  # Borrowed from https://stackoverflow.com/a/41509345
  myEnum <- as.list(inputList)
  enumNames <- names(myEnum)
  if (is.null(enumNames)) {
    names(myEnum) <- myEnum
  } else if ("" %in% enumNames) {
    stop("The inputList has some but not all names assigned. They must be all assigned or none assigned")
  }
  return(myEnum)
}
visits <- makeEnum(list(baseline = c("0", "baseline assessment"), 
                    repeat_visit = c("1", "repeat visit"), 
                    imaging = c("2", "imaging visit"), 
                    repeat_imaging = c("3","repeat imaging visit")))

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
    name = "Rec_DateAssess.0.0",
    source = c("Rec_DateAssess.0.0"),
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = "DateBaselineAssess",
    description = "Date of baseline assessment"
  )
}

Eth_ethnicity <- function() {
  list(
    name = "Eth_ethnicity.0.0",
    source = "Eth_ethnicity.0.0",
    mapper = FN_reorderfactor(
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
    description = "The participant's self-reported ethnicity"
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
    source = c("TEU_BaC_DateOfBirth", "Rec_DateAssess.0.0"),
    mapper = function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess.0.0"]], data[["TEU_BaC_DateOfBirth"]], unit =
                                  "days") / 365.25,
                       digits = 2))
    },
    post_exclusion = FALSE,
    display_name = "AgeAtRecruitment",
    description = "The participant's approximate age at recruitment"
  )
}

TEU_BlP_SBP.0 <- function() {
  list(
    name = "TEU_BlP_SBP.0",
    source = c("BlP_SBPAuto.0", "BlP_SBPMan.0"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0"]], data[["BlP_SBPMan.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First SBP at baseline",
    description = "First SBP measurement at baseline, automated or manual"
  )
}

TEU_BlP_SBP.1 <- function() {
  list(
    name = "TEU_BlP_SBP.1",
    source = c("BlP_SBPAuto.1", "BlP_SBPMan.1"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.1"]], data[["BlP_SBPMan.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second SBP at baseline",
    description = "Second SBP measurement at baseline, automated or manual"
  )
}

TEU_BlP_DBP.0 <- function() {
  list(
    name = "TEU_BlP_DBP.0",
    source = c("BlP_DBPAuto.0", "BlP_DBPMan.0"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0"]], data[["BlP_DBPMan.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First DBP at baseline",
    description = "First DBP measurement at baseline, automated or manual"
  )
}

TEU_BlP_DBP.1 <- function() {
  list(
    name = "TEU_BlP_DBP.1",
    source = c("BlP_DBPAuto.1", "BlP_DBPMan.1"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.1"]], data[["BlP_DBPMan.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second DBP at baseline",
    description = "Second DBP measurement at baseline, automated or manual"
  )
}

TEU_BlP_nSBP <- function() {
  list(
    name = "TEU_BlP_nSBP",
    source = c("TEU_BlP_SBP.0", "TEU_BlP_SBP.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0", "TEU_BlP_SBP.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. SBP",
    description = "Number of SBP measurements taken at baseline"
  )
}

TEU_BlP_nDBP <- function() {
  list(
    name = "TEU_BlP_nDBP",
    source = c("TEU_BlP_DBP.0", "TEU_BlP_DBP.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_DBP.0", "TEU_BlP_DBP.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. DBP",
    description = "Number of DBP measurements taken at baseline"
  )
}

TEU_BlP_SBP.avg <- function() {
  list(
    name = "TEU_BlP_SBP.avg",
    source = c("TEU_BlP_SBP.0",
               "TEU_BlP_SBP.1"),
    mapper = FN_average(colnames = c("TEU_BlP_SBP.0",
                                     "TEU_BlP_SBP.1")),
    post_exclusion = FALSE,
    display_name = "SBP",
    description = "The average systolic blood pressure from two measurements at baseline"
  )
}

TEU_BlP_DBP.avg <- function() {
  list(
    name = "TEU_BlP_DBP.avg",
    source = c("TEU_BlP_DBP.0",
               "TEU_BlP_DBP.1"),
    mapper = FN_average(colnames = c("TEU_BlP_DBP.0",
                                     "TEU_BlP_DBP.1")),
    post_exclusion = FALSE,
    display_name = "DBP",
    description = "The average diastolic blood pressure from two measurements at baseline"
  )
}

TEU_BlP_measuredHTN <- function(SBPthreshold = 140,
                                DBPthreshold = 90) {
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
    source = "Alc_Status",
    mapper = FN_reorderfactor(
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
    source = "Smo_Status",
    mapper = FN_reorderfactor(
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
    source = c("HoH_PreTaxInc.0", "HoH_PreTaxInc_P.0"),
    mapper = function(data) {
      y <- ifelse(is.na(data[["HoH_PreTaxInc.0"]]),
                  as.character(data[["HoH_PreTaxInc_P.0"]]),
                  as.character(data[["HoH_PreTaxInc.0"]]))
      y <- fct_collapse(
        y,
        "Less than 18,000" = "Less than 18,000",
        "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
        "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
        "52,000 to 100,000" = "52,000 to 100,000",
        "Greater than 100,000" = "Greater than 100,000",
        "Do not know" = "Do not know",
        "Prefer not to know" = "Prefer not to know"
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
    source = "Sle_Duration",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "SleepDuration_h",
    description = "Participant's self-reported average sleep duration in hours"
  )
}

TEU_BSM_BMIcat <- function() {
  list(
    name = "TEU_BSM_BMIcat",
    source = "BSM_BMI",
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
    description = "BMI category"
  )
}

TEU_BSM_WaistCircCat <- function() {
  list(
    name = "TEU_BSM_WaistCircCat",
    source = c("BSM_Waist", "BaC_Sex"),
    mapper = function(data) {
      # Categorise waist circ into labelled categories
      y <- dplyr::case_when(
        data[["BaC_Sex"]] == "Female" & data[["BSM_Waist"]] >= 88 ~ "Obese",
        data[["BaC_Sex"]] == "Female" &
          data[["BSM_Waist"]] ~ "Overweight",
        data[["BaC_Sex"]] == "Male" & data[["BSM_Waist"]] ~ "Obese",
        data[["BaC_Sex"]] == "Male" &
          data[["BSM_Waist"]] ~ "Overweight",
        is.na(data[["BSM_Waist"]]) ~ "Unknown",
        TRUE ~ "Normal"
      )
      y <-
        factor(y, levels = c("Normal", "Overweight", "Obese", "Unknown"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "WaistCirc",
    description = "Categorised waist circumference"
  )
}

PhA_METsWkAllAct <- function() {
  list(
    name = "PhA_METsWkAllAct",
    source = "PhA_METsWkAllAct",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "WeeklyMETs",
    description = "Average weekly METs, derived from participant self-reported weekly exercise"
  )
}

CoF_RTTTimeID <- function() {
  list(
    name = "CoF_RTTTimeID",
    source = "CoF_RTTTimeID",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ReactionTime",
    description = "Reaction time in a game of snap, in seconds"
  )
}

TEU_Edu_HighestQual <- function() {
  list(
    name = "TEU_Edu_HighestQual",
    source = c(
      "Edu_Qualif.0",
      "Edu_Qualif.1",
      "Edu_Qualif.2",
      "Edu_Qualif.3",
      "Edu_Qualif.4",
      "Edu_Qualif.5"
    ),
    mapper = function(data) {
      y <- rep(NA, nrow(data))
      for (qual in c(
        "College or University degree",
        "NVQ or HND or HNC or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "A levels/AS levels or equivalent",
        "O levels/GCSEs or equivalent",
        "CSEs or equivalent",
        "None of the above"
      )) {
        y[is.na(y)] <-
          ifelse(apply(data[is.na(y), grep("Edu_Qualif", colnames(data), fixed =
                                             TRUE)], 1,
                       function(x)
                         any(x == qual & !is.na(x))),
                 qual,
                 NA)
      }
      y <-
        factor(
          y,
          levels = c(
            "College or University degree",
            "NVQ or HND or HNC or equivalent",
            "Other professional qualifications eg: nursing, teaching",
            "A levels/AS levels or equivalent",
            "O levels/GCSEs or equivalent",
            "CSEs or equivalent",
            "None of the above"
          )
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
    source = "GAC_AideMem",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "AideMemoir",
    description = "Did the participant bring the requested aide-memoir with a note of their medications and previous operations?"
  )
}

TEU_Rec_AssessCentre <- function() {
  list(
    name = "TEU_Rec_AssessCentre",
    source = "Rec_AssessCentre",
    mapper = function(x) {
      map <-
        read.table(
          "K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\coding10.tsv",
          sep = "\t",
          header = TRUE,
          quote = "",
          comment.char = "$",
          fill = FALSE
        )
      y <- merge(x,
                 map,
                 by.x = "x",
                 by.y = "coding",
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
    source = "Rec_AssessCentre",
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
        TRUE ~ "Other"
      )
      if ("Other" %in% y) {
        warning("Unrecognised centre code")
      }
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "CountryResidence",
    description = "Which country does the participant live in"
  )
}

TEU_TownsendDepInd_Quint <- function() {
  list(
    name = "TEU_TownsendDepInd_Quint",
    source = c("BaC_DeprivInd"),
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

TEU_HMH_Med.any <- function() {
  list(
    name = "TEU_HMH_Med.any",
    source = c(
      paste0("HMH_MedCholBPDiabHorm.", c(0:3)),
      paste0("HMH_MedCholBPDiab.", c(0:2))
    ),
    mapper = function(data) {
      # Combine the first medication field across males and females
      medcombine <-
        coalesce(data[["HMH_MedCholBPDiabHorm.0"]], data[["HMH_MedCholBPDiab.0"]])
      # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
      medlist <-
        c(
          "Cholesterol lowering medication",
          "Blood pressure medication",
          "Oral contraceptive pill or minipill",
          "Hormone replacement therapy",
          "Insulin"
        )
      y <- dplyr::case_when(
        medcombine == "None of the above" ~ "No",
        medcombine == "Do not know" ~ "Do not know",
        medcombine == "Prefer not to answer" ~ "Prefer not to answer",
        is.na(med.combine) ~ "NA",
        medcombine %in% medlist ~ "Yes",
        TRUE ~ "Unexpected answer"
      )
      y <-
        factor(y,
               levels = c("Yes", "No", "Do not know", "Prefer not to answer", "NA"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "HMH_Med",
    description = "Did the participant self-report taking medication for cholesterol, blood pressure, diabetes or HRT?"
  )
}

TEU_HMH_Med.BP <- function() {
  list(
    name = "TEU_HMH_Med.BP",
    source = c(
      paste0("HMH_MedCholBPDiabHorm.", c(0:3)),
      paste0("HMH_MedCholBPDiab.", c(0:2))
    ),
    mapping = FN_HMHmeds(medtype = "Blood pressure medication", string =
                           "BP meds"),
    post_exclusion = FALSE,
    display_name = "HBPmeds",
    description = "Participant self-reported taking BP medication in the touchscreen questionnaire"
  )
}

TEU_ethnicgrp <- function() {
  list(
    name = "TEU_ethnicgrp",
    source = "Eth_Ethnicity",
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
    description = "The participant's self-reported ethnicity"
  )
}

TEU_Alc_Status <- function() {
  list(
    name = "TEU_Alc_Status",
    source = "Alc_Status",
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
    source = "Smo_Status",
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
      "Alc_RedWineWk",
      "Alc_WhiteWineWk",
      "Alc_BeerCiderWk",
      "Alc_SpiritsWk",
      "Alc_FortWineWk",
      "Alc_OtherAlcWk"
    ),
    mapper = function(data) {
      alcservings <- list()
      for (alc in c(
        "Alc_RedWineWk",
        "Alc_WhiteWineWk",
        "Alc_BeerCiderWk",
        "Alc_SpiritsWk",
        "Alc_FortWineWk",
        "Alc_OtherAlcWk"
      )) {
        alcservings[[alc]][data[[alc]] < 0 | is.na(data[[alc]])] <-  0
      }
      
      weekly_alcunits <-
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
      
      # Truncate alcohol consumption at upper 95th percentile
      upper95 <- quantile(weeklyunits, 0.95, na.rm = TRUE)
      weeklyunits[weeklyunits > upper95] <- upper95
      weeklyunits[is.na(weeklyunits)] <- 0
      
      return(weeklyunits)
    },
    post_exclusion = FALSE,
    display_name = "WeeklyAlcUnits",
    description = "Total weekly units of alcohol, derived from self-reported average weekly consumption of each different type alcohol"
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
    description = "Does the patient's self-reported weekly alcohol consumption exceed the threshold for binge drinking"
  )
}

TEU_Pha_METsover1200 <- function() {
  list(
    name = "TEU_Pha_METsover1200",
    source = "PhA_METsWkAllAct",
    mapper = function(x) {
      y <- dplyr::case_when(
        is.na(x) ~ "Unknown",
        x > 1200 ~ "High (METs > 1200)",
        x <= 1200 ~ "Low (METs <= 1200)",
        TRUE ~ "Other"
      )
      y <-
        factor(y,
               levels = c("High (METs > 1200)", "Low (METs <= 1200)", "Unanswered"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Sufficient_METs",
    description = "Indicates whether the participant's average daily METs exceed 150"
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
    display_name = "",
    description = ""
  )
}

TEU_HMH_BowelCancerScreen <- function() {
  list(
    name = "TEU_HMH_BowelCancerScreen",
    source = "HMH_BowelSc",
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
      paste0("FaH_FatherIll.", c(0:9)),
      paste0("FaH_MotherIll.", c(0:10)),
      paste0("FaH_SibIll.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Heart disease", "High blood pressure", "Stroke"),
      label = "CVD"
    ),
    post_exclusion = FALSE,
    display_name = "FamilyHistoryCVD",
    description = "Family history of CVD (Heart disease, high blood pressure, stroke)"
  )
}
