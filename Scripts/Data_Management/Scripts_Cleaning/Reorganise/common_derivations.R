# Jennifer Collister
# 22/09/20

library(yaml)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")

source(file.path(config$scripts$cleaning, "Reorganise", "basic_functions.R"))

# Formatting of existing UKB variables

common <- list(

  list(name="ID",
             source="ID",
             mapper=FN_id,
             post_exclusion=FALSE,
             display_name="ID",
             description="The unique participant identifier"
             ),
  
  list(
    name="BaC_Sex", 
    source="BaC_Sex", 
    mapper=FN_unorder,
    post_exclusion=FALSE, 
    display_name="gender", 
    description="Participant's self-reported gender"
  ),
  
  
  list(name="Eth_ethnicity",
                    source="Eth_ethnicity",
                    mapper=FN_reorderfactor(levelorder=c("White", "British", "Irish", "Any other white background",
                                                     "Mixed", "White and Black Caribbean", "White and Black African",
                                                     "White and Asian", "Any other mixed background",
                                                     "Asian or Asian British", "Indian", "Pakistani", "Bangladeshi",
                                                     "Any other Asian background",
                                                     "Black or Black British", "Caribbean", "African", "Any other Black background",
                                                     "Chinese", "Other ethnic group", "Do not know", "Prefer not to answer")),
                    post_exclusion=FALSE,
                    display_name="ethnicity",
                    description="The participant's self-reported ethnicity"
                    ),
  
  
  list(name="BaC_RsnLostFU",
                    source="BaC_RsnLostFU",
                    mapper=FN_unorder,
                    post_exclusion=FALSE,
                    display_name="lfu_reason",
                    description="The reported reason for loss to follow-up"
                    ),
  
  list(name="TEU_BaC_DateOfBirth",
              source=c("BaC_BirthMonth", "BaC_BirthYear"),
              mapper=FN_MYtoDate(day=15, monthField="BaC_BirthMonth", yearField="BaC_BirthYear"),
              post_exclusion=FALSE,
              display_name="DateOfBirth",
              description="The patient's approximate date of birth, derived from self-reported month and year with date assumed to be 15th"
              ),
  
  list(name="TEU_BlP_SBP.avg",
              source=c("BlP_SBPAuto.0", "BlP_SBPAuto.1", "BlP_SBPMan.0", "BlP_SBPMan.1"),
              mapper=FN_average(colnames=c("BlP_SBPAuto.0", "BlP_SBPAuto.1", "BlP_SBPMan.0", "BlP_SBPMan.1")),
              post_exclusion=FALSE,
              display_name="SBP",
              description="The average systolic blood pressure from two measurements at baseline"
              ),
  
  list(name="TEU_BlP_DBP.avg",
              source=c("BlP_DBPAuto.0", "BlP_DBPAuto.1", "BlP_DBPMan.0", "BlP_DBPMan.1"),
              mapper=FN_average(colnames=c("BlP_DBPAuto.0", "BlP_DBPAuto.1", "BlP_DBPMan.0", "BlP_DBPMan.1")),
              post_exclusion=FALSE,
              display_name="DBP",
              description="The average diastolic blood pressure from two measurements at baseline"
  ),
  
  list(name="TEU_BlP_measuredHTN",
                      source=c("TEU_SBP.avg", "TEU_DBP.avg"),
                      mapper=function(data){data[["TEU_SBP.avg"]]>=140 | data[["TEU_DBP.avg"]]>=90},
                      post_exclusion=FALSE,
                      display_name="measuredHTN",
                      description="Whether the participant had hypertensive BP measured at baseline"
                      ),
  
  list(
    name="Alc_Status", 
    source="Alc_Status", 
    mapper=FN_reorderfactor(levelorder=c("Never", "Previous", "Current", "Prefer not to answer")),
    post_exclusion=FALSE, 
    display_name="Alc_Status", 
    description="Self-reported alcohol status"
  ),
  
  list(
    name="Smo_Status", 
    source="Smo_Status", 
    mapper=FN_reorderfactor(levelorder=c("Never", "Previous", "Current", "Prefer not to answer")),
    post_exclusion=FALSE, 
    display_name="Smo_Status", 
    description="Self-reported smoking status"
  ),
  
  
  list(
    name="TEU_HoH_PreTaxInc", 
    source=c("HoH_PreTaxInc.0", "HoH_PreTaxInc_P.0"), 
    mapper=function(data){
      y <- ifelse(is.na(data[["HoH_PreTaxInc.0"]]), 
                  as.character(data[["HoH_PreTaxInc_P.0"]]),
                  as.character(data[["HoH_PreTaxInc.0"]]))
      y <- fct_collapse(y, 
                        "Less than 18,000" = "Less than 18,000",
                        "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
                        "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
                        "52,000 to 100,000" = "52,000 to 100,000",
                        "Greater than 100,000" = "Greater than 100,000",
                        "Do not know" = "Do not know",
                        "Prefer not to know" = "Prefer not to know"
        )
      y <- factor(y, levels=c("Less than 18,000", "18,000 to 30,999",
                                               "31,000 to 51,999", "52,000 to 100,000", "Greater than 100,000", 
                                               "Do not know", "Prefer not to answer"))
      return(y)
    }, 
    post_exclusion=FALSE,
    display_name="HouseholdIncome", 
    description="Participant's pre-tax household income"
  ),
  
  list(
    name="Sle_Duration", 
    source="Sle_Duration", 
    mapper=FN_id, 
    post_exclusion=FALSE,
    display_name="SleepDuration_h", 
    description="Participant's self-reported average sleep duration in hours"
  ),
  
  list(
    name="TEU_BSM_BMIcat", 
    source="BSM_BMI", 
    mapper=function(x){
      y <- as.character(cut(x, breaks=c(0, 18.5, 25, 30, 200), right=FALSE))
      y[is.na(y)] <- "Unknown"
      y <- factor(y, levels=c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)", "Unknown"), 
                            labels=c("Normal", "Underweight", "Overweight", "Obese", "Unknown"))
      return(y)
    }, 
    post_exclusion=FALSE,
    display_name="BMIcat", 
    description="BMI category"
  ),
  
  list(
    name="TEU_BSM_WaistCircCat", 
    source=c("BSM_Waist", "BaC_Sex"), 
    mapper=function(data){
      # Categorise waist circ into labelled categories
      y <- dplyr::case_when(
        data[["BaC_Sex"]]=="Female" & data[["BSM_Waist"]]>=88 ~ "Obese",
        data[["BaC_Sex"]]=="Female" & data[["BSM_Waist"]] ~ "Overweight",
        data[["BaC_Sex"]]=="Male" & data[["BSM_Waist"]] ~ "Obese",
        data[["BaC_Sex"]]=="Male" & data[["BSM_Waist"]] ~ "Overweight",
        is.na(data[["BSM_Waist"]]) ~ "Unknown",
        TRUE ~ "Normal"
      )
      y <- factor(y, levels=c("Normal", "Overweight", "Obese", "Unknown"))
      return(y)
    }, 
    post_exclusion=FALSE,
    display_name="WaistCirc", 
    description="Categorised waist circumference"
  ),
  
  list(
    name="PhA_METsWkAllAct", 
    source="PhA_METsWkAllAct", 
    mapper=FN_id, 
    post_exclusion=FALSE,
    display_name="WeeklyMETs", 
    description="Average weekly METs, derived from participant self-reported weekly exercise"
  ),
  
  list(
    name="CoF_RTTTimeID", 
    source="CoF_RTTTimeID", 
    mapper=FN_id, 
    post_exclusion=FALSE,
    display_name="ReactionTime", 
    description="Reaction time in a game of snap, in seconds"
  ),
  
  list(
    name="TEU_Edu_HighestQual", 
    source=c("Edu_Qualif.0", "Edu_Qualif.1", "Edu_Qualif.2", "Edu_Qualif.3", "Edu_Qualif.4", "Edu_Qualif.5"), 
    mapper=function(data){
      y <- rep(NA, nrow(data))
      for(qual in c("College or University degree",
                    "NVQ or HND or HNC or equivalent",
                    "Other professional qualifications eg: nursing, teaching",
                    "A levels/AS levels or equivalent",
                    "O levels/GCSEs or equivalent",
                    "CSEs or equivalent",
                    "None of the above")) {
        y[is.na(y)] <- ifelse(
          apply(data[is.na(y),grep("Edu_Qualif", colnames(data), fixed=TRUE)], 1, function(x) any(x == qual & !is.na(x))), 
          qual, 
          NA
        )
      }
      y <- factor(y, levels=c("College or University degree", "NVQ or HND or HNC or equivalent", 
                              "Other professional qualifications eg: nursing, teaching", 
                              "A levels/AS levels or equivalent", "O levels/GCSEs or equivalent", 
                              "CSEs or equivalent", "None of the above"))
      return(y)
    }, 
    post_exclusion=FALSE,
    display_name="HighestQualification", 
    description="Highest of a participant's self-reported educational qualifications"
  ),
  
  list(
    name="GAC_AideMem",
    source="GAC_AideMem", 
    mapper=FN_unorder,
    post_exclusion=FALSE,
    display_name="AideMemoir",
    description="Did the participant bring the requested aide-memoir with a note of their medications and previous operations?"
  ),
  
  list(
    name="TEU_Rec_AssessCentre", 
    source="Rec_AssessCentre", 
    mapper=function(x){
      map <- read.table("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\coding10.tsv",
                        sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)
      y <- merge(x, map, by.x="x", by.y="coding", all.x=TRUE)
      y <- y[["meaning"]]
    }, 
    post_exclusion=FALSE,
    display_name="AssessCentre", 
    description="Which assessment centre did the participant attend"
  ),
  
  
  list(
    name="TEU_Rec_Country", 
    source="Rec_AssessCentre", 
    mapper=function(x){
      y <- dplyr::case_when(
        x %in% c(10003, 11001, 11002, 11006, 11007, 11008, 11009, 
                 11010, 11011, 11012, 11013, 11014, 11016, 11017, 
                 11018, 11020, 11021, 11024, 11025, 11026, 11027, 11028) ~ "England",
        x %in% c(11004, 11005) ~ "Scotland",
        x %in% c(11003, 11022, 11023) ~ "Wales",
        TRUE ~ "Other"
      )
      if("Other" %in% y){warning("Unrecognised centre code")}
      return(y)
    }, 
    post_exclusion=FALSE,
    display_name="CountryResidence", 
    description="Which country does the participant live in"
  )

)

names(common) <- sapply(common, function(x) x$name)

