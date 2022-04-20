# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


ID <- function(x){x}


BaC_Sex <- function(x){factor(x, ordered=FALSE)}


TEU_BaC_DateOfBirth <- function(data){
    as.Date(paste0(as.character(day), as.character(data[[monthField]]), as.character(data[[yearField]])), format)
  }


Rec_DateAssess <- function(x){
  as.Date(x, origin=as.Date("1970-01-01"))
}


TEU_BaC_AgeAtRec <- function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess"]], data[["TEU_BaC_DateOfBirth"]], unit =
                                  "days") / 365.25,
                       digits = 2))
    }


TEU_ethnicgrp <- function(x) {
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
    }


TEU_Rec_AssessCentre <- function(x) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding10_AssessmentCentre.csv"))
      # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=10
      y <- merge(x,
                 map,
                 by.x = "x",
                 by.y = "Code",
                 all.x = TRUE,
                 sort = FALSE)
      y <- y[["meaning"]]
    }


TEU_Rec_Country <- function(x) {
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
    }


TEU_BlP_SBP.0.0 <- function(data) {
      coalesce(data[["BlP_SBPAuto.0.0"]], data[["BlP_SBPMan.0.0"]])
    }


TEU_BlP_SBP.0.1 <- function(data) {
      coalesce(data[["BlP_SBPAuto.0.1"]], data[["BlP_SBPMan.0.1"]])
    }


TEU_BlP_DBP.0.0 <- function(data) {
      coalesce(data[["BlP_DBPAuto.0.0"]], data[["BlP_DBPMan.0.0"]])
    }


TEU_BlP_DBP.0.1 <- function(data) {
      coalesce(data[["BlP_DBPAuto.0.1"]], data[["BlP_DBPMan.0.1"]])
    }


TEU_BlP_nSBP <- function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1")]))
    }


TEU_BlP_nDBP <- function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1")]))
    }


TEU_BlP_SBP.avg <- function(data){
    rowMeans(data[,colnames], na.rm)
  }


TEU_BlP_DBP.avg <- function(data){
    rowMeans(data[,colnames], na.rm)
  }


TEU_VeI_HTN <- function(data) {

    mapper_file <- read.csv_kdrive(mapper)
    
    long_dx <- evalWithMemoization(
      FN_VItoLong(
        data,
        colname = colname,
        instance = instance,
        mapper = mapper
      ) %>%
        filter(Code %in% dx_codes) %>%
        group_by(ID) %>%
        arrange(Year) %>%
        slice_head %>%
        mutate(dx = factor(meaning),
               Year = ifelse(Year %in% c(-1, -3), NA, Year),
               Year_date = date_decimal(Year),
               duration = as.numeric(round(
                 difftime(Rec_DateAssess, Year_date, unit = "days") / 365.25,
                 digits = 2
               ))),
      key = c(dx_codes, instance, mapper_file)
    )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    return(y)
  }


TEU_VeI_HTN_dur <- function(data) {

    mapper_file <- read.csv_kdrive(mapper)
    
    long_dx <- evalWithMemoization(
      FN_VItoLong(
        data,
        colname = colname,
        instance = instance,
        mapper = mapper
      ) %>%
        filter(Code %in% dx_codes) %>%
        group_by(ID) %>%
        arrange(Year) %>%
        slice_head %>%
        mutate(dx = factor(meaning),
               Year = ifelse(Year %in% c(-1, -3), NA, Year),
               Year_date = date_decimal(Year),
               duration = as.numeric(round(
                 difftime(Rec_DateAssess, Year_date, unit = "days") / 365.25,
                 digits = 2
               ))),
      key = c(dx_codes, instance, mapper_file)
    )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    return(y)
  }


TEU_VeI_HTNmeds_rubric <- function(x) {
      rubric <- readRDS(file.path(config$data$derived, "HTNMedsRubric.rds"))
      y <- rubric[["HTN_probablemeds"]][match(x, rubric$ID)]
    }


VeI_PregnantNow <- function(x){x}


TEU_BaC_AgeCat <- function(x){
    cut(x, breaks=breaks, labels=labels, right=right)
  }


TEU_BlP_measuredHTN <- function(data) {
      data[["TEU_BlP_SBP.avg"]] >= SBPthreshold |
        data[["TEU_BlP_DBP.avg"]] >= DBPthreshold
    }


TEU_HMH_BowelCancerScreen <- function(x) {
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
    }


TEU_Edu_HighestQual <- function(data) {
      qual_list <- c(
        "Prefer not to answer",
        "None of the above",
        "CSEs or equivalent",
        "O levels/GCSEs or equivalent",
        "A levels/AS levels or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "NVQ or HND or HNC or equivalent",
        "College or University degree" 
      )
      for(i in seq(1, length(qual_list), by=1)) {
        data[data == qual_list[i]] <- as.character(i)
      }
      y <- do.call(pmax, c(data, list(na.rm=TRUE)))
      y[is.na(y)] <- 1
      y <- factor(y,
                  levels = seq(length(qual_list), 1, by=-1),
                  labels = qual_list[seq(length(qual_list), 1, by=-1)]
                  )
      return(y)
    }


TEU_Edu_ISCED <- function(x) {
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
        is.na(x) ~ NA_character_
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
    }


TEU_Emp_CurrStat <- function(data) {
      # XL change: Changed the order of uncorrected and corrected
      y <- factor(coalesce(as.character(data[["Emp_CurrStat.0.0"]]), 
                           as.character(data[["Emp_CurrStatUnc.0.0"]])),
                  ordered=FALSE)
      return(y)
    }


Emp_JobCode.0.0 <- function(x) {
      v2_emp <- DBfunc$DB_extract(extract_cols = c("ID", "Emp_JobCode.0.0"),
                                  db = "K:/TEU/UKB33952_Data/Data_Downloads/V2_database_duckdb0.2.1/ukb_v2.db",
                                  name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")
      y <- v2_emp[["Emp_JobCode.0.0"]][match(x, v2_emp$ID)]
      y <- as.numeric(y)
      return(y)
    }


TEU_Emp_category <- function(data) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding2_flat_Employment.csv"))
      data <- left_join(data, map[,c("Code", "L0")], by=c("Emp_JobCode.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["L0"]]), 
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
      
    }


TEU_HoH_PreTaxInc <- function(data) {
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
    }


TEU_HouseholdIncome <- function(x){
    # XL add: Need to assign variable as factor format first 
    x<-factor(x)
    # Categorise missing data - change levels so specified levels and NA are both "Unanswered"
    labels <- c(levels(x)[-which(levels(x) %in% missingvals)], categ_name)
    y <- as.character(x)
    y[y %in% missingvals] <- categ_name
    y[is.na(y)] <- categ_name
    y <- factor(y, levels=labels, ordered=FALSE)
    return(y)
  }


TEU_CountryIncome <- function(data){
      
      # Generate mapping first 
      UKB_mapper=read.csv_kdrive(file.path(config$cleaning$coding,'coding89_flat_CountryOfBirth.csv'))
      Income_mapper=read.xlsx_kdrive(file.path(config$cleaning$mapping,'CountryIncomeCategory.xlsx'),sheet = 'UKB_CountryNames')
      
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
      
      
    }


TEU_HMH_Meds_BP <- function(data){
    x <- FN_HMHmeds_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested medication across the columns
    y <- apply(data[,c(grep("HMH_MedCholBPDiab.0.", colnames(data), fixed=TRUE),
                       grep("HMH_MedCholBPDiabHorm.0.", colnames(data), fixed=TRUE))], 1, function(x) any(x==medtype))
    # And incorporate the info on whether this participant is taking any other medication
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }


TEU_Smo_Status <- function(x) {
      y <- FN_MissingCategory(missingvals = c("Prefer not to answer"), categ_name = "Unanswered")(x)
      y <- FN_factor(levelorder = c("Never", "Previous", "Current", "Unanswered"))(y)
      }


TEU_Alc_Status <- function(x) {
      y <- FN_MissingCategory(missingvals = c("Prefer not to answer"), categ_name = "Unanswered")(x)
      y <- FN_factor(levelorder = c("Never", "Previous", "Current", "Prefer not to answer"))(y)
      }


TEU_Alc_WeeklyAlcUnits <- function(data) {
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
    }


TEU_Alc_WeeklyCat <- function(data) {
      cat <- cut(x=data[["TEU_Alc_WeeklyAlcUnits"]],
                 breaks=c(-1, 0, 5, 10, 20, 30, 100),
                 labels=c("None reported", "Less than 5 units", "5 to 10 units", 
                          "10 to 20 units", "20 to 30 units", "30 units or more"),
                 right=FALSE)
      cat[is.na(data[["Alc_Freq.0.0"]])] <- "None reported"
      cat[data[["Alc_Freq.0.0"]] %in% c("Never", "Special occasions only", "Prefer not to answer")] <- "None reported"
      return(cat)
      }


TEU_Alc_Binge <- function(data) {
      y <- dplyr::case_when(data[["BaC_Sex"]] == "Female" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 7 ~ TRUE,
                            data[["BaC_Sex"]] == "Male" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 14 ~ TRUE,
                            TRUE ~ FALSE)
    }


PhA_METsWkAllAct <- function(x){x}


TEU_Pha_METsover1200 <- function(x) {
      y <- dplyr::case_when(
        # XL add: Change 'Unknown' to 'Unanswered'
        is.na(x) ~ "Unanswered",
        x > 1200 ~ "High (MET minutes > 1200)",
        x <= 1200 ~ "Low (MET minutes <= 1200)",
        TRUE ~ "Other"
      )
      y <- factor(y, levels = c("Low (MET minutes <= 1200)", "High (MET minutes > 1200)", "Unanswered"))
      return(y)
    }


TEU_FaH_CVD <- function(data){
    y <- apply(data[,c(grep("FaH_FatherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_MotherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_SibIll.0.", colnames(data), fixed=TRUE)
    )
    ], 1, function(x) any(x %in% conditions)
    )
    y <- factor(as.numeric(y), levels=c(0,1), 
                labels=c(paste0("No family history of ", label), paste0("Family history of ", label)))
    return(y)
  }


BSM_BMI <- function(x){x}


TEU_BSM_BMIcat <- function(x) {
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
    }


TEU_BSM_WaistCircCat <- function(data) {
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
    }


TEU_SBP_PRS <- function(x) {
    if(file_ext(filepath)=="rds"){
      prs <- readRDS(filepath)
    } else if(file_ext(filepath)=="sscore"){
      prs <- read.delim(filepath, header=TRUE) %>%
        rename(ID = IID)
    } else {
      warning("Unidentified file type for PRS")
    }
    y <- prs[[colname]][match(x, prs$ID)]
    return(y)
  }


TEU_DBP_PRS <- function(x) {
    if(file_ext(filepath)=="rds"){
      prs <- readRDS(filepath)
    } else if(file_ext(filepath)=="sscore"){
      prs <- read.delim(filepath, header=TRUE) %>%
        rename(ID = IID)
    } else {
      warning("Unidentified file type for PRS")
    }
    y <- prs[[colname]][match(x, prs$ID)]
    return(y)
  }


TEU_BP_PRS <- function(data){
    rowMeans(data[,colnames], na.rm)
  }


TEU_HMH_VascCond <- function(data) {
  # Combine the vascular condition columns
  vcon <- coalesce(data[["HMH_HeartProbs.0.0"]], data[["HMH_HeartProbs.0.1"]], 
                   data[["HMH_HeartProbs.0.2"]], data[["HMH_HeartProbs.0.3"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
  y <- dplyr::case_when(
    is.na(vcon) ~ "Unanswered",
    vcon == "None of the above" ~ "No",
    vcon == "Do not know" ~ "Do not know",
    vcon == "Prefer not to answer" ~ "Prefer not to answer",
    vcon %in% condlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer", "Unanswered"))
  return(y)
}


TEU_HMH_prevHTN <- function(data){
    x <- FN_Vascular_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested condition across the columns
    y <- apply(data[,c("HMH_HeartProbs.0.0", "HMH_HeartProbs.0.1", "HMH_HeartProbs.0.2", "HMH_HeartProbs.0.3")],
               1, function(x) any(x %in% conditions))
    # And incorporate the info on whether this participant reported any condition
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }


TEU_HMH_prevstroke <- function(data){
    x <- FN_Vascular_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested condition across the columns
    y <- apply(data[,c("HMH_HeartProbs.0.0", "HMH_HeartProbs.0.1", "HMH_HeartProbs.0.2", "HMH_HeartProbs.0.3")],
               1, function(x) any(x %in% conditions))
    # And incorporate the info on whether this participant reported any condition
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }


TEU_HMH_prevCVD <- function(data){
    x <- FN_Vascular_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested condition across the columns
    y <- apply(data[,c("HMH_HeartProbs.0.0", "HMH_HeartProbs.0.1", "HMH_HeartProbs.0.2", "HMH_HeartProbs.0.3")],
               1, function(x) any(x %in% conditions))
    # And incorporate the info on whether this participant reported any condition
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }


HMH_IllDisab <- function(x){
    factor(x, levels=levelorder, ordered=ordered)
  }


HMH_Diabetes <- function(x){
    factor(x, levels=levelorder, ordered=ordered)
  }


HMH_HTNAge <- function(x){
    x[x%in%values]=NA
    x
  }


TEU_BlP_HTNseverity <- function(data) {
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
    }


TEU_VeI_seriouscomb <- function(data){
      # read in finalised excel mapping
      noncancer=read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))
      # Coding list for serious comorbidities
      serious_comb<-noncancer[which(noncancer$Exclude=='Yes'),]$coding
      y<- FN_VI_filtercodes(dx_codes = serious_comb,
                        colname = "VeI_NonCancer",
                        instance = 0,
                        return_label = "dx",
                        mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
      
      return(y)
    }


TEU_VeI_cancer <- function(data){
      
      cancer_mapper=read.csv_kdrive(file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))
      cancer_codes=cancer_mapper[-which(cancer_mapper$L0%in% c('skin cancer')),]$Code
      
      y<- FN_VI_filtercodes(dx_codes = cancer_codes,
                            colname = "VeI_Cancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))(data)
      return(y)
    }


TEU_VeI_numHTNmeds <- function(x) {
      rubric <- readRDS(file.path(config$data$derived, "HTNMedsRubric.rds"))
      y <- rubric[["hypmedsno"]][match(x, rubric$ID)]
    }


TEU_VeI_numHTNmedscat <- function(x) {
      y <- dplyr::case_when(
        is.na(x) ~ "None reported",
        x == 0 ~ "None reported",
        x == 1 ~ "1",
        x == 2 ~ "2",
        x >= 3 ~ "3 or more",
        TRUE ~ as.character(x)
      )
      y <- factor(y, levels=c("None reported", "1", "2", "3 or more"))
    }


TEU_VeI_CVD <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_diab <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_arrhy <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_osteo <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_joint <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_epil <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_mig <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_anx <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_dep <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


TEU_VeI_asthCOPD <- function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }


Prosp_comorb_num <- function(data){
      rowSums(sapply(select(data,everything()),function(x) grepl("Yes",x)))
    }


Prosp_comorb_numcat <- function(x){
      factor(ifelse(x>=3,'>=3',x),levels = c('0','1','2','>=3'),ordered = FALSE)
    }


GeP_UsedInPCA <- function(x){x}


GeP_Outliers <- function(x){x}


GeP_Kinship <- function(x){x}


GeP_ethnic <- function(x){x}


GeP_Array <- function(x){
      coding <- read.csv(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- coding$L0[match(x, coding$Code)]
    }


GeP_Batch <- function(x) {
      coding <- read.csv(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- coding$L1[match(x, coding$Code)]
    }


GeP_PC_1 <- function(x){x}


GeP_PC_2 <- function(x){x}


GeP_PC_3 <- function(x){x}


GeP_PC_4 <- function(x){x}


GeP_PC_5 <- function(x){x}


GeP_PC_6 <- function(x){x}


GeP_PC_7 <- function(x){x}


GeP_PC_8 <- function(x){x}


GeP_PC_9 <- function(x){x}


GeP_PC_10 <- function(x){x}


GeP_Sex <- function(x){x}


TEU_HES_MACE_prev <- function(data){
    
    HES_total<-NULL
    
    # Get first occurrence dataset from each HES source and rbind together
    if (!is.null(ICD9_xlsx)){
      HES_total<-FN_eachHES_First(data%>%select(ID,'Rec_DateAssess', 
                                                contains("HES_ICD9Diag.0."), 
                                                contains("HES_ICD9DateFirst.0.")),
                                  HES_xlsx = ICD9_xlsx,
                                  condition = condition,
                                  colname = 'HES_ICD9',
                                  removeNAfrom = c('Diag','DateFirst'),
                                  record_level = record_level)
    }
    
    if (!is.null(ICD10_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_ICD10Diag.0."),
                                       contains("HES_ICD10DateFirst.0.")),
                         HES_xlsx = ICD10_xlsx,
                         condition = condition,
                         colname = 'HES_ICD10',
                         removeNAfrom = c('Diag','DateFirst'),
                         record_level = record_level))
    }
    
    if (!is.null(OPCS4_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_OPCS4Code.0."),
                                       contains("HES_OPCS4DateFirst.0.")),
                         HES_xlsx = OPCS4_xlsx,
                         condition = condition,
                         colname = 'HES_OPCS4',
                         removeNAfrom = c('Code','DateFirst'),
                         record_level = record_level))
    }
    
    # Select first occurrence among all HES source
    long_dx=HES_total%>%
      group_by(ID)%>%
      # Select the first occurrence
      slice(which.min(DateFirst))%>%
      
      mutate(
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(DateFirst<=Rec_DateAssess,1,0),
        # Subtype column (prior to or at baseline)
        baseline_comp=ifelse(DateFirst<=Rec_DateAssess,ConditionsType,NA),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(DateFirst>Rec_DateAssess,1,0),
        # Date of condition (Follow-up)
        followup_date=if_else(DateFirst>Rec_DateAssess,DateFirst,as.Date(NA)),
        # Subtype column (Follow-up)
        followup_comp=ifelse(DateFirst>Rec_DateAssess,ConditionsType,NA)
      )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    
    if(return_label%in%c('baseline','followup')){
      y[is.na(y)]=0
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    return(y)
    
  }


TEU_VeI_MACE_nonc <- function(data){
      # read in analysis codings xlsx
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/VI_NonCancerIllness_Mapping_20210128.xlsx'),col_types = c('text'))
      
      dx_codes<-as.numeric(mapping[which(mapping$Conditions==condition),]$Code)
      
      y<- FN_VI_filtercodes(dx_codes = dx_codes,
                            colname = "VeI_NonCancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
      # If not blank, assign yes
      y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
      
      return(y)
    }


TEU_VeI_MACE_op <- function(data){
      
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/VI_Operations_Mapping_20210128.xlsx'),col_types = c('text'))
      
      dx_codes<-as.numeric(mapping[which(mapping$Conditions==condition),]$Code)
      y<- FN_VI_filtercodes(dx_codes = dx_codes,
                            colname = "VeI_Operation",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding5_flat_Operation.csv"))(data)
      # If not blank, assign yes
      y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
      
      return(y)
    }


TEU_HMH_MACE_prev <- function(data){
      y<-FN_Vascular_condition(condition=c("Stroke", "Heart attack"), string="MACE")(data)
      levels(y)<-c('Yes','No','Unanswered')
      return(y)
      }


TEU_MACE_prev <- function(data){
      y<-factor(apply(data,1, function(x) any(x %in% 'Yes')),levels = c('FALSE','TRUE'),labels = c('No','Yes'))
      return(y)
    }


TEU_HES_MACE_fudate <- function(data){
    
    HES_total<-NULL
    
    # Get first occurrence dataset from each HES source and rbind together
    if (!is.null(ICD9_xlsx)){
      HES_total<-FN_eachHES_First(data%>%select(ID,'Rec_DateAssess', 
                                                contains("HES_ICD9Diag.0."), 
                                                contains("HES_ICD9DateFirst.0.")),
                                  HES_xlsx = ICD9_xlsx,
                                  condition = condition,
                                  colname = 'HES_ICD9',
                                  removeNAfrom = c('Diag','DateFirst'),
                                  record_level = record_level)
    }
    
    if (!is.null(ICD10_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_ICD10Diag.0."),
                                       contains("HES_ICD10DateFirst.0.")),
                         HES_xlsx = ICD10_xlsx,
                         condition = condition,
                         colname = 'HES_ICD10',
                         removeNAfrom = c('Diag','DateFirst'),
                         record_level = record_level))
    }
    
    if (!is.null(OPCS4_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_OPCS4Code.0."),
                                       contains("HES_OPCS4DateFirst.0.")),
                         HES_xlsx = OPCS4_xlsx,
                         condition = condition,
                         colname = 'HES_OPCS4',
                         removeNAfrom = c('Code','DateFirst'),
                         record_level = record_level))
    }
    
    # Select first occurrence among all HES source
    long_dx=HES_total%>%
      group_by(ID)%>%
      # Select the first occurrence
      slice(which.min(DateFirst))%>%
      
      mutate(
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(DateFirst<=Rec_DateAssess,1,0),
        # Subtype column (prior to or at baseline)
        baseline_comp=ifelse(DateFirst<=Rec_DateAssess,ConditionsType,NA),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(DateFirst>Rec_DateAssess,1,0),
        # Date of condition (Follow-up)
        followup_date=if_else(DateFirst>Rec_DateAssess,DateFirst,as.Date(NA)),
        # Subtype column (Follow-up)
        followup_comp=ifelse(DateFirst>Rec_DateAssess,ConditionsType,NA)
      )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    
    if(return_label%in%c('baseline','followup')){
      y[is.na(y)]=0
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    return(y)
    
  }


TEU_Dth_MACE_dthdate <- function(data){
        mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
        ICD10_codes<-mapping[which(mapping$Conditions=='MACE'),]$Code
        
        y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level)(data)
        
        return(y)
      }


TEU_MACE_eventdate <- function(data){
      y<-pmin(data$TEU_HES_MACE_fudate,data$TEU_Dth_MACE_dthdate,na.rm = TRUE)
      return(y)
    }


TEU_Dth_NotMACE_dthdate <- function(data){
        mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
        ICD10_codes<-mapping[which(is.na(mapping$Conditions)),]$Code
        
        y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level)(data)
        
        return(y)
      }


Admin_CensorDate <- function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml"))
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        y <- do.call(c, datelist)
      }
      else {
        y <- dplyr::case_when(
          x=='England' ~ FN_toDate('2017-03-31'),
          x=='Scotland' ~ FN_toDate('2016-10-31'),
          x=='Wales' ~ FN_toDate('2016-02-29')
        )
      } 
      return(y)
    }


BaC_LostFUDate <- function(x){
  as.Date(x, origin=as.Date("1970-01-01"))
}


TEU_MACE_censordate <- function(data){
      y<-pmin(data$TEU_Dth_NotMACE_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
      return(y)
    }


TEU_MACE_status <- function(data){
      # Check if censoring date has NA
      if (anyNA(data$TEU_MACE_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(TEU_MACE_eventdate) & TEU_MACE_eventdate<=TEU_MACE_censordate ~ 1,
          is.na(TEU_MACE_eventdate) |(!is.na(TEU_MACE_eventdate)&TEU_MACE_eventdate>TEU_MACE_censordate) ~ 0))
      
      return(data$status)
      
    }


TEU_MACE_time <- function(data){
      
      data=data%>%
        mutate(time=case_when(
          TEU_MACE_status==0 ~ as.numeric(difftime(TEU_MACE_censordate, Rec_DateAssess, unit='days')),
          TEU_MACE_status==1 ~ as.numeric(difftime(TEU_MACE_eventdate, Rec_DateAssess, unit='days'))))
      
      return(data$time)
      
    }


TEU_MACE_time_yrs <- function(x){
      as.numeric(round(x/365.25, digits = 2)) 
    }


TEU_HES_MACE_fucomp <- function(data){
    
    HES_total<-NULL
    
    # Get first occurrence dataset from each HES source and rbind together
    if (!is.null(ICD9_xlsx)){
      HES_total<-FN_eachHES_First(data%>%select(ID,'Rec_DateAssess', 
                                                contains("HES_ICD9Diag.0."), 
                                                contains("HES_ICD9DateFirst.0.")),
                                  HES_xlsx = ICD9_xlsx,
                                  condition = condition,
                                  colname = 'HES_ICD9',
                                  removeNAfrom = c('Diag','DateFirst'),
                                  record_level = record_level)
    }
    
    if (!is.null(ICD10_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_ICD10Diag.0."),
                                       contains("HES_ICD10DateFirst.0.")),
                         HES_xlsx = ICD10_xlsx,
                         condition = condition,
                         colname = 'HES_ICD10',
                         removeNAfrom = c('Diag','DateFirst'),
                         record_level = record_level))
    }
    
    if (!is.null(OPCS4_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_OPCS4Code.0."),
                                       contains("HES_OPCS4DateFirst.0.")),
                         HES_xlsx = OPCS4_xlsx,
                         condition = condition,
                         colname = 'HES_OPCS4',
                         removeNAfrom = c('Code','DateFirst'),
                         record_level = record_level))
    }
    
    # Select first occurrence among all HES source
    long_dx=HES_total%>%
      group_by(ID)%>%
      # Select the first occurrence
      slice(which.min(DateFirst))%>%
      
      mutate(
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(DateFirst<=Rec_DateAssess,1,0),
        # Subtype column (prior to or at baseline)
        baseline_comp=ifelse(DateFirst<=Rec_DateAssess,ConditionsType,NA),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(DateFirst>Rec_DateAssess,1,0),
        # Date of condition (Follow-up)
        followup_date=if_else(DateFirst>Rec_DateAssess,DateFirst,as.Date(NA)),
        # Subtype column (Follow-up)
        followup_comp=ifelse(DateFirst>Rec_DateAssess,ConditionsType,NA)
      )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    
    if(return_label%in%c('baseline','followup')){
      y[is.na(y)]=0
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    return(y)
    
  }


TEU_MACE_fucomp <- function(data){
      data=data%>%
        mutate(TEU_MACE_fucomp=case_when(TEU_MACE_status==0  ~ NA_character_,
                                          TEU_MACE_status==1 & !is.na(TEU_MACE_eventdate) & TEU_MACE_eventdate==TEU_Dth_MACE_dthdate ~ 'CVD death',
                                          TRUE ~ TEU_HES_MACE_fucomp
        ))
      return(data$TEU_MACE_fucomp)
    }


TEU_Dth_MACE_dthtype <- function(data){
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
      ICD10_codes<-mapping$Code[!is.na(mapping$ConditionsType)]
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_code', record_level=record_level)(data)

      y <- as.character(sapply(y, function(i) { if(!is.na(i)){mapping$ConditionsType[mapping$Code==i]}}))
      y <- str_remove(y, "Nonfatal ")
      
      return(y)
    }


BBC_LDL_Result <- function(x){x}


TEU_LDLctrl_v1 <- function(x) {
      y <- ifelse(x<threshold, 1, 0)
      y <- factor(as.numeric(y), levels=c(0,1), 
                  labels=c("Not controlled", "Controlled"))
      return(y)
    }


TEU_MACE_MI <- function(data){
      
      HES <- ifelse(data[["TEU_MACE_fucomp"]] %in% c("Nonfatal MI",
                                                     "MI prevention procedure"), 1,0)
      death <- ifelse((data[["TEU_MACE_fucomp"]]=="CVD death" & !is.na(data[["TEU_MACE_fucomp"]])) & 
                        data[["TEU_Dth_MACE_dthtype"]] %in% c("MI"), 1,0)
      y <- as.numeric(HES|death)
      
    }


TEU_MACE_Stroke <- function(data){
      
      HES <- ifelse(data[["TEU_MACE_fucomp"]] %in% c("Nonfatal Stroke - Haemorrhagic",
                                                     "Nonfatal Stroke - Ischaemic",
                                                     "Nonfatal Stroke - Type unspecified",
                                                     "Stroke prevention procedure"), 1,0)
      death <- ifelse((data[["TEU_MACE_fucomp"]]=="CVD death" & !is.na(data[["TEU_MACE_fucomp"]])) & 
                         data[["TEU_Dth_MACE_dthtype"]] %in% c("Stroke - Haemorrhagic",
                                                               "Stroke - Ischaemic",
                                                               "Stroke - Type unspecified"), 1,0)
      y <- as.numeric(HES|death)
      
    }


TEU_MACE_HaemStroke <- function(data){
      
      HES <- ifelse(data[["TEU_MACE_fucomp"]] %in% c("Nonfatal Stroke - Haemorrhagic"), 1,0)
      death <- ifelse((data[["TEU_MACE_fucomp"]]=="CVD death" & !is.na(data[["TEU_MACE_fucomp"]])) & 
                        data[["TEU_Dth_MACE_dthtype"]] %in% c("Stroke - Haemorrhagic"), 1,0)
      y <- as.numeric(HES|death)
      
    }


TEU_HTN_dur <- function(data){
      data=data%>%
        mutate(
          # Duration from TQ
          TEU_TQ_HTN_dur=TEU_BaC_AgeAtRec-HMH_HTNAge,
          # Combine duration from TQ & VI
          # Only take values from TQ when VI is not available, otherwise always take values from VI
          dur=ifelse(!is.na(TEU_TQ_HTN_dur) & is.na(TEU_VeI_HTN_dur),TEU_TQ_HTN_dur,TEU_VeI_HTN_dur)
          )
      # Should set duration<0 to be NA
      y<-data[['dur']];y[y<0]=NA
      
      return(y)
    }


TEU_selfrepHTN_dx <- function(data) {
      VI <- !is.na(data[["TEU_VeI_HTN"]])
      TQ <- (data[["TEU_HMH_prevHTN"]] == "Self-reported hypertension")
      y <- (VI | TQ)
      return(y)
    }


TEU_selfrepHTN_meds <- function(data) {
      VI <- data[["TEU_VeI_HTNmeds_rubric"]]
      VI[is.na(VI)] <- FALSE
      TQ <- (data[["TEU_HMH_Meds_BP"]] == "Self-reported BP meds")
      y <- (VI | TQ)
      return(y)
    }


TEU_evidenceHTN <- function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]] | data[["TEU_BlP_measuredHTN"]])
    }


TEU_awareHTN <- function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]])
      y[(data[["TEU_evidenceHTN"]] == FALSE | is.na(data[["TEU_evidenceHTN"]]))] <- NA
      return(y)
    }


TEU_treatedHTN <- function(data) {
      y <- data[["TEU_selfrepHTN_meds"]]
      y[(data[["TEU_awareHTN"]] == FALSE | is.na(data[["TEU_awareHTN"]]))] <- NA
      return(y)
    }


TEU_controlledHTN <- function(data) {
      y <- !data[["TEU_BlP_measuredHTN"]]
      y[(data[["TEU_treatedHTN"]] == FALSE | is.na(data[["TEU_treatedHTN"]]))] <- NA
      return(y)
    }


TEU_uncontrolledHTN <- function(data) {
      y <- data[["TEU_BlP_measuredHTN"]]
      y[(data[["TEU_treatedHTN"]] == FALSE | is.na(data[["TEU_treatedHTN"]]))] <- NA
      return(y)
    }


HTN_comorb_num <- function(data){
      rowSums(sapply(select(data,everything()),function(x) grepl("Yes",x)))
    }


TownsendDepInd <- function(x){x}


TEU_TownsendDepInd_Quint <- function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    y <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
    if(any(is.na(y))) {
      levels(y) <- c(levels(y), recodeNA)
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }


TEU_BP_PRS_quintiles <- function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    y <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
    if(any(is.na(y))) {
      levels(y) <- c(levels(y), recodeNA)
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }


TEU_LDL_Quintiles <- function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    y <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
    if(any(is.na(y))) {
      levels(y) <- c(levels(y), recodeNA)
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }


TEU_BlP_SBP_quintiles <- function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    y <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
    if(any(is.na(y))) {
      levels(y) <- c(levels(y), recodeNA)
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }


TEU_SBP_PRS_quintiles <- function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    y <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
    if(any(is.na(y))) {
      levels(y) <- c(levels(y), recodeNA)
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }


