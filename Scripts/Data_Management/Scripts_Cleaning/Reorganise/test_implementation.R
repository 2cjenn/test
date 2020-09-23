# Jennifer Collister
# 22/09/20

library(yaml)
library(glue)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")

source(file.path(config$scripts$cleaning, "Reorganise", "common_derivations.R"))
source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))





common <- list(ID, ethnicity, rsnlostfu, dob, gender, 
               alc_status, smo_status, 
               householdincome, sleep,
               BMIcat, waistcirccat, weeklyMETs,
               reacttime,
               highestqual)

rawdata <- config$data$received
derived <- config$data$derived

if(!file.exists(file.path(derived, "Test_all.rds"))){
  tables <- list("Alc_base", "ArS_base", "BaC", "BBC_base", "BlP_base", "BSM_base","CoF_base",
              "Die_base", "Edu_base", "Emp_base", "Eth_base", "FaH_base", "GAC_base", "HMH_base",
              "HoH_base", "PhA_base", "Rec_base", "Sle_base", "Smo_base", "Sun_base", "VeIcovars_base")
  dfs <- list()
  for(tabname in tables){
    dfs[[tabname]] <- readRDS(file.path(rawdata, glue("{tabname}.rds")))
  }
  alldata <- Reduce(function(df1, df2) merge(df1, df2, by = "ID", all.x = TRUE), dfs)
  saveRDS(alldata, file.path(derived, "Test_all.rds"))
}

alldata <- readRDS(file.path(derived, "Test_all.rds"))




cols <- c("ID", "BaC_Sex", "BaC_RsnLostFU", "TEU_BaC_DateOfBirth", "TEU_Alc_WeeklyAlcUnits", "Sle_Duration",
          "Alc_Status", "TEU_Alc_Status")

data <- derive_variables(alldata, colnames=cols)

data$test <- unclass(data$BaC_RsnLostFU)==1