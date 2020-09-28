# Jennifer Collister
# 22/09/20

library(yaml)
library(glue)
library(tidyverse)
library(DBI)
library(duckdb)
library(data.table)


# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

# if(!file.exists(file.path(config$data$derived, "Test_all.rds"))){
#   tables <- list("Alc_base", "ArS_base", "BaC", "BBC_base", "BlP_base", "BSM_base","CoF_base",
#                  "Die_base", "Edu_base", "Emp_base", "Eth_base", "FaH_base", "GAC_base", "HMH_base",
#                  "HoH_base", "PhA_base", "Rec_base", "Sle_base", "Smo_base", "Sun_base", "VeIcovars_base")
#   dfs <- list()
#   for(tabname in tables){
#     dfs[[tabname]] <- readRDS(file.path(config$data$received, glue("{tabname}.rds")))
#   }
#   alldata <- Reduce(function(df1, df2) merge(df1, df2, by = "ID", all.x = TRUE), dfs)
#   saveRDS(alldata, file.path(config$data$derived, "Test_all.rds"))
# }
# alldata <- readRDS(file.path(config$data$derived, "Test_all.rds"))

# cols <- c("ID", "BaC_Sex", "BaC_RsnLostFU", "TEU_BaC_DateOfBirth", "Sle_Duration",
#           "Alc_Status", "TEU_TownsendDepInd_Quint")#, "TEU_Alc_WeeklyAlcUnits", "TEU_Alc_Status")

attach(TEUmaps)
Neo_HTN <- list(ID,
             TEU_BaC_DateOfBirth, Rec_DateAssess, TEU_BaC_AgeAtRec, TEU_BaC_AgeCat, 
             TEU_HMH_BowelCancerScreen, 
             TEU_Edu_HighestQual, TEU_Edu_ISCED)

test <- list(ID, PsF_VisitFreq, BaC_Sex, PsF_VisitFreq(instance=visit$repeat_visit))
detach(TEUmaps)


data <- derive_variables(alldata, colnames=Neo_HTN)



source(file.path(config$scripts$cleaning, "Reorganise", "dataset.R"))


# # Load the data into the db
# ukb_df("ukb38358", path="K:/TEU/UKB33952_Data/Data_Downloads/V2.0_B2006022_R38358/R/Alldata/",
#        dbname="ukb_v2.db", tblname="ukb38358")


# Check if it worked
con <- dbConnect(duckdb::duckdb(), "ukb_v2.db")

dbListTables(con)

dbDisconnect(con, shutdown=TRUE)

source(file.path(config$scripts$cleaning, "Reorganise", "DuckDB.R"))
data <- DB_extract(test)
