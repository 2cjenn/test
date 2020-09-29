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


attach(TEUmaps)
Neo_HTN <- list(ID,
             TEU_BaC_DateOfBirth, Rec_DateAssess, TEU_BaC_AgeAtRec, TEU_BaC_AgeCat, 
             TEU_HMH_BowelCancerScreen, 
             TEU_Edu_HighestQual, TEU_Edu_ISCED)

test <- list(ID, PsF_VisitFreq, BaC_Sex, TEU_BaC_AgeAtRec, Rec_DateAssess)
detach(TEUmaps)



source(file.path(config$scripts$cleaning, "Reorganise", "dataset.R"))
# # Load the data into the db
# ukb_df("ukb38358", path="K:/TEU/UKB33952_Data/Data_Downloads/V2.0_B2006022_R38358/R/Alldata/",
#        dbname="ukb_v2.db", tblname="ukb38358")
# 
# 
# # Check if it worked
# con <- dbConnect(duckdb::duckdb(), "ukb_v2.db")
# 
# dbListTables(con)
# 
# dbDisconnect(con, shutdown=TRUE)

source(file.path(config$scripts$cleaning, "Reorganise", "DuckDB.R"))
data <- derive_variables(database = "ukb_v2.db", field_definitions = test)


