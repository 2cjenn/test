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
TEUvars_age <- c(TEU_BaC_DateOfBirth, Rec_DateAssess, TEU_BaC_AgeAtRec, TEU_BaC_AgeCat)
TEUvars_BP <- c(TEU_BlP_SBP.0.0, TEU_BlP_SBP.0.1, TEU_BlP_DBP.0.0, TEU_BlP_DBP.0.1,
                TEU_BlP_nSBP, TEU_BlP_nDBP,
                TEU_BlP_SBP.avg, TEU_BlP_DBP.avg)
Neo_HTN <- c(ID, BaC_Sex,
             TEUvars_age, 
             TEUvars_BP, TEU_BlP_measuredHTN,
             TEU_HMH_BowelCancerScreen, 
             TEU_Edu_HighestQual, TEU_Edu_ISCED,
             TEU_TownsendDepInd_Quint)
detach(TEUmaps)


data <- derive_variables(database = "ukb_v2.db", field_definitions = Neo_HTN)


