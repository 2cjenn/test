# Jennifer Collister
# 29/09/2020

library(yaml)
library(duckdb)
library(DBI)
library(stringr)
library(tidyverse)
library(data.table)


config = yaml.load_file("config.yml")

source(file.path(config$scripts$cleaning, "Reorganise", "dataset.R"))

#---------------------------------------------------------------------------------
# You need to modify the values here:

# download_runID should be the prefix of the .html, .tab and .R files, of the form ukbXXXXX
download_runID <- "ukb42633"

# file_path should be the path to the folder containing the three ukbXXXXX files
file_path <- "K:/TEU/UKB33952_Data/Data_Downloads/V2.2_B2008751_R42633/Data_20200708"

# db_name should be the name of the database to write the data into
# format: ukb_vX.db where X is the UKB refresh version of the data as per our download naming convention
db_name <- "ukb_v2.db"

# Then run this function to load the data into the db
ukb_db(download_runID,
       path = file_path,
       dbname = db_name,
       tblname = download_runID)

