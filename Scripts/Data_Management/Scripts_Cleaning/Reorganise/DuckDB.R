library(tidyverse)
library(DBI)
library(duckdb)
library(data.table)
library(yaml)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}
source(file.path(config$scripts$cleaning, "Reorganise", "dataset.R"))

# Load the data into the db
ukb_df("ukb42633", path="K:/TEU/UKB33952_Data/Data_Downloads/V2.2_B2008751_R42633/Data_20200708/",
       dbname="ukb_v2.db", tblname="ukb42633")


# Check if it worked
con <- dbConnect(duckdb::duckdb(), "ukb_v2.db")

dbListTables(con)

dbGetQuery(con, "SELECT COUNT(*) FROM ukb42633")

df <- tbl(con, from="ukb42633") %>% collect
  select(f.eid, f.21.0.0, f.34.0.0, starts_with("f.84."), starts_with("f.87.")) %>% 
  collect

dbDisconnect(con)
