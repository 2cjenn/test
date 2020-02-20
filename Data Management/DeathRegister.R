#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 09/12/2019
# Code to filter through the Death register data and produce specific tables per condition
#--------------------------------------------------------------------------------------------------------------

library(RSQLite)
library(epitools)
library(reshape2)
library(dplyr)
library(survival)
library(survminer)
library(car)

setwd("K:/TEU/APOE on Dementia/Data Management")
rm(list=(ls()[ls()!="bd"]))

source("K:/TEU/APOE on Dementia/Statistical Analysis/JCfunctions.R")

conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")

# Investigate the list of tables
dbListTables(conn)
# and the columns of data in a table
dbGetQuery(conn, "PRAGMA table_info(Dth)")

#--------------------------------------------------------------------------------------------------------------
# Stroke
#--------------------------------------------------------------------------------------------------------------
# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61", "I62")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4654947/
haem_ICD9 <- c("430", "431", "432")
infarct_ICD9 <- c("433", "434", "436")
other_ICD9 <- c()
stroke_ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9)

deaths <- dbGetQuery(conn, "SELECT * FROM DTH")
strokedeaths <- code_filter(df=deaths, diagcolname="DTH_ICD10", datecolname="Dth_Date", ncols=212, codelist=stroke_ICD10, separator=".m", first=FALSE)

death_filter <- function(df, codelist) {
  for (i in 0:1) {
    for (m in 0:13) {
      print(paste0(i,".",m))
      codecol <- paste0("Dth_ICD10Secondary", ".m", m, ".i", i)
      df[[codecol]][!substr(df[[codecol]], 1, 3) %in% codelist] <- NA
    }
  }
  
  df <- df[rowSums(!is.na(df))>1,]
  df <- df[, unlist(lapply(df, function(x) !all(is.na(x))))]
}