#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 09/12/2019
# Code to filter through the HES diagnoses and produce specific tables per condition
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
dbGetQuery(conn, "PRAGMA table_info(BlP_base)")

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

ICD10Diag <- dbGetQuery(conn, paste0("SELECT ID, ", diagcollist("HES_ICD10Diag", ".m", 212), ", ", diagcollist("HES_ICD10DateFirst", ".m", 212), "FROM HES"))
ICD10DiagS <- code_filter(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=stroke_ICD10, separator=".m", first=FALSE)
ICD10DiagFirst <- do.call(rbind, by(ICD10DiagS, ICD10DiagS[,c("ID")], function(x) x[which.min(x$Date),]))
# Classify the stroke diagnoses by type of stroke
ICD10DiagFirst$type <- ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% haem_ICD10, "Haemorrhagic",
                              ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% infarct_ICD10, "Ischaemic", "Other"))

# Repeat for ICD9 diagnoses
ICD9Diag <- dbGetQuery(conn, paste0("SELECT ID, ", diagcollist("HES_ICD9Diag", ".m", 46), ", ", diagcollist("HES_ICD9DateFirst", ".m", 46), "FROM HES"))
ICD9Diag <- code_filter(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=stroke_ICD9, separator=".m", first=FALSE)
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))
ICD9DiagFirst$type <- ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% haem_ICD9, "Haemorrhagic",
                             ifelse(substr(ICD9DiagFirst$Code, 1, 3) %in% infarct_ICD9, "Ischaemic", "Other"))



# Combine the ICD10 and ICD9 diagnoses into one stroke diagnosis dataset
stroke <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first stroke per person
# (There were only 3 individuals who had both ICD9 and ICD10 stroke diagnoses)
stroke <- do.call(rbind, by(stroke, stroke[,c("ID")], function(x) x[which.min(x$Date),]))


# Save this table to the database
dbWriteTable(conn, "DIAGNOSES_STROKE", stroke, overwrite=TRUE)
