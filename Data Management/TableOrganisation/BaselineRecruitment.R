#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
base <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\BaC.rds")
rec <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Rec_base.rds")
gac <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\GAC_base.rds")

basechar <-merge(merge(base, rec, by="ID", all=TRUE), gac, by="ID", all=TRUE)

# Convert dates to date format
basechar$recdate <- as.Date(basechar$Rec_DateAssess, origin=as.Date("1970-01-01"))
basechar$lfudate <- as.Date(basechar$BaC_DateLostFU, origin=as.Date("1970-01-01"))
# Estimate date of birth as the 15th of the month of birth
basechar$dob <- as.Date(paste0("15",basechar$BaC_BirthMonth, basechar$BaC_BirthYear), "%d%B%Y")
# Calculate age at recruitment
basechar$age <- as.numeric(round(difftime(basechar$recdate, basechar$dob, unit="days")/365.25,2))

# I don't think gender should be ordered
basechar$gender <- factor(basechar$BaC_Sex, ordered=FALSE)

names(basechar)[names(basechar)=="BaC_RsnLostFU"] <- "lfureason"
names(basechar)[names(basechar)=="BaC_DeprivInd"] <- "townsend_depind"
names(basechar)[names(basechar)=="GAC_AideMem"] <- "aide_memoir"
names(basechar)[names(basechar)=="Rec_AssessCentre"] <- "assess_centre"

saveRDS(basechar[,c("ID", "dob", "age", "gender", "lfudate", "lfureason", "townsend_depind", 
                    "recdate", "assess_centre", "aide_memoir")], 
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\basechar.rds")
