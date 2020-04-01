#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(forcats)

#--------------------------------------------------------------------------------------------------------------
inpath <- readChar("./data/raw/filepath.txt", file.info("./data/raw/filepath.txt")$size)
outfile <- "./data/clean/covars.rds"
#--------------------------------------------------------------------------------------------------------------

# Read in the baseline characteristics
covars <- readRDS("./data/clean/basechar.rds")


# Categorise age into 10-yr groups
covars$agegrp <- cut(covars$age, breaks=c(40, 50, 60, 70), right=FALSE)

#--------------------------------------------------------------------------------------------------------------
# Ethnicity
ethnicity <- readRDS(paste0(inpath, "Eth_base.rds"))

# Reorder the ethnicity factor levels to make more sense when tabulating
ethnicity$Eth_Ethnicity <- factor(ethnicity$Eth_Ethnicity, levels=c("White", "British", "Irish", "Any other white background",
                                                                   "Mixed", "White and Black Caribbean", "White and Black African", 
                                                                   "White and Asian", "Any other mixed background",
                                                                   "Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", 
                                                                   "Any other Asian background",
                                                                   "Black or Black British", "Caribbean", "African", "Any other Black background",
                                                                   "Chinese", "Other ethnic group", "Do not know", "Prefer not to answer"))

# Collapse ethnic groups into broader categories
ethnicity$ethnicity <- as.character(ethnicity$eth_group)
ethnicity$ethnicity <- ifelse(ethnicity$ethnicity=="White", "White", 
                         ifelse(ethnicity$ethnicity=="Prefer not to answer" | is.na(ethnicity$ethnicity), "Unknown",
                                "Non-white"))
ethnicity$ethnicity <- factor(ethnicity$ethnicity, levels=c("White", "Non-white", "Unknown"))

covars <- merge(covars, ethnicity[,c("ID", "Eth_Ethnicity", "ethnicity")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Education
education <- readRDS(paste0(inpath, "Edu_base.rds"))
education$uni <- apply(education[,grep("Edu_Qualif", colnames(education), fixed=TRUE)], 1, function(x) any(x == "College or University degree" & !is.na(x)))
education$eduNA <- apply(education[,grep("Edu_Qualif", colnames(education), fixed=TRUE)], 1, function(x) any(x == "Prefer not to answer" & !is.na(x)))





covars <- merge(covars, education[,c("ID", "uni", "eduNA", "edu_highest", "Edu_Age.0", "ISCED")], by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Employment
jobcodes <- readRDS(paste0(inpath, "Jobcodes_pts.rds"))
jobcodes$employment <- jobcodes$TL

employment <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Emp_base.rds")
employment$shiftwork <- employment$Emp_ShiftWrk.0

covars <- merge(covars, jobcodes[,c("ID", "employment")], by="ID", all=TRUE)
covars <- merge(covars, employment[,c("ID", "shiftwork")], by="ID", all=TRUE)




#--------------------------------------------------------------------------------------------------------------
# Other from verbal interview
veint <- readRDS(paste0(inpath, "VeIcovars_base.rds"))

veint$BirthCountry[is.na(veint$BirthCountry)] <- "United Kingdom"
veint$BirthContinent[is.na(veint$BirthContinent)] <- "Europe"
veint$BirthCountryIncomeLevel[is.na(veint$BirthCountryIncomeLevel)] <- "HUK"
covars <- merge(covars, veint[,c("ID", "BirthContinent", "BirthCountry", "BirthCountryIncomeLevel", 
                                 "VeI_Ncancer.0", "VeI_NNonCancer.0", "VeI_NOperation.0", "VeI_Ntreatments")],
                by="ID", all=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Save the resulting conglomerate
saveRDS(covars, file=outfile)