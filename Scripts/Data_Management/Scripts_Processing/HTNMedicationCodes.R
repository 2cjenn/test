#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 21/01/2020
# Filter the medication codes reported in verbal interview
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(readxl)
library(reshape2)
library(dplyr)
library(yaml)
# library(data.table)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------
# Load the excel of medications where hypertensive drugs are identified
# Generate a table with a column per generic drug to identify which medications contain which drugs
# Also generate a table with a column per drug class
#--------------------------------------------------------------------------------------------------------------

htmeds <- read_excel(file.path(config$cleaning$mapping, "UKBmedsHypertension_JC_20200122.xlsx"), 
                     sheet="MedicationCodes", col_names=TRUE, na="0")

# Consider only hypertensive medications - makes dataset smaller and easier to work with
htmeds <- htmeds[htmeds$antibp==1 & !is.na(htmeds$antibp),]
# Make the coding column an integer for ease of use
htmeds$coding <- as.integer(htmeds$coding)
# Consider only columns of interest
htmeds <- as.data.frame(htmeds[,c("coding", "medname", "generic.1", "generic.2", "class.1", "class.2")])

# Reshape to long format so we have a single column for generic drug and a single column for drug class
htmedslong <- reshape(htmeds, direction='long', idvar="coding", varying=c("generic.1", "generic.2", "class.1", "class.2"), sep=".")
# Again remove NAs to make it easier to work with
htmedslong <- htmedslong[!is.na(htmedslong$generic),]

# Add a column of 1s to be used as the value
htmedslong$test <- 1
# Long to wide - a column per generic drug name or class
codegeneric <- reshape2::dcast(htmedslong, coding~generic, value.var="test")
codeclass <- reshape2::dcast(htmedslong, coding~class, value.var="test")

# Remove the NA column from the end (this was to allow blanks in the dropdown in excel)
codeclass <- codeclass[,1:(ncol(codeclass)-1)]

# Combine them to get generic drugs and classes in one dataframe
codemapping <- merge(codegeneric, codeclass, by="coding")

# Make lists of the names of the generic drugs
hypdruglist <- colnames(codegeneric)[-c(1)]
# and classes
hypclasslist <- colnames(codeclass)[-c(1)]

# Remove the old tables to save memory
rm(htmeds, htmedslong, codegeneric, codeclass)

#--------------------------------------------------------------------------------------------------------------
# Load the participant data and join the drug types to get a column per generic drug and class for each participant
# Where a participant is taking multiple hypertensive medications we can collapse these to just get a summary of which drugs
# Note: We are considering hypertensive medications ONLY
#--------------------------------------------------------------------------------------------------------------

veint <- readRDS(file.path(config$data$derived, "VeI_medcodes_base.rds"))
# We have this field for all 502520 individuals
sum(rowSums(veint[,c(2:49)], na.rm=TRUE)==0)
# But for 138537 individuals it is NA across all columns
# This means 363,983 individuals have at least one medication recorded in the verbal interview


# Get this into long format, so we have one column of medcodes and can join our drug info to this
veint <- gather(veint, medno, coding, VeI_MedCode.0:VeI_MedCode.47, factor_key=TRUE)
veint <- veint[!is.na(veint$coding),]

# Join to the drug info
# Left join - only keep rows corresponding to participants taking any medication
VImeds <- merge(veint, codemapping, by="coding", all.x=TRUE)
length(unique(VImeds$ID))

# There are 112,372 unique individuals taking at least one medication which we categorised as "hypertensive"

# Drop the "coding" and "medno" columns so that we can summarise - get a single row for each participant
# and columns for all the hypertensive drugs
VImeds <- VImeds[,-which(names(VImeds) %in% c("coding", "medno"))]
VImeds %<>% group_by(ID) %>%
    summarise_all(.funs=mean, na.rm=TRUE)
sum(rowSums(VImeds[,-c(1)], na.rm=TRUE)==0)

VImeds$hypmedsno <- rowSums(VImeds[,setdiff(names(VImeds), c("ID", hypclasslist))], na.rm=TRUE)
# Check the ones where they're taking more individual medications than categories of medication
consider <- VImeds[rowSums(VImeds[,c(hypclasslist)], na.rm=TRUE)!=VImeds$hypmedsno,]

VImeds$hypmeds <- ifelse(VImeds$hypmedsno!=0, "Taking potential BP medication", "Taking non-BP medication")

saveRDS(VImeds, file=paste0(config$data$derived, "VIhypmeds.rds"))
save(hypclasslist, hypdruglist, VImeds, file=paste0(config$data$derived, "VIhypmeds.RData"))


