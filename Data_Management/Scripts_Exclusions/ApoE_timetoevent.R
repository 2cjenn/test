#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

apoe <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\APOE\\apoe_raw.rds")

# Individuals with dementia as a cause of death should be included as having dementia
apoe$eventdate <- apoe$dement_date
apoe$eventdate[apoe$demdeath==TRUE] <- pmin(apoe$dement_date[apoe$demdeath==TRUE], apoe$deathdate[apoe$demdeath==TRUE], na.rm=TRUE)


# Calculate time in days to different end points
# Time to HES-recorded diagnosis of dementia
apoe$demtime <- as.numeric(difftime(apoe$eventdate, apoe$recdate, unit='days'))

# Time to death
apoe$deathtime <- as.numeric(difftime(apoe$deathdate, apoe$recdate, unit='days'))

# Time until lost to follow-up
apoe$lfutime <- as.numeric(difftime(apoe$lfudate, apoe$recdate, unit='days'))

# Administrative censoring:
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016
# Majority of data is from England therefore use that date
apoe$actime <- as.numeric(difftime(as.Date("31Mar2017", "%d%B%Y"), apoe$recdate, units="days"))

# For the survival analysis, we want the time until the first of these end points
apoe$time_to_dementia <- pmin(apoe$demtime, apoe$deathtime, apoe$lfutime, apoe$actime, na.rm=TRUE)
apoe$time_to_dementia_yrs <- apoe$time_to_dementia/365.25
# The status is whether they received a dementia diagnosis by the end point
apoe$dementia_status <- (!is.na(apoe$demtime) & apoe$demtime == apoe$time_to_dementia)

# At the moment, the censoring date from HES data is before the latest death registry data
# So we have 45 individuals who have dementia listed on their death certificate, but who died after the current administrative follow-up point
# Alas for wasted data
hmm <- apoe[apoe$dementia_status==0 & apoe$demdeath==TRUE,]

saveRDS(apoe, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\APOE\\apoe_surv.rds")