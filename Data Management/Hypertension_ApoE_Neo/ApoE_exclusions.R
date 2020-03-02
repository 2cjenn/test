#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the dataset
# n = 514,776
apoe <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\APOE\\apoe_surv.rds")

apoe$e4carrier <- apoe$apoe4>0
apoe$apoe4 <- factor(apoe$apoe4)
apoe$number_of_apoe4_alleles <- apoe$apoe4

apoe$alcdaily <- apoe$Alc_Freq=="Daily or almost daily"
apoe$diab <- apoe$diabetes=="Yes"


## Remove individuals who have withdrawn from UKB
withdrawn <- read.csv("K:\\TEU\\APOE on Dementia\\Data Management\\WithdrawnIDs.csv", header=FALSE)
apoe <- apoe[!apoe$ID %in% withdrawn$V1,]

## Genetic exclusions

# Exclude individuals with APOE e1 because it's rare and don't know much about it
# Exclude individuals with genotype e2/e4 because they cancel out?
# Also can't be definitively sure if genotype is e1/e3 or e2/e4
# n = 11,493
apoe <- apoe[apoe$apoe1==0 & !is.na(apoe$apoe1),]

# Exclude individuals with genetic/reported sex mismatch
# n=337
apoe$Sex <- factor(apoe$Sex, ordered=FALSE)
apoe <- apoe[apoe$Sex == apoe$gender,]

## Other important exclusions

# Exclude individuals with missing BP data
# n = 432
apoe <- apoe[!is.na(apoe$SBP) & !is.na(apoe$DBP),]
apoe <- apoe[!is.na(apoe$evidenceHTN),]

# Restrict to self-report "white"
apoe$cauc <- apoe$eth_group=="White"
apoe <- apoe[apoe$cauc==TRUE,]


# Exclude individuals with prevalent dementia
# n = 155
apoe <- apoe[!apoe$VIdementia,]
apoe <- apoe[is.na(apoe$dement_date) | (apoe$dement_date>apoe$recdate & !is.na(apoe$dement_date)),]


# Exclude individuals with no age data
# n = 0
apoe <- apoe[!is.na(apoe$age),]
# Exclude those outside the 40-70 age range
# n = 2247
apoe <- apoe[apoe$age >= 40 & apoe$age < 70,]

# Remaining n = 474,343
saveRDS(apoe, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\APOE\\apoe_excl.rds")