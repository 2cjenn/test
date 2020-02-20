# Jennifer Collister
# Code to combine the medication classifications with the actual data and categorise individuals by whether we think they are
# definitely, probably or not taking medication for their BP

data <- readRDS(file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\APOE\\apoe_excl.rds")
load("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\VIhypmeds.RData")

# Line 1 drugs: Ace Inhibitors (ACEI), Angiotensin II receptor blockers (ARBs) and Calcium Channel Blockers (CCB)
VImeds$line1 <- rowSums(VImeds[,c("ACEI", "ARBs", "CCB")], na.rm=TRUE)>0
# Line 2 drugs: Beta blocker (BB), Alpha blocker (AB) or Spironolactone
VImeds$line2 <- rowSums(VImeds[,c("AB", "BB", "Potassium-sparing diuretic")], na.rm=TRUE)>0
VImeds$onlyone <- rowSums(VImeds[,hypclasslist], na.rm=TRUE)==1

VImeds$step12 <- VImeds$line1 & is.na(VImeds[["Thiazide"]]) & !VImeds$line2 & is.na(VImeds[["Other"]])
VImeds$step3 <- VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & !VImeds$line2 & is.na(VImeds[["Other"]])
VImeds$step4 <- VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & VImeds$line2 & is.na(VImeds[["Other"]])
VImeds$step5 <- VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & VImeds$line2 & !is.na(VImeds[["Other"]])
VImeds$thiaonly <- !is.na(VImeds[["Thiazide"]]) & VImeds$onlyone
VImeds$BBonly <- !is.na(VImeds[["BB"]]) & VImeds$onlyone
VImeds$weird1 <- VImeds$line1 & is.na(VImeds[["Thiazide"]]) & VImeds$line2 & is.na(VImeds[["Other"]])
VImeds$weird2 <- !VImeds$line1 & !is.na(VImeds[["Thiazide"]]) & VImeds$line2 & is.na(VImeds[["Other"]])


VImeds$hypgrp <- ifelse(VImeds$step12==TRUE, "Step 1/2", 
                        ifelse(VImeds$step3==TRUE, "Step 3", 
                               ifelse(VImeds$step4==TRUE, "Step 4", 
                                      ifelse(VImeds$step5==TRUE, "Step 5", 
                                             ifelse(VImeds$thiaonly==TRUE, "Thiazide only", 
                                                    ifelse(VImeds$BBonly==TRUE, "BB only", 
                                                           ifelse(VImeds$weird1==TRUE, "Step 4 without thiazide",
                                                                  ifelse(VImeds$weird2==TRUE, "Step 4 without Step 1/2 drugs",
                                                                         VImeds$hypmeds))))))))


medsdata <- merge(data[,c("ID", "Sex", "measuredhyp", "selfrephyp", "HBPmeds", "medication", "VIhyp", "prevHBP",
                          "measuredhyp_", "selfrephyp_", "HBPmeds_", "VIhyp_", "prevHBP_")], 
                  VImeds,#[,c("ID", "grp1", "grp2", "grp1T", "grp2T", "hypgrp", "hypmeds")], 
                  by="ID", all.x=TRUE)
# Joining the two datasets introduced a bunch of NAs because of people who aren't taking any medication, and therefore don't feature in the VImeds dataset
medsdata$hypmeds[is.na(medsdata$hypmeds)] <- "Not taking any medication"
medsdata$hypgrp[is.na(medsdata$hypgrp)] <- "Not taking any medication"

# Once the potential BP medication has been categorised, any remaining are non-standard combinations
medsdata$hypgrp[medsdata$hypgrp=="Taking potential BP medication"] <- "Other non-standard combination of potential BP medication"
# Make the groups a factor, with appropriate ordering of levels
medsdata$hypgrp <- factor(medsdata$hypgrp, levels=c("Step 1/2", "Step 3", "Step 4", "Step 5", 
                                                    "Thiazide only", "BB only", 
                                                    "Step 4 without thiazide", "Step 4 without Step 1/2 drugs", 
                                                    "Other non-standard combination of potential BP medication", 
                                                    "Taking non-BP medication", "Not taking any medication"))

saveRDS(medsdata, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\ClassifiedHypMedGroups.rds")