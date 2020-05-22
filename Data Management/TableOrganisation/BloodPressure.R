#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------
# Read in the raw data
bp <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\BlP_base.rds")

#--------------------------------------------------------------------------------------------------------------
# Mean BP and number of measurements

bp$SBP.0 <- coalesce(bp$BlP_SBPAuto.0, bp$BlP_SBPMan.0)
bp$SBP.1 <- coalesce(bp$BlP_SBPAuto.1, bp$BlP_SBPMan.1)
bp$DBP.0 <- coalesce(bp$BlP_DBPAuto.0, bp$BlP_DBPMan.0)
bp$DBP.1 <- coalesce(bp$BlP_DBPAuto.1, bp$BlP_DBPMan.1)

bp$nSBP <- rowSums(!is.na(bp[,c("SBP.0", "SBP.1")]))
bp$nDBP <- rowSums(!is.na(bp[,c("DBP.0", "DBP.1")]))

bp$SBP <- rowMeans(bp[,c("SBP.0", "SBP.1")], na.rm=TRUE)
bp$DBP <- rowMeans(bp[,c("DBP.0", "DBP.1")], na.rm=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Mean pulse rate
bp$PulseRate <- rowMeans(bp[,c("BlP_PulseRateAuto.0", "BlP_PulseRateAuto.1", "BlP_PulseRate.0", "BlP_PulseRate.1")], na.rm=TRUE)

#--------------------------------------------------------------------------------------------------------------
# Categorise BP based on HTN thresholds
bp$SBPcat <- ifelse(bp$SBP<=120, "SBP<=120", 
                    ifelse(bp$SBP>120 & bp$SBP<140, "120<SBP<140", 
                           ifelse(bp$SBP>=140 & bp$SBP<160, "140<=SBP<160", 
                                  ifelse(bp$SBP>=160, "SBP>=160", "Error"
                                  )
                           )
                    )
)
bp$SBPcat <- factor(bp$SBPcat, levels=c("SBP<=120", "120<SBP<140", "140<=SBP<160", "SBP>=160"))
bp$DBPcat <- ifelse(bp$DBP<=80, "DBP<=80", 
                    ifelse(bp$DBP>80 & bp$DBP<90, "80<DBP<90", 
                           ifelse(bp$DBP>=90 & bp$DBP<100, "90<=DBP<100", 
                                  ifelse(bp$DBP>=100, "DBP>=100", "Error"
                                  )
                           )
                    )
)
bp$DBPcat <- factor(bp$DBPcat, levels=c("DBP<=80", "80<DBP<90", "90<=DBP<100", "DBP>=100"))

#--------------------------------------------------------------------------------------------------------------
# Indicator variable for hypertensive status at baseline assessment
bp$measuredhyp <- (bp$SBP>=140 | bp$DBP>=90)
bp$controlled <- !bp$measuredhyp

#--------------------------------------------------------------------------------------------------------------
# Save the organised data
saveRDS(bp, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\bp.rds")