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

bp$SBP <- rowMeans(bp[,c("BlP_SBPAuto.0", "BlP_SBPAuto.1", "BlP_SBPMan.0", "BlP_SBPMan.1")], na.rm=TRUE)
bp$DBP <- rowMeans(bp[,c("BlP_DBPAuto.0", "BlP_DBPAuto.1", "BlP_DBPMan.0", "BlP_DBPMan.1")], na.rm=TRUE)
bp$PulseRate <- rowMeans(bp[,c("BlP_PulseRateAuto.0", "BlP_PulseRateAuto.1", "BlP_PulseRate.0", "BlP_PulseRate.1")], na.rm=TRUE)
# bp$SBPcat <- cut(bp$SBP, breaks=c(0, 120, 140, 160, 1000), right=FALSE, labels=c("<120", "120-140", "140-160", ">160"))
# bp$DBPcat <- cut(bp$DBP, breaks=c(0, 80, 90, 100, 1000), right=FALSE, labels=c("<80", "80-90", "90-100", ">100"))
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
# Indicator variable for hypertensive status at baseline assessment
bp$measuredhyp <- (bp$SBP>=140 | bp$DBP>=90)
bp$controlled <- !(bp$SBP>=140 | bp$DBP>=90)

# Save the organised data
saveRDS(bp[,c("ID", "SBP", "DBP", "PulseRate", "SBPcat", "DBPcat", "measuredhyp", "controlled")],
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\bp.rds")