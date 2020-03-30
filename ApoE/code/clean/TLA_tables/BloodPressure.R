#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
inpath <- readChar("./data/raw/filepath.txt", file.info("./data/raw/filepath.txt")$size)
outfile <- "./data/clean/bp.rds"
#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
bp <- readRDS(paste0(inpath, "BlP_base.rds"))

bp$DV_SBP.avg <- rowMeans(bp[,c("BlP_SBPAuto.0", "BlP_SBPAuto.1", "BlP_SBPMan.0", "BlP_SBPMan.1")], na.rm=TRUE)
bp$DV_DBP.avg <- rowMeans(bp[,c("BlP_DBPAuto.0", "BlP_DBPAuto.1", "BlP_DBPMan.0", "BlP_DBPMan.1")], na.rm=TRUE)
bp$DV_PulseRate.avg <- rowMeans(bp[,c("BlP_PulseRateAuto.0", "BlP_PulseRateAuto.1", "BlP_PulseRate.0", "BlP_PulseRate.1")], na.rm=TRUE)

bp$DV_SBP.cat <- ifelse(bp$DV_SBP.avg<=120, "SBP<=120", 
                    ifelse(bp$DV_SBP.avg>120 & bp$DV_SBP.avg<140, "120<SBP<140", 
                           ifelse(bp$DV_SBP.avg>=140 & bp$DV_SBP.avg<160, "140<=SBP<160", 
                                  ifelse(bp$DV_SBP.avg>=160, "SBP>=160", "Error"
                                  )
                           )
                    )
)
bp$DV_SBP.cat <- factor(bp$DV_SBP.cat, levels=c("SBP<=120", "120<SBP<140", "140<=SBP<160", "SBP>=160"))
bp$DV_DBP.cat <- ifelse(bp$DV_DBP.avg<=80, "DBP<=80", 
                    ifelse(bp$DV_DBP.avg>80 & bp$DV_DBP.avg<90, "80<DBP<90", 
                           ifelse(bp$DV_DBP.avg>=90 & bp$DV_DBP.avg<100, "90<=DBP<100", 
                                  ifelse(bp$DV_DBP.avg>=100, "DBP>=100", "Error"
                                  )
                           )
                    )
)
bp$DV_DBP.cat <- factor(bp$DV_DBP.cat, levels=c("DBP<=80", "80<DBP<90", "90<=DBP<100", "DBP>=100"))

# These factors shouldn't be ordered
bp$BlP_MthMsrBP.0 <- factor(bp$BlP_MthMsrBP.0, ordered=FALSE)
bp$BlP_MthMsrBP.1 <- factor(bp$BlP_MthMsrBP.1, ordered=FALSE)

# Indicator variable for hypertensive status at baseline assessment
bp$DV_MeasuredHTN <- (bp$DV_SBP.avg>=140 | bp$DV_DBP.avg>=90)


# Save the organised data
saveRDS(bp, file=outfile)