#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 27/03/2020
# Clean the UKB baseline & recruitment data
#--------------------------------------------------------------------------------------------------------------
# No libraries needed
#--------------------------------------------------------------------------------------------------------------
inpath <- readChar("./data/raw/filepath.txt", file.info("./data/raw/filepath.txt")$size)
outfile <- "./data/clean/basechar.rds"
#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
base <- readRDS(paste0(inpath, "BaC.rds"))
rec <- readRDS(paste0(inpath, "Rec_base.rds"))

basechar <- merge(base, rec, by="ID", all=TRUE)

# Estimate date of birth as the 15th of the month of birth
basechar$DV_DateOfBirth <- as.Date(paste0("15",basechar$BaC_BirthMonth, basechar$BaC_BirthYear), "%d%B%Y")
# Calculate age at recruitment
basechar$DV_AgeAtRec <- as.numeric(round(difftime(basechar$Rec_DateAssess, basechar$DV_DateOfBirth, unit="days")/365.25,2))

# These factors shouldn't be ordered
basechar$BaC_Sex <- factor(basechar$BaC_Sex, ordered=FALSE)
basechar$BaC_RsnLostFU <- factor(basechar$BaC_RsnLostFU, ordered=FALSE)

saveRDS(basechar, file=outfile)