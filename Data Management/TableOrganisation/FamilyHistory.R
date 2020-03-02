#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
fam <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\FaH_base.rds")

fam$FaH_CVD <- apply(fam[,c(grep("FaH_FatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_MotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_SibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Heart disease", "Stroke")))

saveRDS(fam[,c("ID", "FaH_CVD")],
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\familyhistory.rds")