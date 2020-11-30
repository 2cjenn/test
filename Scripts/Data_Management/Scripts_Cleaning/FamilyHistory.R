#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
fam <- readRDS(file.path(config$data$received, "FaH_base.rds"))

#--------------------------------------------------------------------------------------------------------------
# Biological family

fam$FaH_CVD <- apply(fam[,c(grep("FaH_FatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_MotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_SibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Heart disease", "Stroke")))

fam$FaH_HeartDisease <- apply(fam[,c(grep("FaH_FatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_MotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_SibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Heart disease")))

fam$FaH_Stroke <- apply(fam[,c(grep("FaH_FatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_MotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_SibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Stroke")))

fam$FaH_Hypertension <- apply(fam[,c(grep("FaH_FatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_MotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_SibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("High blood pressure")))

#--------------------------------------------------------------------------------------------------------------
# Adoptive family

fam$FaH_Ad_CVD <- apply(fam[,c(grep("FaH_AdFatherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_AdMotherIll.", colnames(fam), fixed=TRUE),
                            grep("FaH_AdSibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Heart disease", "Stroke")))

fam$FaH_Ad_HeartDisease <- apply(fam[,c(grep("FaH_AdFatherIll.", colnames(fam), fixed=TRUE),
                                     grep("FaH_AdMotherIll.", colnames(fam), fixed=TRUE),
                                     grep("FaH_AdSibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Heart disease")))

fam$FaH_Ad_Stroke <- apply(fam[,c(grep("FaH_AdFatherIll.", colnames(fam), fixed=TRUE),
                               grep("FaH_AdMotherIll.", colnames(fam), fixed=TRUE),
                               grep("FaH_AdSibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("Stroke")))

fam$FaH_Ad_Hypertension <- apply(fam[,c(grep("FaH_AdFatherIll.", colnames(fam), fixed=TRUE),
                                     grep("FaH_AdMotherIll.", colnames(fam), fixed=TRUE),
                                     grep("FaH_AdSibIll.", colnames(fam), fixed=TRUE)
)
], 1, function(x) any(x %in% c("High blood pressure")))

saveRDS(fam[,c("ID", "FaH_CVD", "FaH_HeartDisease", "FaH_Stroke", "FaH_Hypertension",
               "FaH_Ad_CVD", "FaH_Ad_HeartDisease", "FaH_Ad_Stroke", "FaH_Ad_Hypertension")],
        file=file.path(config$data$derived, "familyhistory.rds"))