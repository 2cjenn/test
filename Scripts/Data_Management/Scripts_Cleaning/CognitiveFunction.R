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
cogfunc <- readRDS(file.path(config$data$received, "CoF_base.rds"))

names(cogfunc)[names(cogfunc)=="CoF_RTTTimeID"] <- "mean_reacttime"

saveRDS(cogfunc[,c("ID", "mean_reacttime")],
        file=file.path(config$data$derived, "cognitivefunction.rds"))
