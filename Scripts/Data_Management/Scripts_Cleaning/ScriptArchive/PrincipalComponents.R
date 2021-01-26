#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------

library(yaml)

config = yaml.load_file("config.yml")

# Read in the raw data
gep <- readRDS(file.path(config$data$received, "GeP.rds"))

# Let's keep the first 10 PCs
pclist <- paste0("GeP_PC.m",seq(1,10))

# save
saveRDS(gep[,c("ID", pclist)], 
        file=file.path(config$data$derived, "principalcomponents.rds"))

