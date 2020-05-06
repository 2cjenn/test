#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
gep <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\GeP.rds")

# Let's keep the first 10 PCs
pclist <- paste0("GeP_PC.m",seq(1,10))

# save
saveRDS(gep[,c("ID", pclist)], 
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\principalcomponents.rds")

