#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

VI_diag <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_diag_base.rds")
VI_diagdur <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_diagdur_base.rds")

# Use the "year" columns for duration
VI_diagdur <- VI_diagdur[,c(1, grep("VeI_NonCancerYear.", colnames(VI_diagdur), fixed=TRUE))]

coding6 <- read.table("K:/TEU/CancerPRS/Data_Dictionary/Mappings/coding6.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)

# Indicator variables for many health conditions of interest
veint_HTNhist <- VI_diag

strcodes <- coding6[grep("stroke|ischaemic stroke|subdural haemorrhage|subarachnoid haemorrhage|brain haemorrhage", coding6$meaning),]
veint_HTNhist$VIstroke <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% strcodes$coding))

bpcodes <- coding6[grep("hypertension|essential hypertension", coding6$meaning),]
veint_HTNhist$VIhyp <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% bpcodes$coding))



# asthcodes <- coding6[grep("asthma", coding6$meaning),]
# VI_diag$VIasth <- apply(VI_diag[,grep("VeI_NonCancerCode", colnames(VI_diag), fixed=TRUE)], 1, function(x) any(x %in% asthcodes$coding))
# 
# copdcodes <- coding6[grep("copd", coding6$meaning),]
# VI_diag$VIcopd <- apply(VI_diag[,grep("VeI_NonCancerCode", colnames(VI_diag), fixed=TRUE)], 1, function(x) any(x %in% copdcodes$coding))
# 
dementiacodes <-coding6[grep("dementia", coding6$meaning),]
veint_HTNhist$VIdementia <- apply(VI_diag[,grep("VeI_NonCancerCode", colnames(VI_diag), fixed=TRUE)], 1, function(x) any(x %in% dementiacodes$coding))

# Here it seems likely that NA doesn't mean not answered, but rather not applicable
# for(col in c("VIstroke", "VIhyp", "VIasth", "VIcopd", "VIdementia")){
#   veint[[col]][is.na(veint$VeI_NonCancerCode.0)] <- NA
# }


# Additionally, prepare a list of all participants and their diagnosis codes 
# this can later be joined to mappings from codes to specific conditions of interest
# Get the data into long format
# And drop rows where individuals do not have any illness recorded
# Diagnosis codes
VI_diaglong <- gather(data=VI_diag, key=medno, value=coding, VeI_NonCancerCode.0:VeI_NonCancerCode.33, factor_key=FALSE)
VI_diaglong <- VI_diaglong[!is.na(VI_diaglong$coding),]
VI_diaglong <- VI_diaglong %>% tidyr::separate(medno, into=c("VeI", "NonCancerCode", "instance"))
# And durations
VI_durlong <- gather(data=VI_diagdur, key=medno, value=year, VeI_NonCancerYear.0:VeI_NonCancerYear.33, factor_key=FALSE)
VI_durlong <- VI_durlong[!is.na(VI_durlong$year),]
VI_durlong <- VI_durlong %>% tidyr::separate(medno, into=c("VeI", "NonCancerYear", "instance"))
# Merge the diagnosis codes with the corresponding durations
VI_diaglong <- merge(VI_diaglong[,c("ID", "instance", "coding")], VI_durlong[,c("ID", "instance", "year")], by=c("ID", "instance"), all.x=TRUE)
# Note that when the year is coded -1 it means unknown, and -3 means preferred not to answer
VI_diaglong$year[VI_diaglong$year %in% c(-1,-3)] <- NA
# remove rows where individuals have the same illness recorded twice - 
# two subtly different conditions that fit in the same UKB category?
VI_diaglong <- unique(VI_diaglong[,c("ID", "coding", "year")])
# Save this ready to be joined to any diagnosis mapping
saveRDS(VI_diaglong, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\VIDiagnosisCodes_long.rds")

VIhyp <- VI_diaglong[VI_diaglong$coding %in% bpcodes$coding,]
VIhypfirst <- VIhyp %>% group_by(ID) %>% slice(which.min(year))
VIhypfirst$VIhypdx_yr <- VIhypfirst$year

veint_HTNhist <- merge(veint_HTNhist, VIhypfirst[,c("ID", "VIhypdx_yr")], by="ID", all.x=TRUE)
saveRDS(veint_HTNhist[,c("ID", "VIstroke", "VIhyp", "VIhypdx_yr", "VIdementia", "VeI_NNonCancer.0")], 
        file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\veint_HTNhist.rds")
