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

VI_diag <- readRDS(file.path(config$data$derived, "VeI_diag_base.rds"))
VI_diagdur <- readRDS(file.path(config$data$derived, "VeI_diagdur_base.rds"))

# Use the "year" columns for duration
VI_diagage <- VI_diagdur[,c(1, grep("VeI_NonCancerAge.",  colnames(VI_diagdur),  fixed=TRUE))]
VI_diagdur <- VI_diagdur[,c(1, grep("VeI_NonCancerYear.", colnames(VI_diagdur), fixed=TRUE))]


coding6 <- read.table(file.path(config$cleaning$coding, "coding6.tsv"), sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)

# Indicator variables for many health conditions of interest
veint_HTNhist <- VI_diag

strcodes <- coding6[grep("stroke|ischaemic stroke|subdural haemorrhage|subarachnoid haemorrhage|brain haemorrhage", coding6$meaning),]
veint_HTNhist$VIstroke <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% strcodes$coding))

bpcodes <- coding6[grep("hypertension|essential hypertension", coding6$meaning),]
veint_HTNhist$VIhyp <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% bpcodes$coding))

alscodes <- coding6[grep("motor neurone disease", coding6$meaning),]
veint_HTNhist$VIals <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% alscodes$coding))

parkcodes <- coding6[grep("parkinsons disease", coding6$meaning),]
veint_HTNhist$VIpark <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% parkcodes$coding))

cholcodes <- coding6[grep("high cholesterol",  coding6$meaning),]
veint_HTNhist$VIchol <- apply(veint_HTNhist[,grep("VeI_NonCancerCode", colnames(veint_HTNhist), fixed=TRUE)], 1, function(x) any(x %in% cholcodes$coding))

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
# alspark <- veint_HTNhist[veint_HTNhist$VIals | veint_HTNhist$VIpark,c("ID", "VIals", "VIpark")]
# saveRDS(alspark, file=paste0(config$data$derived, "veint_ALSpark_hist.rds"))                      

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
# And ages
VI_agelong <- gather(data=VI_diagage, key=medno, value=age, VeI_NonCancerAge.0:VeI_NonCancerAge.33, factor_key=FALSE)
VI_agelong <- VI_agelong[!is.na(VI_agelong$age),]
VI_agelong <- VI_agelong %>% tidyr::separate(medno, into=c("VeI", "NonCancerAge", "instance"))
# Merge the diagnosis codes with the corresponding durations
VI_diaglong <- merge(VI_diaglong[,c("ID", "instance", "coding")], VI_durlong[,c("ID", "instance", "year")], 
                     by=c("ID", "instance"), all.x=TRUE)
VI_diaglong <- merge(VI_diaglong, VI_agelong[,c("ID", "instance", "age")], 
                     by=c("ID", "instance"), all.x=TRUE)
# Note that when the year is coded -1 it means unknown, and -3 means preferred not to answer
VI_diaglong$year[VI_diaglong$year %in% c(-1,-3)] <- NA
VI_diaglong$age[VI_diaglong$age %in% c(-1,-3)] <- NA
# remove rows where individuals have the same illness recorded twice - 
# two subtly different conditions that fit in the same UKB category?
VI_diaglong <- unique(VI_diaglong[,c("ID", "coding", "year", "age")])
# Save this ready to be joined to any diagnosis mapping
saveRDS(VI_diaglong, paste0(config$data$derived, "VIDiagnosisCodes_long.rds"))

VIhyp <- VI_diaglong[VI_diaglong$coding %in% bpcodes$coding,]
VIhypfirstyr <- VIhyp %>% group_by(ID) %>% slice(which.min(year))
VIhypfirstyr$VIhypdx_yr <- VIhypfirstyr$year
VIhypyoungest <- VIhyp %>% group_by(ID) %>% slice(which.min(age))
VIhypyoungest$VIhypdx_age <- VIhypyoungest$age

veint_HTNhist <- merge(veint_HTNhist[,c("ID", "VIstroke", "VIhyp", "VIdementia", "VIchol", "VeI_NNonCancer.0")], 
                       VIhypfirstyr[,c("ID", "VIhypdx_yr")], by="ID", all.x=TRUE)
veint_HTNhist <- merge(veint_HTNhist, VIhypyoungest[,c("ID", "VIhypdx_age")], by="ID", all.x=TRUE)
saveRDS(veint_HTNhist[,c("ID", "VIstroke", "VIhyp", "VIhypdx_yr", "VIhypdx_age", "VIdementia", "VIchol", "VeI_NNonCancer.0")], 
        file=paste0(config$data$derived, "veint_HTNhist.rds"))
