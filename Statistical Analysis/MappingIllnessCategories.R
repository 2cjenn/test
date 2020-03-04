#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 24/01/2020
# Identify non-cancer illness codes relative for Neo's hypertension work
#--------------------------------------------------------------------------------------------------------------


noncancerillness_mapping <- function(mappath, mapcol="Mapping", outfile) {
  require(reshape2)
  require(readxl)
  # Load the long-format data of participant IDs and verbal interview diagnosis codes
  pts <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\VIDiagnosisCodes_long.rds")
  
  # Load the mapped diagnoses data
  diagnoses <- read_excel(mappath)
  diagnoses$Mapping <- gsub(" ", "_", diagnoses[[mapcol]])
  diagnoses <- diagnoses[!is.na(diagnoses$Mapping),]
  
  # Join them
  ptdiagnoses <- merge(pts, diagnoses[,c("coding", "Mapping")], by="coding", all.x=TRUE)
  
  # Save a long version with the earliest year of diagnosis per coded condition
  ptdiagnoses <- aggregate(year ~ ID + coding + Mapping, data=ptdiagnoses, min)
  saveRDS(ptdiagnoses, file=paste0(outfile,"_long.rds"))
  
  # Pivot to get a column for each mapped condition
  # We don't need the UKB code for each condition any more,
  # but we do need a dummy column filled with 1s so that when we pivot each condition column will be 1/0
  ptdiagnoses$coding <- 1
  # Now dcast from long to wide so we have a column per category
  # ptdiagnoses <- unique(ptdiagnoses[,c("ID", "Mapping", "coding")])
  ptdiagnoses <- dcast(ptdiagnoses, ID ~ Mapping, value.var="coding", fun.aggregate = sum)
  
  # Save the result
  
  saveRDS(ptdiagnoses, file=paste0(outfile,".rds"))
}

#--------------------------------------------------------------------------------------------------------------
# Neo's hypertension study, parallel to Botswana study
#--------------------------------------------------------------------------------------------------------------
# Conditions of interest for hypertension study
noncancerillness_mapping(mappath="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\UKBHtn_NonCancerIllness_Mapping.xlsx",
                         outfile="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypIllnesses")

# Chronic co-morbidities of interest
noncancerillness_mapping(mappath="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\UKBHtn_NonCancerIllness_Mapping.xlsx",
                         mapcol="ComorbidityCategory",
                         outfile="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VI_ComorbidityCategories")
# test <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VI_ComorbidityCategories.rds")


# Alternate diagnoses for "probable" BP med rubric
noncancerillness_mapping(mappath="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\UKBHtn_NonCancerIllness_Mapping.xlsx",
                         mapcol="AlternateDiagnoses",
                         outfile="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypAltDiagnoses")

# Really serious conditions to be excluded from the data set
noncancerillness_mapping(mappath="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\UKBHtn_NonCancerIllness_Mapping.xlsx",
                         mapcol="Exclude",
                         outfile="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypExclude")

# Group B and Group C
noncancerillness_mapping(mappath="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\UKBHtn_NonCancerIllness_Mapping.xlsx",
                         mapcol="ComorbidityGroup",
                         outfile="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypGroupBC")

test <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\Neo\\VIhypGroupBC.rds")

#--------------------------------------------------------------------------------------------------------------
# Old version
#--------------------------------------------------------------------------------------------------------------
# 
# illness <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\NonCancerIllness_pts.rds")
# illness$flag <- 1
# 
# pts <- as.data.frame(unique(illness$ID))
# names(pts) <- "ID"
# 
# 
# addcol <- function(ptsdf, illnessdf, condition, level, colname){
#   ptsdf <- merge(ptsdf, illnessdf[illnessdf[[level]]==condition & !is.na(illnessdf[[level]]),][,c("ID", "flag")], by="ID", all.x=TRUE)
#   names(ptsdf)[names(ptsdf) == "flag"] <- colname
#   illnessdf <- illnessdf[(!illnessdf[[level]]==condition & !is.na(illnessdf[[level]])) | is.na(illnessdf[[level]]),]
#   return(list(ptsdf, illnessdf))
# }
# 
# # Cardiovascular conditions
# return <- addcol(pts, illness, "essential hypertension", "L2", "essential_hypertension")
# return <- addcol(return[[1]], return[[2]], "gestational hypertension/pre-eclampsia", "L2", "gestational_hypertension")
# return <- addcol(return[[1]], return[[2]], "hypertension", "L1", "other_hypertension")
# return <- addcol(return[[1]], return[[2]], "angina", "L2", "angina")
# return <- addcol(return[[1]], return[[2]], "heart attack/myocardial infarction", "L2", "MI")
# return <- addcol(return[[1]], return[[2]], "heart failure/pulmonary odema", "L2", "pulmonary_oedema")
# return <- addcol(return[[1]], return[[2]], "atrial fibrillation", "L3", "atrial_fibrillation")
# return <- addcol(return[[1]], return[[2]], "heart arrhythmia", "L2", "other_arrhythmia")
# return <- addcol(return[[1]], return[[2]], "heart valve problem/heart murmur", "L2", "heart_valve_problem")
# return <- addcol(return[[1]], return[[2]], "cardiomyopathy", "L2", "cardiomyopathy")
# return <- addcol(return[[1]], return[[2]], "pericardial problem", "L2", "pericardial_problem")
# return <- addcol(return[[1]], return[[2]], "heart/cardiac problem", "L1", "other_cardiac")
# return <- addcol(return[[1]], return[[2]], "stroke", "L2", "stroke")
# return <- addcol(return[[1]], return[[2]], "cerebrovascular disease", "L1", "other_cerebrovascular")
# return <- addcol(return[[1]], return[[2]], "peripheral vascular disease", "L1", "peripheral_vascular")
# return <- addcol(return[[1]], return[[2]], "deep venous thrombosis (dvt)", "L2", "DVT")
# return <- addcol(return[[1]], return[[2]], "venous thromboembolic disease", "L1", "other_thromboembolic")
# return <- addcol(return[[1]], return[[2]], "high cholesterol", "L1", "high_cholesterol")
# return <- addcol(return[[1]], return[[2]], "other venous/lymphatic disease", "L1", "other_venous_lymphatic")
# 
# # Respiratory/ent
# return <- addcol(return[[1]], return[[2]], "asthma", "L1", "asthma")
# return <- addcol(return[[1]], return[[2]], "chronic obstructive airways disease/copd", "L1", "COPD")
# return <- addcol(return[[1]], return[[2]], "bronchitis", "L2", "bronchitis")
# return <- addcol(return[[1]], return[[2]], "respiratory infection", "L1", "respiratory_infection")
# return <- addcol(return[[1]], return[[2]], "respiratory/ent", "TL", "other_respiratory")
# 
# 
# # Gastrointestinal/abdominal
# return <- addcol(return[[1]], return[[2]], "gastrointestinal/abdominal", "TL", "gastrointestinal")
# 
# 
# # Renal/urology
# return <- addcol(return[[1]], return[[2]], "renal/kidney failure", "L1", "kidney_failure")
# return <- addcol(return[[1]], return[[2]], "urinary tract infection/kidney infection", "L1", "UTI")
# return <- addcol(return[[1]], return[[2]], "renal/urology", "TL", "other_renal_urology")
# 
# # Endocrine/diabetes
# return <- addcol(return[[1]], return[[2]], "gestational diabetes", "L2", "gestational_diabetes")
# return <- addcol(return[[1]], return[[2]], "type 1 diabetes", "L2", "type1_diabetes")
# return <- addcol(return[[1]], return[[2]], "type 2 diabetes", "L2", "type2_diabetes")
# return <- addcol(return[[1]], return[[2]], "diabetes insipidus", "L2", "diabetes_insipidus")
# return <- addcol(return[[1]], return[[2]], "diabetes", "L1", "other_diabetes")
# return <- addcol(return[[1]], return[[2]], "thyroid problem (not cancer)", "L1", "thyroid_problem")
# return <- addcol(return[[1]], return[[2]], "endocrine/diabetes", "TL", "other_endocrine")
# 
# # Neurology/eye/psychiatry
# return <- addcol(return[[1]], return[[2]], "neurology", "L1", "neurology")
# return <- addcol(return[[1]], return[[2]], "neurological injury/trauma", "L1", "neuro_injury")
# return <- addcol(return[[1]], return[[2]], "glaucoma", "L2", "glaucoma")
# return <- addcol(return[[1]], return[[2]], "eye/eyelid problem", "L1", "other_eye")
# return <- addcol(return[[1]], return[[2]], "psychological/psychiatric problem", "L1", "psychiatric")
# 
# # Musculoskeletal/trauma
# return <- addcol(return[[1]], return[[2]], "musculoskeletal/trauma", "TL", "musculoskeletal")
# 
# # Haematology/dermatology
# return <- addcol(return[[1]], return[[2]], "haematology", "L1", "haematology")
# return <- addcol(return[[1]], return[[2]], "dermatology", "L1", "dermatology")
# 
# # Gynaecology/breast
# return <- addcol(return[[1]], return[[2]], "gynaecological disorder (not cancer)", "L1", "gynaecological")
# return <- addcol(return[[1]], return[[2]], "breast disease (not cancer)", "L1", "breast_disease")
# return <- addcol(return[[1]], return[[2]], "obstetric problem", "L1", "obstetric")
# 
# # Immunological/systemic disorders
# return <- addcol(return[[1]], return[[2]], "vasculitis", "L2", "vasculitis")
# return <- addcol(return[[1]], return[[2]], "systemic lupus erythematosis/sle", "L2", "lupus")
# return <- addcol(return[[1]], return[[2]], "dermatopolymyositis", "L2", "dermatopolymyositis")
# return <- addcol(return[[1]], return[[2]], "immunological/systemic disorders", "TL", "other_immunological")
# 
# # Infections
# return <- addcol(return[[1]], return[[2]], "septicaemia / sepsis", "L1", "sepsis")
# return <- addcol(return[[1]], return[[2]], "HIV", "L2", "HIV")
# return <- addcol(return[[1]], return[[2]], "viral infection", "L1", "other_viral")
# return <- addcol(return[[1]], return[[2]], "tuberculosis (tb)", "L2", "TB")
# return <- addcol(return[[1]], return[[2]], "scarlet fever / scarlatina", "L2", "scarlet_fever")
# return <- addcol(return[[1]], return[[2]], "bacterial infection", "L1", "other_bacterial")
# return <- addcol(return[[1]], return[[2]], "tropical & travel-related infections", "L1", "tropical_infections")
# 
# # Unclassifiable
# return <- addcol(return[[1]], return[[2]], "unclassifiable", "TL", "unclassifiable")
# 
# 
# 
# # View(return[[2]])
# # View(return[[1]])
# 
# pts <- return[[1]]
# pts <- unique(pts)
# colSums(pts[,c(-1)], na.rm=TRUE)
# sum(pts$other_hypertension, na.rm=TRUE)
# length(unique(pts$ID))
# 
# saveRDS(pts, file="K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Hypertension\\VIhypIllnesses.rds")
