library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(reshape)

setwd("K:\\TEU\\APOE on Dementia\\4thData_10May2019\\SelectedConvR_112fields")

load("tables.Rdata")
load("t_diagnoses.Rdata")

#--------------------------------------------------------------------------------------------------------------
# ICD-10 codes for Dementia and Alzheimers
# Remember that in the data the . are removed from the codes, so F00.1 is stored as F001 etc
AD_ICD <- c("F00", "F000", "F001", "F002", "F009", 
            "G30", "G300", "G301", "G308", "G309")
VD_ICD <- c("F01", "F010", "F011", "F012", "F013", "F018", "F019", 
            "I673")
FTD_ICD <- c("F020", 
             "G310")
AllD_ICD <- c(AD_ICD, VD_ICD, FTD_ICD, 
              "A810", 
              "F02", "F021", "F022", "F023", "F024", "F028", "F03", "F051", "F106", 
              "G311", "G318")

diag1.dementia <- t.1diagnoses[t.1diagnoses$HES_ICD10Main %in% AllD_ICD,]
diag2.dementia <- t.2diagnoses[t.2diagnoses$HES_ICD10Secondary %in% AllD_ICD,]
diag.dementia <- t.diagnoses[t.diagnoses$HES_ICD10 %in% AllD_ICD,]
death1.dementia <- t.death[t.death$Dth_ICD10Underlying %in% AllD_ICD,]
death2.dementia <- t.dsecondary[t.dsecondary$Dth_ICD10Secondary %in% AllD_ICD,]


#--------------------------------------------------------------------------------------------------------------
# Ethnicity
ethnic <- t.repmeas[,c("ID", "instance","Eth_Ethnicity")]
ethnic <- reshape(ethnic, idvar="ID", timevar="instance", direction="wide")
ethnic <- left_join(ethnic, t.baseline[,c("ID", "Gen_Ethnic")], by="ID")
white <- ethnic[ethnic$Eth_Ethnicity.0 %in% c("White", "British", "Irish", "Any other white background"),]
table(ethnic$Eth_Ethnicity.0)

#--------------------------------------------------------------------------------------------------------------
# Medication - different questions for men and women
# Women also get asked about hormone replacement therapy and contraceptive pill
table(t.medsM$HMH_MedCholBPDiab[t.medsM$instance==0])
table(t.medsF$HMH_MedCholBPDiabHorm[t.medsF$instance==0])

# Combine to get a single table with all IDs and whether they were taking BP meds at initial assessment
medBP_M <- aggregate(t.medsM$HMH_MedCholBPDiab[t.medsM$instance==0]=="Blood pressure medication", 
          by=list(t.medsM$ID[t.medsM$instance==0]), FUN=sum, na.rm=TRUE)
medBP_F <- aggregate(t.medsF$HMH_MedCholBPDiabHorm[t.medsF$instance==0]=="Blood pressure medication", 
                     by=list(t.medsM$ID[t.medsF$instance==0]), FUN=sum, na.rm=TRUE)
t.medsBP <- inner_join(medBP_F, medBP_M, by="Group.1")


#--------------------------------------------------------------------------------------------------------------
# Alzheimer's/Dementia
# Find incident cases
recdate <- t.repmeas[t.repmeas$instance==0,c("ID", "Rec_DateAssess")]

recdate <- mutate(recdate, Rec_DateAssess= as.Date(Rec_DateAssess, format= "%Y-%m-%d"))
diag.dementia <- mutate(diag.dementia, HES_ICD10Date= as.Date(HES_ICD10Date, format= "%Y-%m-%d"))

incident <- left_join(diag.dementia, recdate, by="ID")
incident <- incident[incident$HES_ICD10Date<incident$Rec_DateAssess,]

t.verbaldiag[sum(t.verbaldiag$VeI_NonCancerCode==1263, na.rm=TRUE),]

ado <- t.baseline[,c(1,grep("ADO_", colnames(t.baseline)))]
sum(!is.na(ado$ADO_DementiaDate))
ado <- mutate(ado, ADO_DementiaDate= as.Date(ADO_DementiaDate, format= "%Y-%m-%d"))
incident2 <- left_join(ado[,c("ID", "ADO_DementiaDate", "ADO_DementiaSource")], recdate, by="ID")
incident2 <- incident2[incident2$ADO_DementiaDate<incident2$Rec_DateAssess & !is.na(incident2$ADO_DementiaDate),]

anti_join(incident2, incident, by="ID")

diag.dementia[diag.dementia$ID==2267966,]

#--------------------------------------------------------------------------------------------------------------
# Survival

# Create a new dataframe to contain survival times for each individual
survival <- bdnew[,"ID", drop=FALSE]
# Merge in the death and loss to follow-up times by ID
# 4 individuals who have two different causes of death recorded
t.death[t.death$ID %in% t.death$ID[t.death$instance==1],]
# Minimum time of death per individual
t.mindeath <- setNames(aggregate(t.death$Dth_Date, by=list(t.death$ID), min),c("ID","Dth_Date"))
survival <- merge(x=survival, y=t.mindeath, by="ID", all.x=TRUE)
survival <- merge(x=survival, y=t.lost[,c("ID", "LtF_Date")], by="ID", all.x=TRUE)

