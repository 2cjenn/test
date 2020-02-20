#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister - 07/11/2019
# Address the questions raised in the meeting on 31/10/19
#--------------------------------------------------------------------------------------------------------------


# The APOE SNP data is located in K:\TEU\UKB Genetic Data\SNP Extraction\APOE_20191018
# The .gen file has been converted to .ped and read into R using the scripts in K:\TEU\UKB Genetic Data\SNP Extraction\APOE_20191018\extracted_files\R
# The raw genotype data has been converted into e2/e3/e4 haplotype data, using GeneticData_JC.R
# Load in the resulting APOE data

load("K:/TEU/UKB Genetic Data/SNP Extraction/APOE_20191018/extracted_files/R/gen_apoe.Rdata")

# Check prevalence of APOE e4 allele - expect ~20% population carriers, ~1.8% homozygotes
for (i in 1:4){
  apoe_col = paste0("apoe", i)
  tab_e <- table(t.apoe_genetic[[apoe_col]], useNA="ifany")
  print(tab_e)
  print(100*prop.table(table(t.apoe_genetic[[apoe_col]])))
}
prop.table(table(t.apoe_genetic$apoe_gen))

#--------------------------------------------------------------------------------------------------------------
# First approach: an e4 dosage model [1]
e4dosage <- t.apoe_genetic
e4dosage <- e4dosage[!is.na(e4dosage$apoe4),]
#	Exclude participants with e2/e4 genotype (combination of protective and risk alleles)
e4dosage <- e4dosage[!(e4dosage$apoe2==1 & e4dosage$apoe4==1),]
#	Exclude participants with e1 as little known about it
e4dosage <- e4dosage[e4dosage$apoe1==0,]
#	e2/e2, e2/e3, e3/e3 coded as 0
#	e3/e4 coded as 1
#	e4/e4 coded as 2
e4dosage$dosage <- e4dosage$apoe4

#--------------------------------------------------------------------------------------------------------------
# Dementia diagnoses
load("K:/TEU/APOE on Dementia/Data Management/5thData_15Oct2019/SelectedConvR_114fields/dementia.Rdata")

analysis <- merge(x=e4dosage, y=dementia, by.x="IID", by.y="ID", all.x = TRUE)
dosetab <- table(analysis$dosage, is.na(analysis$HES_ICD10))
dosetab
100*prop.table(dosetab, margin=1)
chisq.test(dosetab)

load("K:/TEU/APOE on Dementia/Data Management/5thData_15Oct2019/SelectedConvR_114fields/dementia.Rdata")
