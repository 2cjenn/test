#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister
# Load the UKB data, rename the variables and split it into manageable separate tables
#--------------------------------------------------------------------------------------------------------------


library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(reshape)
library(readxl)
library(lme4)
library(sqldf)
library(rstudioapi)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# First make sure the biobank data has been loaded, if not then load it
if(exists("bd") && is.data.frame(get("bd"))) {
  print("Good to go")
} else {
  print("Please wait a moment")
  source("K:\\TEU\\CancerPRS\\5thData_15Oct2019\\SelectedConvR_114fields\\ukb37981.r")
  rm(list=(ls()[ls()!="bd"]))
  print("Ok, ready!")
}

#--------------------------------------------------------------------------------------------------------------
# Prep for labelling instances
# Find instance type per data field
dataProperties <- read_excel("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\DataFieldProperties.xlsx")
instances <- read_excel("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\instances.xlsx")
instvalues <- read_excel("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\insvalue.xlsx")
dataInstances <- left_join(dataProperties[,c("field_id", "instance_id")], instvalues[,c("instance_id", "index", "title")], by="instance_id")

# Split the column names into data code, instance and measurement
cols <- data.frame("full"=colnames(bd)[-1])
cols <- separate(cols, col=full, into=c("f", "code", "instance", "measurement"), convert=TRUE, remove=FALSE, fill="right")
# Find instance type per data code
cols <- left_join(cols, dataInstances, by=c("code" = "field_id", "instance" = "index"))
cols$instlabel <- ifelse(is.na(cols$title), cols$instance, cols$title)

# Read in the data dictionary 
matching <- read.csv("K:\\TEU\\CancerPRS\\Data_Dictionary\\Renaming_List_UPDATE_Nov2019_TEU.csv")
# # Use this to find the codes that were in our data but not Alex's; add them to the matching list
antis <- anti_join(cols, matching, by=c("code" = "Field_ID"))
unique(antis$code)
# Join the column codes to the list of data names to get user-friendly variable names
cols <- inner_join(cols, matching, by=c("code" = "Field_ID"))
cols$newcols <- paste0(cols$NewVarName, ".", cols$instance, ".", cols$measurement)

# Rename the columns of data with the user-friendly strings
bdnew <- bd
names(bdnew) <- cols$newcols[match(names(bd), cols$full)]
colnames(bdnew)[1] <- "ID"

# Count the NAs in each column - data completeness
# colSums(!is.na(bdnew))

#--------------------------------------------------------------------------------------------------------------
# Find maximum instance and measurement per data code
# meas.agg <- aggregate(measurement~code, cols, max)
# inst.agg <- aggregate(instance~code, cols, max)
cols.max <- merge(aggregate(instance~code, cols, max), merge(aggregate(measurement~code, cols, max), cols))

# Split out the data into tables with variables grouped by instances/measurements
# Rename columns so if either instance or variable is only ever 0 then it isn't in the colname
# Make sure each table has the ID in!
combos <- unique(cols.max[,c("instance", "measurement")])
combos <- combos[order(combos$instance, combos$measurement),]
ddat <- as.list(rep("",nrow(combos)))
for(row in 1:nrow(combos)) {
  choosecodes <- cols.max[(cols.max$instance==combos$instance[row] & cols.max$measurement==combos$measurement[row]),"code", drop=FALSE]
  choosecols <- merge(choosecodes, cols)
  if(combos$instance[row]==0 & combos$measurement[row]==0){
    choosecols$colname <- choosecols$NewVarName
  } else if(combos$instance[row]==0){
    choosecols$colname <- paste0(choosecols$NewVarName, ".m", choosecols$measurement)
  } else if(combos$measurement[row]==0){
    choosecols$colname <- paste0(choosecols$NewVarName, ".i", choosecols$instance)
  } else {
    choosecols$colname <- paste0(choosecols$NewVarName, ".m", choosecols$measurement, ".i", choosecols$instance)
  }
  ddat[[row]] <- data.frame(bdnew[,c("ID", choosecols$newcols)])
  names(ddat[[row]]) <- choosecols$colname[match(names(ddat[[row]]), choosecols$newcols)]
  colnames(ddat[[row]])[1] <- "ID"
}

rm(list=(ls()[ls()!="ddat"]))

#--------------------------------------------------------------------------------------------------------------
# Need to manually inspect these tables and create appropriate usable tables
# This includes further categorisation, and reshaping from wide to long
# After reshaping, rows with all NA are removed - these can be recreated when merging but no point storing empty data?
# (eg no point keeping cause of death = NA when individual not yet dead) 
#--------------------------------------------------------------------------------------------------------------
# ddat[[1]] - instance 0, measure 0
# baseline characteristics
t.baseline <- ddat[[1]]
save("t.baseline", file="t_baseline.Rdata")

# ddat[[2]] - instance 0, measure 0, 1
# mental category
t.mentalcat <- ddat[[2]]
save("t.mentalcat", file="t_mentalcat.Rdata")

# ddat[[3]]
# Diagnoses: ICD-9 Main

# ddat[[4]]
# Diagnoses: ICD-9 Secondary

# ddat[[5]] - measures up to 40
# Genetic principal components
t.pc <- ddat[[5]]
save("t.pc", file="t_pc.Rdata")
# t.pc <- reshape(t.pc, varying=sort(colnames(t.pc[,-1])), direction="long", idvar="ID", sep=".m")

# ddat[[6]]
# Diagnoses: ICD-10 Main
t.1diagnoses <- ddat[[6]]
save("t.1diagnoses", file="t_1diagnoses.Rdata")

# ddat[[7]]
# Diagnoses: ICD-10 Secondary
t.2diagnoses <- ddat[[7]]
save("t.2diagnoses", file="t_2diagnoses.Rdata")

# ddat[[8]]
# Diagnoses: ICD-10 Summary across primary and secondary
t.diagnoses <- ddat[[8]]
save("t.diagnoses", file="t_diagnoses.Rdata")

# ddat[[9]]
# Death registries, underlying cause and date
t.death <- ddat[[9]]
save("t.death", file="t_death.Rdata")

# ddat[[10]] - instances 0, 1, measures up to 13
# Death registries, secondary cause
t.dsecondary <- ddat[[10]]
save("t.dsecondary", file="t_dsecondary.Rdata")

# ddat[[11]] - instances 0, 1, 2
t.rep2meas <- ddat[[11]]
save("t.rep2meas", file="t_rep2meas.Rdata")

# ddat[[12]] - instances 0, 1, 2, 3
# Repeated measures data, recorded once at each repeat visit
t.repmeas <- ddat[[12]]
t.repmeas <- reshape(t.repmeas, varying=sort(colnames(t.repmeas[,-1])), direction="long", idvar="ID", sep=".i")
t.repmeas <- dplyr::rename(t.repmeas, instance = time)
save("t.repmeas", file="t_repmeas.Rdata")

# ddat[[12]] - instances 0, 1, 2, measures up to 1
# Blood pressure
t.bp <- ddat[[13]]
t.bp <- reshape(t.bp, varying=sort(colnames(t.bp[,-1])), direction="long", idvar="ID", sep=".i")
t.bp <- dplyr::rename(t.bp, instance = time)
t.bp <- reshape(t.bp, varying=sort(colnames(t.bp[,-1:-2])), direction="long", idvar=c("ID", "instance"), sep=".m")
t.bp <- dplyr::rename(t.bp, measurement = time)
save("t.bp", file="t_bp.Rdata")
#t.bp <- t.bp[rowSums(is.na(t.bp[,grep("BlP_", colnames(t.bp))])) !=length(grep("BlP_", colnames(t.bp))),]

# ddat[[13]] - instances 0, 1, 2, measures 0, 1, 2
# Medication for high BP, cholesterol, diabetes
t.medsM <- ddat[[14]]
t.medsM <- reshape(t.medsM, varying=sort(colnames(t.medsM[,-1])), direction="long", idvar="ID", sep=".i")
t.medsM <- dplyr::rename(t.medsM, instance = time)
t.medsM <- reshape(t.medsM, varying=sort(colnames(t.medsM[,-1:-2])), direction="long", idvar=c("ID", "instance"), sep=".m")
t.medsM <- dplyr::rename(t.medsM, measurement = time)
save("t.medsM", file="t_medsM.Rdata")

# ddat[[14]] - instances 0, 1, 2, measures 0, 1, 2, 3
# Medication for high BP, cholesterol, diabetes and hormone therapy
t.medsF <- ddat[[15]]
t.medsF <- reshape(t.medsF, varying=sort(colnames(t.medsF[,-1])), direction="long", idvar="ID", sep=".i")
t.medsF <- dplyr::rename(t.medsF, instance = time)
t.medsF <- reshape(t.medsF, varying=sort(colnames(t.medsF[,-1:-2])), direction="long", idvar=c("ID", "instance"), sep=".m")
t.medsF <- dplyr::rename(t.medsF, measurement = time)
save("t.medsF", file="t_medsF.Rdata")

# ddat[[15]]
# Verbal interview non-cancer illness codes
t.verbaldiag <- ddat[[16]]
t.verbaldiag <- reshape(t.verbaldiag, varying=sort(colnames(t.verbaldiag[,-1])), direction="long", idvar="ID", sep=".i")
t.verbaldiag <- dplyr::rename(t.verbaldiag, instance = time)
t.verbaldiag <- reshape(t.verbaldiag, varying=sort(colnames(t.verbaldiag[,-1:-2])), direction="long", idvar=c("ID", "instance"), sep=".m")
t.verbaldiag <- dplyr::rename(t.verbaldiag, measurement = time)

coding6 <- read.table("coding6.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)
t.verbaldiag <- left_join(t.verbaldiag, coding6, by=c("VeI_NonCancerCode"="coding"))
save("t.verbaldiag", file="t_verbaldiag.Rdata")

# ddat[[16]]
# Verbal interview medication codes
t.verbalmeds <- ddat[[17]]
t.verbalmeds <- reshape(t.verbalmeds, varying=sort(colnames(t.verbalmeds[,-1])), direction="long", idvar="ID", sep=".i")
t.verbalmeds <- dplyr::rename(t.verbalmeds, instance = time)
t.verbalmeds <- reshape(t.verbalmeds, varying=sort(colnames(t.verbalmeds[,-1:-2])), direction="long", idvar=c("ID", "instance"), sep=".m")
t.verbalmeds <- dplyr::rename(t.verbalmeds, measurement = time)

coding4 <- read.table("coding4.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)
t.verbalmeds <- left_join(t.verbalmeds, coding4, by=c("VeI_MedCode"="coding"))

#--------------------------------------------------------------------------------------------------------------
# Save these "tables" into an .Rdata file so other scripts can load them without spending time creating them
save(list=c("t.diagnoses", "t.1diagnoses", "t.2diagnoses", "t.pc", "t.baseline", "t.mentalcat", "t.verbalmeds", 
            "t.bp", "t.repmeas", "t.death", "t.dsecondary", "t.medsM", "t.medsF", "t.verbaldiag")
      , file="tables.Rdata", compress=FALSE)

save(list=c("t.diagnoses", "t.baseline","t.verbalmeds", "t.bp", "t.repmeas", "t.death", "t.medsM", "t.medsF", "t.verbaldiag")
     , file="selectedtables.Rdata", compress=FALSE)



