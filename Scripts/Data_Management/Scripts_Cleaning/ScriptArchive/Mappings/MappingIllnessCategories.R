#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 24/01/2020
# Identify non-cancer illness codes relative for Neo's hypertension work
#--------------------------------------------------------------------------------------------------------------
library(yaml)

config = yaml.load_file("config.yml")

noncancerillness_mapping <- function(mappath, mapcol="Mapping", outfile) {
  require(reshape2)
  require(readxl)
  # Load the long-format data of participant IDs and verbal interview diagnosis codes
  pts <- readRDS(file.path(config$data$derived, "VIDiagnosisCodes_long.rds"))
  
  # Load the mapped diagnoses data
  diagnoses <- read_excel(mappath)
  diagnoses$Mapping <- gsub(" ", "_", diagnoses[[mapcol]])
  diagnoses <- diagnoses[!is.na(diagnoses$Mapping),]
  
  # Join them
  ptdiagnoses <- merge(pts, diagnoses[,c("coding", "Mapping")], by="coding", all.x=TRUE)
  
  # Save a long version with the earliest year of diagnosis per coded condition
  ptdiagnoses$year[is.na(ptdiagnoses$year)] <- -1
  ptdiagnoses <- aggregate(year ~ ID + coding + Mapping, data=ptdiagnoses, min)
  ptdiagnoses$year[ptdiagnoses$year==-1] <- NA
  saveRDS(ptdiagnoses, file=paste0(outfile,"_long.rds"))
  
  # Pivot to get a column for each mapped condition
  # We don't need the UKB code for each condition any more,
  # but we do need a dummy column filled with 1s so that when we pivot each condition column will be 1/0
  ptdiagnoses$coding <- 1
  # Now dcast from long to wide so we have a column per category
  # ptdiagnoses <- unique(ptdiagnoses[,c("ID", "Mapping", "coding")])
  ptdiagnoses <- dcast(ptdiagnoses, ID ~ Mapping, value.var="coding", fun.aggregate = sum)
  
  # Save the result
  
  saveRDS(ptdiagnoses, file=file.path(outfile,".rds"))
}

#--------------------------------------------------------------------------------------------------------------
# Neo's hypertension study, parallel to Botswana study
#--------------------------------------------------------------------------------------------------------------

# Really serious conditions to be excluded from the data set
noncancerillness_mapping(mappath=file.path(config$cleaning$mapping, "UKBHtn_NonCancerIllness_Mapping.xlsx"),
                         mapcol="Exclude",
                         outfile=file.path(config$data$derived, "VIhypExclude"))


# Hopefully final set of comorbidities of interest
noncancerillness_mapping(mappath=file.path(config$cleaning$mapping, "UKBHtn_NonCancerIllness_Mapping.xlsx"),
                         mapcol="ComorbidityCondition",
                         outfile=file.path(config$data$derived, "VI_HTNcomorb"))

# Alternate diagnoses for "probable" BP med rubric
noncancerillness_mapping(mappath=file.path(config$cleaning$mapping, "UKBHtn_NonCancerIllness_Mapping.xlsx"),
                        mapcol="AlternateDiagnoses",
                          outfile=file.path(config$data$derived, "VIhypAltDiagnoses"))

