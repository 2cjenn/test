#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 04/09/20
# Create a general function to extract conditions from the HES data
# Takes ICD9 and ICD10 codes, can return
# - Incident/prevalent/all outcomes
# Currently returns first outcome per individual - modify to have option of first per code? all outcomes?
# Currently saves a dataframe to rds, also invisibly returns the dataframe
# Returns condition labels according to selected level of hierarchical mapping
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
library(yaml)
library(data.table)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------

HESdiag <- function(datafile, ICDcodelist, codelength=nchar(ICDcodelist[1]), 
                    mapping_level, mapping_file, first=FALSE){
  # Reads the hierarchical ICD code mapping file
  # Retains only the selected level of labelling
  mapdf <- readRDS(mapping_file) %>%
    select(ICD_Code, Type = as.name(mapping_level))
  
  # Keeps only records containing the ICD codes of interest
  # Joins to the 
  data <- datafile %>%
    filter(substr(Code, 1, codelength) %in% ICDcodelist) %>%
    left_join(mapdf, by=c("Code"="ICD_Code"))
  
  if(first){
    # Filters to first diagnosis per individual
    data <- data %>% group_by(ID) %>% 
      filter(Date == min(Date), rank(Code, ties.method="first")==1)
  }
  return(data)
}

get_incident <- function(data,
                         recdatepath=file.path(config$data$derived, "basechar.rds")){
  # Returns only diagnoses prior to baseline assessment
  BaC <- readRDS(recdatepath)
  data <- merge(data, BaC[,c("ID", "recdate", "dob")], by="ID", order=FALSE)
  data <- data[data$Date > data$recdate,]
  data <- data[data$Date < Sys.Date(),] %>%
    select(-recdate, -dob)
}

get_prevalent <- function(data,
                          recdatepath=file.path(config$data$derived, "basechar.rds")){
  # Returns only diagnoses which occurred after baseline assessment
  BaC <- readRDS(recdatepath)
  data <- merge(data, BaC[,c("ID", "recdate", "dob")], by="ID", order=FALSE)
  data <- data[data$Date <= data$recdate,] %>%
    select(-recdate, -dob)
}

HES_condition <- function(ICD10codes, ICD9codes, filename, 
                          mapping="L2", 
                          incident=FALSE, 
                          prevalent=FALSE,
                          colprefix=NULL,
                          ICD10_mapping=file.path(config$data$derived, "ICD10codes.rds"),
                          ICD9_mapping=file.path(config$data$derived, "ICD9codes.rds"),
                          recordlevel=FALSE){
  if(recordlevel){
    ICD9file <- readRDS(file.path(config$data$derived, "HES_recordlevelICD9.rds")) %>%
      select(ID, ICD, Code, Date)
    ICD10file <- readRDS(file.path(config$data$derived, "HES_recordlevelICD10.rds")) %>%
      select(ID, ICD, Code, Date)
  } else {
    ICD9file <- readRDS(file.path(config$data$derived, "HES_ICD9.rds"))
    ICD10file <- readRDS(file.path(config$data$derived, "HES_ICD10.rds"))
  }
  
  ICD9 <- HESdiag(datafile=ICD9file, 
                  ICDcodelist=ICD9codes, 
                  mapping_level=mapping, mapping_file=ICD9_mapping, 
                  first=FALSE)
  
  ICD10 <- HESdiag(datafile=ICD10file,
                   ICDcodelist=ICD10codes,
                   mapping_level=mapping, mapping_file=ICD10_mapping, 
                   first=FALSE)
  
  # Join ICD9 and ICD10 diagnoses together
  ICD <- rbind(ICD10, ICD9) %>% 
    mutate(ICD = factor(ICD),
           Code = factor(Code),
           Type = factor(Type))
  
  if(incident){
    ICD <- get_incident(ICD)
  }
  if(prevalent){
    ICD <- get_prevalent(ICD)
  }
  
  ICD <- ICD %>% group_by(ID) %>% 
    filter(Date == min(Date), rank(Code, ties.method="first")==1) 
  # Note use of rank to take one row when multiple minima
  
  if(!is.null(colprefix)){
    # Rename columns as specified by supplied prefix
    oldnames <- c("Date", "Code", "Type")
    newnames <- paste0(colprefix, "_", tolower(oldnames))
    ICD <- ICD %>% rename_at(all_of(oldnames), ~ newnames)
  }
  
  saveRDS(ICD, file.path(filename))
  
  # Invisibly return the dataframe
  # "This function can be useful when it is desired to have functions return values which can be assigned, 
  # but which do not print when they are not assigned."
  invisible(ICD)
}

#---------------------------------------------------------------------------------------------------------
# Use this function for various conditions - should pull these out into separate scripts/part of other scripts?
#---------------------------------------------------------------------------------------------------------
# Dementia

# ICD 10 codes for dementia
alz_ICD10 <- c("F00")
vasc_ICD10 <- c("F01")
other_ICD10 <- c("F02", "F03")
dement_ICD10 <- list(alz_ICD10, vasc_ICD10, other_ICD10)
# ICD 9 codes for dementia
alz_ICD9 <- c("3310")
vasc_ICD9 <- c("2904")
other_ICD9 <- c("2900", "2901", "2902", "2903", "2941", "2942", "3311")
dement_ICD9 <- c(alz_ICD9, vasc_ICD9, other_ICD9)

HES_condition(ICD10codes = dement_ICD10,
              ICD9codes = dement_ICD9,
              filename=file.path(config$data$derived, "dementia.rds"),
              colprefix = "dement")

dementia <- readRDS(file.path(config$data$derived, "dementia.rds"))

#---------------------------------------------------------------------------------------------------------
# Stroke

# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
haem_ICD9 <- c("430", "431")
infarct_ICD9 <- c("433")
other_ICD9 <- c("434","436")
stroke_ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9)

HES_condition(ICD10codes = stroke_ICD10,
              ICD9codes = stroke_ICD9,
              filename=file.path(config$data$derived, "stroke.rds"),
              colprefix = "stroke")

stroke <- readRDS(file.path(config$data$derived, "stroke.rds"))

#---------------------------------------------------------------------------------------------------------
# CVD first pass

# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
haem_ICD9 <- c("430", "431")
infarct_ICD9 <- c("433")
other_ICD9 <- c("434","436")
stroke_ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9)

# ICD 10 codes for ischemic heart disease
MI_ICD10 <- c("I21")
chronic_ICD10 <- c("I25")
heart_ICD10 <- c(MI_ICD10, chronic_ICD10)
# ICD 9 codes for ischemic heart disease
MI_ICD9 <- c("410", "411")
chronic_ICD9 <- c("414")
heart_ICD9 <- c(MI_ICD9, chronic_ICD9)

CVD_ICD10 <- c(stroke_ICD10, heart_ICD10)
CVD_ICD9 <- c(stroke_ICD9, heart_ICD9)

prior <- HES_condition(ICD10codes = CVD_ICD10,
                       ICD9codes = CVD_ICD9,
                       prevalent=TRUE,
                       filename=file.path(config$data$derived, "CVD1prior_HESevents.rds"),
                       colprefix = "CVDprior",
                       recordlevel=FALSE)

post <- HES_condition(ICD10codes = CVD_ICD10,
                      ICD9codes = CVD_ICD9,
                      incident=TRUE,
                      filename=file.path(config$data$derived, "CVD1_HESevents.rds"),
                      colprefix = "CVD",
                      recordlevel=FALSE)

recordlevel <- HES_condition(ICD10codes = CVD_ICD10,
                      ICD9codes = CVD_ICD9,
                      incident=TRUE,
                      filename=file.path(config$data$derived, "CVD1_HESevents.rds"),
                      colprefix = "CVD",
                      recordlevel=TRUE)

#---------------------------------------------------------------------------------------------------------
# CVD later analyses

# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
occl_ICD10 <- c("I65", "I66")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10, occl_ICD10)
# ICD 9 codes for stroke
haem_ICD9 <- c("430", "431")
infarct_ICD9 <- c("433")
other_ICD9 <- c("434","436")
occl_ICD9 <- c()
stroke_ICD9 <- c(haem_ICD9, infarct_ICD9, other_ICD9, occl_ICD9)

# ICD 10 codes for ischemic heart disease
MI_ICD10 <- c("I21")
chronic_ICD10 <- c("I25")
angina_ICD10 <- c("I20")
heart_ICD10 <- c(MI_ICD10, chronic_ICD10, angina_ICD10)
# ICD 9 codes for ischemic heart disease
MI_ICD9 <- c("410", "411")
chronic_ICD9 <- c("414")
angina_ICD9 <- c("413")
heart_ICD9 <- c(MI_ICD9, chronic_ICD9, angina_ICD9)

# ICD 10 codes for other atherosclerotic disease
pervasc_ICD10 <- c("I70", "I73")
# ICD 9 codes for other atherosclerotic disease
pervasc_ICD9 <- c("440", "441")

CVD_ICD10 <- c(stroke_ICD10, heart_ICD10, pervasc_ICD10)
CVD_ICD9 <- c(stroke_ICD9, heart_ICD9, pervasc_ICD9)

HES_condition(ICD10codes = CVD_ICD10,
              ICD9codes = CVD_ICD9,
              filename=file.path(config$data$derived, "CVD_HESevents.rds"),
              colprefix = "CVD")

CVD <- readRDS(file.path(config$data$derived, "CVD_HESevents.rds"))

#---------------------------------------------------------------------------------------------------------
# ALS

# ICD 10 codes for ALS (motor neurone disease)
als_ICD10 <- c("G122")
# ICD 9 codes for ALS
als_ICD9 <- c("3352")

HES_condition(ICD10codes = als_ICD10,
              ICD9codes = als_ICD9,
              filename=file.path(config$data$derived, "als_incident.rds"),
              incident=TRUE)

als <- readRDS(file.path(config$data$derived, "als_incident.rds"))

#---------------------------------------------------------------------------------------------------------
# Parkinson

# ICD 10 codes for Parkinson disease
park_ICD10 <- c("G20")
# ICD 9 codes for Parkinson disease
park_ICD9 <- c("3320")

HES_condition(ICD10codes = park_ICD10,
              ICD9codes = park_ICD9,
              filename=file.path(config$data$derived, "parkinson_incident.rds"),
              incident=TRUE)

park <- readRDS(file.path(config$data$derived, "parkinson_incident.rds"))

#---------------------------------------------------------------------------------------------------------
# Hyperlipidaemia

# ICD 10 codes for Parkinson disease
chol_ICD10 <- c("E78")
# ICD 9 codes for Parkinson disease
chol_ICD9 <- c("272")

chol <- HES_condition(ICD10codes = chol_ICD10,
                      ICD9codes = chol_ICD9,
                      filename=file.path(config$data$derived, "highchol_prevalent.rds"),
                      prevalent=TRUE,
                      colprefix = "chol")


#---------------------------------------------------------------------------------------------------------
# Testing stuffs

recordlevel <- readRDS(file.path(config$data$derived, "HES_recordlevelICD10.rds")) %>%
  select(ID, ICD, Code, Date)

summary <- readRDS(file.path(config$data$derived, "HES_ICD10.rds")) %>%
  filter(!is.na(Code))

base <-readRDS(file.path(config$data$derived, "basechar.rds"))
withdrawn <- read.table(file.path(config$cleaning$withdrawals, "w33952_20200820.csv"))
missing <- unique(summary$ID[!summary$ID %in% recordlevel$ID])

missing <- missing[!missing %in% withdrawn$V1]
missing %in% base$ID[!is.na(base$lfudate)]

test <- anti_join(summary, recordlevel, by=c("ID", "ICD", "Code", "Date"))
test <- test[!test$ID %in% withdrawn$V1,]
missing <- (unique(test$ID))

missing %in% recordlevel$ID
View(recordlevel[recordlevel$ID %in% missing,])

codes <- test[test$code %in% recordlevel$code]
