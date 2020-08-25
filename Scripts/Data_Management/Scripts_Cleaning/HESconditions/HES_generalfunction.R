#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
library(yaml)
library(data.table)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------

HESdiag <- function(datapath, ICDcodelist, codelength=nchar(ICDcodelist[1]), first=TRUE){
  data <- readRDS(datapath) %>%
    filter(substr(Code, 1, codelength) %in% ICDcodelist)
  if(first){
    data <- data %>% 
      group_by(ID) %>% 
      filter(Date == min(Date))
  }
  return(data)
}

incident <- function(data,
                     recdatepath=file.path(config$data$derived, "basechar.rds")){
  BaC <- readRDS(recdatepath)
  data <- merge(data, BaC[,c("ID", "recdate", "dob")], by="ID", order=FALSE)
  data <- data[data$Date > data$recdate,]
  data <- data[data$Date < Sys.Date(),]
}

HES_condition <- function(ICD10codes, ICD9codes, filename, 
                          mapping="L2", 
                          incident=FALSE,
                          colprefix=NULL,
                          ICD10file=file.path(config$data$derived, "ICD10codes.rds"),
                          ICD9file=file.path(config$data$derived, "ICD9codes.rds")
                          ){
  
  mapping10 <- readRDS(ICD10file) %>%
    select(ICD10, Type = as.name(mapping))
  
  mapping9 <- readRDS(ICD9file) %>%
    select(ICD9, Type = as.name(mapping))
  
  ICD10 <- HESdiag(file.path(config$data$derived, "HES_ICD10.rds"), ICDcodelist=ICD10codes) %>%
    left_join(mapping10, by=c("Code"="ICD10"))
  
  ICD9 <- HESdiag(file.path(config$data$derived, "HES_ICD9.rds"), ICDcodelist=ICD9codes) %>%
    left_join(mapping9, by=c("Code"="ICD9"))
  
  ICD <- rbind(ICD10, ICD9) %>% 
    group_by(ID) %>% 
    filter(Date == min(Date), rank(Code, ties.method="first")==1) %>% # Note use of rank to take one row when multiple minima
    mutate(ICD = factor(ICD),
           Code = factor(Code),
           Type = factor(Type))
  
  if(incident){
    ICD <- incident(ICD)
  }
  
  if(!is.null(colprefix)){
    oldnames <- c("Date", "Code", "Type")
    newnames <- paste0(colprefix, "_", tolower(oldnames))
    ICD <- ICD %>% rename_at(all_of(oldnames), ~ newnames)
  }
  saveRDS(ICD, file.path(filename))
}


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
MI_ICD9 <- c()
chronic_ICD9 <- c()
heart_ICD9 <- c(MI_ICD9, chronic_ICD9)

CVD_ICD10 <- c(stroke_ICD10, heart_ICD10)
CVD_ICD9 <- c(stroke_ICD9, heart_ICD9)

HES_condition(ICD10codes = CVD_ICD10,
              ICD9codes = CVD_ICD9,
              filename=file.path(config$data$derived, "CVD1_HESevents.rds"),
              colprefix = "CVD")

CVD <- readRDS(file.path(config$data$derived, "CVD1_HESevents.rds"))

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
MI_ICD9 <- c()
chronic_ICD9 <- c()
angina_ICD9 <- c()
heart_ICD9 <- c(MI_ICD9, chronic_ICD9, angina_ICD9)

# ICD 10 codes for other atherosclerotic disease
pervasc_ICD10 <- c("I70", "I73")
# ICD 9 codes for other atherosclerotic disease
pervasc_ICD9 <- c()

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
