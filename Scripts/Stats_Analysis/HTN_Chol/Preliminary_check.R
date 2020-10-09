# Jennifer Collister
# 09/10/2020

library(yaml)
library(glue)

config = yaml.load_file("config.yml")
source(config$functions)
source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

exclusions <- function(data){
  
  data <- data[!is.na(data$TEU_LDL_C_PRS),]
  
  return(data)
}

data <- derive_variables(database = "K:/TEU/UKB33952_Data/Data_Downloads/V3_database/ukb_v3.db", 
                         field_definitions = TEU_SPECS$Cholesterol_PRS, 
                         exclusions = exclusions)


