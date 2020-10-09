# Jennifer Collister
# 09/10/2020

library(yaml)
library(glue)

config = yaml.load_file("config.yml")
source(config$functions)
source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

exclusions <- function(data){
  
  data <- data[!is.na(data$TEU_LDL_C_PRS),]
  
  data <- data[!is.na(data$BBC_LDL_Result),]
  
  return(data)
}

data <- derive_variables(database = "K:/TEU/UKB33952_Data/Data_Downloads/V3_database/ukb_v3.db", 
                         field_definitions = TEU_SPECS$Cholesterol_PRS, 
                         exclusions = exclusions)


# Scatter-plot
# plot(data$TEU_LDL_C_PRS, data$BBC_LDL_Result)

# Correlation
cor(data$TEU_LDL_C_PRS, data$BBC_LDL_Result)

# Simple linear regression
model <- lm(BBC_LDL_Result ~ TEU_LDL_C_PRS, data=data)
summary(model)

# Trinder's multivariable linear regression: 
# LDL-C and PRS, adjusted for age, sex, first 4 PCs, genotyping array and batch
trinder <- lm(BBC_LDL_Result ~ TEU_LDL_C_PRS_deciles + TEU_BaC_AgeAtRec + BaC_Sex + 
                GeP_PC_1 + GeP_PC_2 + GeP_PC_3 + GeP_PC_4 + GeP_Batch, 
              data=data)
summary(trinder)


