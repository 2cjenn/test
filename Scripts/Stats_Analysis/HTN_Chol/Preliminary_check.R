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
                         exclusions = exclusions,
                         dictionary = "LDL-C_PRS_Dict.html")


# 
# # Scatter-plot
# # plot(data$TEU_LDL_C_PRS, data$BBC_LDL_Result)
# 
# # Correlation
# cor(data$TEU_LDL_C_PRS, data$BBC_LDL_Result)
# 
# # Simple linear regression
# model <- lm(BBC_LDL_Result ~ TEU_LDL_C_PRS, data=data)
# summary(model)
# 
# # Simple linear regression
# agesex <- lm(BBC_LDL_Result ~ TEU_LDL_C_PRS_deciles + TEU_BaC_AgeAtRec + BaC_Sex, data=data)
# summary(agesex)
# 
# # Trinder's multivariable linear regression: 
# # LDL-C and PRS, adjusted for age, sex, first 4 PCs, genotyping array and batch
# trinder <- lm(BBC_LDL_Result ~ TEU_LDL_C_PRS_deciles + TEU_BaC_AgeAtRec + BaC_Sex + 
#                 GeP_PC_1 + GeP_PC_2 + GeP_PC_3 + GeP_PC_4 + GeP_Batch, 
#               data=data)
# summary(trinder)

trinderPath <- "K:/TEU/UKB33952_Data/ukblink/App42857"

trinderInfectious <- read.csv(file.path(trinderPath, "ukbreturn2142/Files for Retman", "main_datafile.csv")) %>%
  select(f_eid, starts_with("mvp_ldl"))

trinderExome <- read.csv(file.path(trinderPath, "ukbreturn2381/Files for retman", "summary_dataframe.csv"))

# Sanity check
check <- inner_join(trinderInfectious, trinderExome %>% select(f_eid, mvp_ldl_grs), by="f_eid")
check$diff <- check$mvp_ldl_grs.x - check$mvp_ldl_grs.y
max(check$diff)

linkFile <- read.table(file.path(trinderPath, "ukb33952bridge42857.txt")) %>% rename(ID = V1, f_eid = V2)

xcheck <- inner_join(inner_join(data %>% select(ID, TEU_LDL_C_PRS), linkFile, by="ID"), 
                     trinderData2 %>% select(f_eid, mvp_ldl_grs), by="f_eid") %>%
  mutate(diff = TEU_LDL_C_PRS - mvp_ldl_grs)
max(xcheck$diff)

xcheck$propdiff <- xcheck$diff/223

par(mfrow=c(1,2))
plot(xcheck$mvp_ldl_grs, xcheck$TEU_LDL_C_PRS)
plot(xcheck$TEU_LDL_C_PRS, xcheck$diff)
