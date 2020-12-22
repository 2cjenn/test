# Jennifer Collister
# 22/09/20

library(yaml)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")


source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

data <- DBfunc$DB_extract(extract_cols = c("ID", "Rec_DateAssess",
                                           paste0("HES_OpProc_OPCS4.0", seq(0, 116, by=1)),
                                           paste0("HES_OPCS4DateFirst.0", seq(0, 116, by=1))),
                          name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")

icd9_codes <- c("430", "431")
icd10_codes <- c("I60", "I61","I63", "I64")

FN_ICDdx <- function(data, icd9_codes, icd10_codes) {
  ICD9 <- FN_ICDtoLong(
    data %>% select(ID, starts_with("HES_ICD9")),
    colname = "HES_ICD9",
    mapper = name_map
  ) 
  ICD9_cvd <- ICD9 %>%
    filter(str_detect(Diag, paste(icd9_codes, collapse="|")))
  
  ICD10 <- FN_ICDtoLong(
    data %>% select(ID, starts_with("HES_ICD10")),
    colname = "HES_ICD10",
    mapper = name_map
  )
  ICD10_cvd <- ICD10 %>%
    filter(str_detect(Diag, paste(icd10_codes, collapse="|")))
  
  ICD_cvd <- rbind(ICD9_cvd,ICD10_cvd)
}

ops <- FN_ICDtoLong(
  data %>% select(ID, starts_with("HES_ICD9")),
  colname = "HES_ICD9",
  mapper = name_map
) 