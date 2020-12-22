# Jennifer Collister
# 22/09/20

library(yaml)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")


source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

# db = "K:/TEU/UKB33952_Data/Data_Downloads/V3_database_duckdb0.2.1/ukb_v3.db"
# con <- duckdb::dbConnect(duckdb::duckdb(), db)
# dbDisconnect(con, shutdown=TRUE)

data <- DBfunc$DB_extract(extract_cols = c("ID", paste0("VeI_MedCode.0.", seq(0,47,by=1))),
                          db = "K:/TEU/UKB33952_Data/Data_Downloads/V2_database_duckdb0.2.1/ukb_v2.db",
                          name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")

med_counts <- data %>% 
  pivot_longer(cols = starts_with("VeI_MedCode.0.")) %>%
  select(-name) %>%
  filter(!is.na(value)) %>%
  count(value)
                              


tables <- dbListTables(con)
view <- lapply(tables, function(x) tbl(con, from=x)) %>% 
  reduce(inner_join, by = "f.eid", suffix = c("", ".delete")) %>%
  select("f.eid", "f.20277.0.0") %>%
  filter(f.eid != 6025392) %>% # NOTE: HACKY FIX TO DEAL WITH BROKEN PARTICIPANT
  collect

icd9_codes <- c("430", "431")
icd10_codes <- c("I60", "I61","I63", "I64")

op_codes <- c("K40", "K41", "K42", "K43", "K44", "K45", "K46", "K49", "K50", "K533", "K75", "K76", 
              "L29", "L303", "L311", "L313", "L314", "L318", "L319")

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

ops <- FN_HEStoLong(
  data,
  colname = "HES_OPCS4",
  removeNAfrom = "Code"
) 
