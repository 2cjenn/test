# Jennifer Collister
# 23/11/20
# Count the number of participants taking each medication in the verbal interview list at baseline (20003.0.*)

library(yaml)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")

source(file.path(config$scripts$cleaning, "dataset_generator.R"))

mappings <- list(list(name="HES_ICD9Diag", fid=41271, measurement=46, coding=87),
              list(name="HES_ICD10Diag", fid=41270, measurement=212, coding=19),
              list(name="HES_OPCS3Code", fid=41273, measurement=15, coding=259),
              list(name="HES_OPCS4Code", fid=41272, measurement=116, coding=240),
              list(name="VeI_NonCancerCode", fid=20002, measurement=33, coding=6),
              list(name="VeI_OperationCode", fid=20004, measurement=31, coding=5),
              list(name="Dth_ICD10Underlying", fid=40001, measurement=1, coding=19),
              list(name="Dth_ICD10Secondary", fid=40002, measurement=14, coding=19)
)

for(map in mappings) {
  print(map$name)
  data <- DBfunc$DB_extract(extract_cols = c("ID", paste0(glue("{map$name}.0."), seq(0,map$measurement,by=1))),
                            db = "K:/TEU/UKB33952_Data/Data_Downloads/V3_database_duckdb0.2.1/ukb_v3.db",
                            name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")
  
  icd_counts <- data %>% 
    pivot_longer(cols = starts_with(glue("{map$name}.0."))) %>%
    select(-name) %>%
    filter(!is.na(value)) %>%
    count(value)
    
  
  medcodes <- read.csv(glue("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding{map$coding}_flattened.csv")) %>%
    mutate(Code = as.character(Code))
  
  medcodes <- left_join(medcodes, icd_counts, by = c("Code" = "value"))
  
  write.csv(medcodes, 
            file=glue("K:/TEU/UKB33952_Data/Data_Dictionary/Mappings/Encoding_files/coding{map$coding}_counts.csv"),
            na="0")

}
  