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

# Read in the raw data
HES <- readRDS(paste0(config$data$received, "HES.rds"))

HESfunc <- function(data, diag_col, date_col, col_remove, ICD_ID){
  # Select the relevant diagnosis and date columns from the HES data
  data %>% select("ID", starts_with(diag_col), starts_with(date_col)) %>%
    # Rename to remove unnecessary repeated prefix
    rename_with(function(x) str_remove(x, col_remove)) %>%
    # Wide to long, note diag column contains diagnoses and dates
    # This throws a warning because it's combining factors with different levels, it's fine
    gather(key=measurement, value=diag, -ID, factor_key=TRUE) %>%
    # Remove rows with NA diagnosis or date
    filter(!is.na(diag)) %>%
    # Separate out measurement column into diagnosis/date and measurement number
    separate(measurement, c("type", "measurement"), sep=".m") %>%
    # Pivot diagnosis/date column into separate diagnosis and date columns
    spread(type, diag) %>%
    # Convert date column to date objects, add a identifier column
    mutate(DateFirst = as.Date(as.numeric(DateFirst), origin="1970-01-01"),
           ICD = ICD_ID) %>%
    # Keep only the first diagnosis of each code per individual
    group_by(ID, Diag) %>%
    filter(DateFirst == min(DateFirst)) %>%
    # Rename columns
    select(ID, ICD,
           Code = Diag,
           Date = DateFirst)
}

saveRDS(HESfunc(data=HES, diag_col="HES_ICD10Diag", date_col="HES_ICD10DateFirst",
                col_remove="HES_ICD10", ICD_ID="ICD10"), 
        file=file.path(config$data$derived, "HES_ICD10.rds"))


saveRDS(HESfunc(data=HES, diag_col="HES_ICD9Diag", date_col="HES_ICD9DateFirst",
                col_remove="HES_ICD9", ICD_ID="ICD9"), 
        file=file.path(config$data$derived, "HES_ICD9.rds"))

