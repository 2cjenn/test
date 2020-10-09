# Jennifer Collister
# 09/10/2020

library(yaml)
library(glue)

config = yaml.load_file("config.yml")
source(config$functions)
source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

exclusions <- function(data){
  
  return(data)
}

data <- derive_variables(database = "K:/TEU/UKB33952_Data/Data_Downloads/V3_database/ukb_v3.db", 
                         field_definitions = TEU_SPECS$Cholesterol_PRS, 
                         exclusions = exclusions)

con <- dbConnect(duckdb::duckdb(), db)


# List all tables available in the database
# Each should correspond to one data download
tables <- dbListTables(con)

# Join all download tables to get all data and extract requested columns
view <- lapply(tables, function(x) tbl(con, from=x)) %>% 
  reduce(inner_join, by = "f.eid", suffix = c("", ".del")) %>%
  filter(f.eid==6025392) %>%
  collect

bd_sans_NA_cols <- view[!map_lgl(view, ~ all(is.na(.)))]

View(view[is.na(view$f.54.0.0),])
View(view[view$f.eid==6025392])
View(data[data$ID==6025392,])

6025392 %in% old_data$ID

dbDisconnect(con, shutdown=TRUE)
