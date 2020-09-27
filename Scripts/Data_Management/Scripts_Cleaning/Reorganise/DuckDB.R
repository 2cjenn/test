library(tidyverse)
library(DBI)
library(duckdb)
library(data.table)
library(yaml)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}
source(file.path(config$scripts$cleaning, "Reorganise", "dataset.R"))

# Load the data into the db
ukb_df("ukb38358", path="K:/TEU/UKB33952_Data/Data_Downloads/V2.0_B2006022_R38358/R/Alldata/",
       dbname="ukb_v2.db", tblname="ukb38358")


# Check if it worked
con <- dbConnect(duckdb::duckdb(), "ukb_v2.db")

dbListTables(con)



map_fn <- function(ukb_col) {
  ukb_col <- strsplit(ukb_col, split = ".", fixed = TRUE)
  ukb_col <- sapply(ukb_col, function(x) {
    if (as.character(x[2]) %in% matching$Field_ID) {
      # Swap the field ID for a human-readable variable name
      x[2] <- matching$NewVarName[matching$Field_ID == as.character(x[2])]
    } else {
      print(x[2])
    }
    # Remove the 'f'
    x <- x[-1]
    # Stick it back together with the instances and measurements
    x <- paste(x, collapse = ".")
    return(x)
  })
  return(ukb_col)
}

invert_map <- function(collist) {
  colnames <- sapply(Map(function(p) p(), collist), function(x) x$name)
  colnames <- strsplit(colnames, split = ".", fixed = TRUE)
  colnames <- sapply(colnames, function(x){
    x[1] <- matching$Field_ID[matching$NewVarName == x[1]]
    x <- paste(c("f", x), collapse=".")
    return(x)
  })
  return(colnames)
}



DB_extract <- function(col_list){
  con <- dbConnect(duckdb::duckdb(), "ukb_v2.db")
  on.exit(dbDisconnect(con, shutdown=TRUE))
  
  matching <- read.csv("K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv",
                       stringsAsFactors = FALSE)
  
  pendingtable <- inner_join(tbl(con, from="ukb40731"), tbl(con, from="ukb42633"), by="f.eid") %>% 
    select(invert_map(col_list)) %>%
    collect %>%
    rename_with(map_fn)
}

data <- DB_extract(test)


df <- tbl(con, from="ukb42633") %>% collect
  select(f.eid, f.21.0.0, f.34.0.0, starts_with("f.84."), starts_with("f.87.")) %>% 
  collect

dbDisconnect(con)






objects <- Map(function(f) f(), colnames)


