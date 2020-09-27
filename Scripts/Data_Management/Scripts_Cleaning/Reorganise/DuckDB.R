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

ukb_df("ukb40731", path="K:/TEU/UKB33952_Data/Data_Downloads/V2.1_B2007025_R40731/R/",
       dbname="ukb_v2.db", tblname="ukb40731")



col_type <- c(
  "Sequence" = "integer",
  "Integer" = "integer",
  "Categorical (single)" = "character",
  "Categorical (multiple)" = "character",
  "Continuous" = "double",
  "Text" = "character",
  "Date" = "character",
  "Time" = "character",
  "Compound" = "character",
  "Binary object" = "character",
  "Records" = "character",
  "Curve" = "character"
)

spec <- ukb_df_field(fileset = "ukb38358",
                     path = "K:/TEU/UKB33952_Data/Data_Downloads/v2.0_B2006022_R38358") %>%
  mutate(fread_column_type = col_type[col.type])


df_func<- function(skip=0, nrows=25000, header=FALSE, col.names=NULL){
  # Read the data
  if(is.null(col.names)){
    df <- data.table::fread(
      input = "K:/TEU/UKB33952_Data/Data_Downloads/v2.0_B2006022_R38358/R/Alldata/ukb38358.tab",
      sep = "\t",
      header = header,
      colClasses = stringr::str_c(spec$fread_column_type),
      data.table = FALSE,
      showProgress = TRUE,
      skip=skip,
      nrows=nrows
    ) 
  } else{
    df <- data.table::fread(
      input = "K:/TEU/UKB33952_Data/Data_Downloads/v2.0_B2006022_R38358/R/Alldata/ukb38358.tab",
      sep = "\t",
      header = header,
      col.names=col.names,
      colClasses = stringr::str_c(spec$fread_column_type),
      data.table = FALSE,
      showProgress = TRUE,
      skip=skip,
      nrows=nrows
    ) 
  }
  # Apply the UKB R script to format the data
  
}

con <- dbConnect(duckdb::duckdb(), "ukb_v2.db")

for(row in seq(1, 502520, by=25000)){
  
  print(paste0("row ", row))
  
  overwrite <- row==1
  if(overwrite){
    # Coded "ok" (TRUE) as Don't overwrite and "cancel" (FALSE) as Do overwrite
    # so that DEFAULT OPTION is NOT OVERWRITING!
    dontOverwrite <- rstudioapi::showQuestion("Overwrite", 
                                              message="Are you sure you want to overwrite this large, painstakingly created database???", 
                                              ok="Uhoh, wait, no", 
                                              cancel="Yup, I'm sure")
    if(dontOverwrite){
      stop("User decided not to overwrite")
    }
  }
  append <- row!=1
  
  if(row==1){
    df <- df_func(header=TRUE)
    
    col.names <- colnames(df)
  } else if (row>1){
    df <- df_func(skip=row, col.names=col.names)
  }
  
  print("read")
  
  dbWriteTable(con, df, name="ukb", overwrite=overwrite, append=append, temporary=FALSE)
  
  print("written")

}

dbListTables(con)

qry <- dbGetQuery(con, "SELECT COUNT(*) FROM ukb40731")

df <- tbl(con, from="ukb40731") %>% collect
  select(f.eid, f.21.0.0, f.34.0.0, starts_with("f.84."), starts_with("f.87.")) %>% 
  collect




dbDisconnect(con)
