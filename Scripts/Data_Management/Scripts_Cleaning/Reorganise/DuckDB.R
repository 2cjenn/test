


#' Reads a database of UKB data and returns a dataset containing the requested columns
#'
#' The database contains the data with field codes instead of variable names. It has been pre-processed by the UKB-generated R file to apply categorical variable levels and labels. This loads selecteed variables from the database, applies the chosing naming convention and derives requested variables.
#'
#' @param col_list The objects defining derived variables you want in your dataset.
#' @param db The path to the database you want to extract the variables from.
#' @param name_map The path to the .csv mapping file containing human-readable names for the raw UKB data fields. 
#'
#' @return Returns a data.frame containing the variables requested.
#'
#' @import DBI
#' @import duckdb
#' @export
#' @examples
#' \dontrun{
#' # Extract variables for HTN project from V2 data
#'
#' DB_extract(HTNcols, db="ukb_v2.db")
#' }
#'
DB_extract <- function(extract_cols, db = "ukb_v2.db", 
                       name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv"){
  
  mapping <- read.csv(name_map, stringsAsFactors = FALSE)
  
  # Connect to the database
  con <- dbConnect(duckdb::duckdb(), db)
  on.exit(dbDisconnect(con, shutdown=TRUE))
  
  # List all tables available in the database
  # Each should correspond to one data download
  tables <- dbListTables(con)
  
  # Join all download tables to get all data and extract requested columns
  view <- lapply(tables, function(x) tbl(con, from=x)) %>% 
    reduce(inner_join, by = "f.eid", suffix = c("", ".delete")) %>%
    select(any_of(name_to_fdot(extract_cols, mapping)), -ends_with(".delete")) %>%
    filter(f.eid != 6025392) %>% # NOTE: HACKY FIX TO DEAL WITH BROKEN PARTICIPANT
    collect %>%
    rename_with(fdot_to_name, mapping=mapping)
  
  return(view)
}



# Maps UKB variable names to human readable names according to the given mapping
#
# UKB variable names of the form f.XXXXX.0.0 are converted to TLA_VarName.0.0
#
# @param ukb_col A vector of UKB variable names
# @param mapping A dataframe with the mapping between UKB field IDs and human readable variable names
#
fdot_to_name <- function(ukb_col, mapping) {
  ukb_col <- strsplit(ukb_col, split = ".", fixed = TRUE)
  ukb_col <- sapply(ukb_col, function(x) {
    if (as.character(x[2]) %in% mapping$Field_ID) {
      # Swap the field ID for a human-readable variable name
      x[2] <- mapping$NewVarName[mapping$Field_ID == as.character(x[2])]
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


# Maps human readable names to UKB variable names according to the given mapping
#
# Human readable names of the form TLA_VarName.0.0 are converted to UKB variable names f.XXXXX.0.0
#
# @param col_list A vector of human-readable names
# @param mapping A dataframe with the mapping between UKB field IDs and human readable variable names
#
name_to_fdot <- function(col_names, mapping) {
  col_names <- strsplit(col_names, split = ".", fixed = TRUE)
  col_names <- sapply(col_names, function(x){
    if(x[1] %in% mapping$NewVarName) {
      x[1] <- mapping$Field_ID[mapping$NewVarName == x[1]]
      x <- c("f", x)
    }
    x <- paste(x, collapse=".")
    return(x)
  })
  return(col_names)
}
