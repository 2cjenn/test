# Jennifer Collister
# 22/09/20

library(DBI)
library(duckdb)
library(tidyverse)
library(R.cache)
setCacheRootPath(path="Data/Cache")

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Create an environment for the dependency functions
DBfunc <- new.env()

# Source the variable maps into the TEUmaps environment
source(file.path(config$scripts$cleaning, "Reorganise", "TEU_specifications.R"))
source(file.path(config$scripts$cleaning, "Reorganise", "DuckDB.R"), local=DBfunc)

# Note - may in future want to use https://cran.r-project.org/web/packages/modules/
# This means stuff in the modules *can't* see and interact with stuff in global env
# Better practice


derive_variables <- function(database, field_definitions, exclusions=function(x){x}){
  
  # Extract the lists from the list of functions
  objects <- Map(function(p) {if(is.function(p)) {p()} else {p}}, field_definitions)
  
  # Find the names of the requested columns
  outcols <- sapply(objects, function(x) x$name)
  
  # And the names of all the source columns required
  source_cols <- unique(unlist(sapply(objects, function(x) x$source)))
  
  # Extract data fields from database
  data <- DBfunc$DB_extract(source_cols, db = database)

  # Separate into derivations to be calculated before and after exclusion criteria
  before <- objects[sapply(objects, function(x) x$post_exclusion==FALSE)]
  after <- objects[sapply(objects, function(x) x$post_exclusion==TRUE)]
  
  data <- DBfunc$source_rotation(data, field_definitions = before)
  
  data <- exclusions(data)
  # Note - future update
  # Make exclusions a list of functions, each function is a separate exclusion criterion
  # Then the derive_variables() function can handle the row counts
  # And can write individual documentation for each exclusion criterion to be output nicely
  
  data <- DBfunc$source_rotation(data, field_definitions = after)
  
  # Return only requested columns
  data <- data[,outcols[outcols %in% colnames(data)]]
  return(data)
}


# Derive variables whose sources are available in the data first
DBfunc$source_rotation <- function(data, field_definitions) {
  
  data_cols <- colnames(data)
  
  while (length(field_definitions) > 0) {
    remove <- vector()
    for (d in seq(1, length(field_definitions), by=1)) {
      
      # Iterate over the derivation objects
      defn <- field_definitions[[d]]
      
      if (all(defn$source %in% data_cols)) {
        
        # If all required source columns are available, derive them
        tryCatch(
          data <- DBfunc$derive_fn(data, field_definition = defn),
          error=function(cond) {
            message(paste(defn$name, "threw the following error: "))
            message(cond)
            return(data)
          },
          warning=function(cond) {
            message(paste(defn$name, "threw the following warning: "))
            message(cond)
            return(data)
          }
        )
        
        
        # Add the variable to the list of available data columns
        data_cols <- append(data_cols, defn$name)
        
        # Keep a list of the indices of the derivations we've completed
        remove <- append(remove, d)
      }
    }
    if(length(remove)>0) {
      # Once variables have been derived, remove them from the list
      field_definitions <- field_definitions[-remove]
    } else {
      # If there are still derivation objects waiting but no new variables have been derived
      # Then we know that these objects can't be derived on the next loop either
      # We print out the objects so the user can see which they are
      warning(paste0("Unable to derive variables: ", paste(sapply(field_definitions, function(x) x$name), collapse=", ")))
      # And exit the while loop
      break
    }
  }
  return(data)
}



# Actually do the deriving
DBfunc$derive_fn <- function(data, field_definition) {
  colfunc <- field_definition$mapper
  if(length(field_definition$source)>1){
    data[[field_definition$name]] <- colfunc(data[field_definition$source])
  } else {
    data[[field_definition$name]] <- colfunc(data[[field_definition$source]])
  }
  print(paste0("Derived ", field_definition$name))

  return(data)
}

