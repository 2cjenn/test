# Jennifer Collister
# 22/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Create an environment for the TEU variable maps
TEUmaps <- new.env()

# Source the variable maps into the TEUmaps environment
source(file.path(config$scripts$cleaning, "Reorganise", "common_derivations.R"), local=TEUmaps)

# Note - may in future want to use https://cran.r-project.org/web/packages/modules/
# This means stuff in the modules *can't* see and interact with stuff in global env
# Better practice


derive_variables <- function(data, colnames, exclusions=function(x){x}){
  
  # Extract the lists from the list of functions
  objects <- Map(function(f) f(), colnames)
  
  # Find the names of the requested columns
  outcols <- sapply(objects, function(x) x$name)

  # Separate into derivations to be calculated before and after exclusion criteria
  before <- objects[sapply(objects, function(x) x$post_exclusion==FALSE)]
  after <- objects[sapply(objects, function(x) x$post_exclusion==TRUE)]
  
  for(colinfo in before){
    colfunc <- colinfo$mapper
    if(length(colinfo$source)>1){
      data[[colinfo$name]] <- colfunc(data[colinfo$source])
    } else {
      data[[colinfo$name]] <- colfunc(data[[colinfo$source]])
    }
    print(colinfo$name)
    print(colinfo$description)
  }
  
  data <- exclusions(data)
  # Note - future update
  # Make exclusions a list of functions, each function is a separate exclusion criterion
  # Then the derive_variables() function can handle the row counts
  # And can write individual documentation for each exclusion criterion to be output nicely
  
  for(colinfo in after){
    colfunc <- colinfo$mapper
    if(length(colinfo$source)>1){
      data[[colinfo$name]] <- colfunc(data[colinfo$source])
    } else {
      data[[colinfo$name]] <- colfunc(data[[colinfo$source]])
    }
    print(colinfo$name)
    print(colinfo$description)
  }
  
  # Return only requested columns
  data <- data[,outcols]
  return(data)
}
