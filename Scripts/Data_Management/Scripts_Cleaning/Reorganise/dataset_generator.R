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


derive_variables <- function(data, colnames, exclusions=NULL){
  
  objects <- Map(function(f) f(), colnames)

  before <- objects[sapply(objects, function(x) x$post_exclusion==FALSE)]
  after <- objects[sapply(objects, function(x) x$post_exclusion==TRUE)]
  
  for(colinfo in before){
    colfunc <- colinfo$mapper
    if(length(colinfo$source)>1){
      data[[colinfo$name]] <- colfunc(data)
    } else {
      data[[colinfo$name]] <- colfunc(data[[colinfo$source]])
    }
    print(colinfo$name)
    print(colinfo$description)
  }
  # data <- data[,colnames]
  return(data)
}
