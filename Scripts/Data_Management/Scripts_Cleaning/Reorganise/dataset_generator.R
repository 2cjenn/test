# Jennifer Collister
# 22/09/20



derive_variables <- function(data, colnames, exclusions){
  # Load the project config file for filepaths etc
  config = yaml.load_file("config.yml")
  
  source(file.path(config$scripts$cleaning, "Reorganise", "common_derivations.R"))
  
  objects <- common[colnames]
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
  data <- data[,colnames]
  return(data)
}
