# Jennifer Collister
# 22/09/20

library(yaml)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")


source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

data <- derive_variables(database = config$data$database, field_definitions = testSpecs$SPEC_Neo_HTN)

thing <- TEU_SPECS$SPEC_Neo_HTN
