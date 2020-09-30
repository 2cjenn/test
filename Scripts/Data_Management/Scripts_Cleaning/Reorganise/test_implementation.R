# Jennifer Collister
# 22/09/20

library(yaml)

# Load the project config file for filepaths etc
config = yaml.load_file("config.yml")


source(file.path(config$scripts$cleaning, "Reorganise", "dataset_generator.R"))

data <- derive_variables(database = "ukb_v2.db", field_definitions = testSpecs$SPEC_Neo_HTN)

thing <- testSpecs$SPEC_Neo_HTN
