#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 02/03/2020
# Clean the UKB BP data and derive new variables
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

config = yaml.load_file("K:/TEU/APOE on Dementia/config.yml")

#--------------------------------------------------------------------------------------------------------------

# Read in the raw data
ethnicity <- readRDS(paste0(config$cleaning$rawdata, "Eth_base.rds"))


# Reorder the factor levels to make more sense
ethnicity$Eth_Ethnicity <- factor(ethnicity$Eth_Ethnicity, levels=c("White", "British", "Irish", "Any other white background",
                                                                    "Mixed", "White and Black Caribbean", "White and Black African", 
                                                                    "White and Asian", "Any other mixed background",
                                                                    "Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", 
                                                                    "Any other Asian background",
                                                                    "Black or Black British", "Caribbean", "African", "Any other Black background",
                                                                    "Chinese", "Other ethnic group", "Do not know", "Prefer not to answer"))

# Individuals that didn't answer the subgroup question get moved to the relevant "other" for their group
ethnicity$eth_group <- dplyr::case_when(
  ethnicity$Eth_Ethnicity %in% c("White", "British", "Irish", 
                                 "Any other white background") ~ "White",
  ethnicity$Eth_Ethnicity %in% c("Mixed", "White and Black Caribbean", 
                               "White and Black African", "White and Asian", 
                               "Any other mixed background") ~ "Mixed",
  ethnicity$Eth_Ethnicity %in% c("Indian", "Pakistani", "Bangladeshi"
                                 ) ~ "S. Asian",
  ethnicity$Eth_Ethnicity %in% c("Black or Black British", "Caribbean", 
                                 "African", "Any other Black background") ~ "Black",
  ethnicity$Eth_Ethnicity %in% c("Other ethnic group","Asian or Asian British", 
                                 "Any other Asian background", "Chinese") ~ "Other",
  ethnicity$Eth_Ethnicity %in% c("Do not know", "Prefer not to answer") ~ "Unanswered",
  is.na(ethnicity$Eth_Ethnicity) ~ "Unanswered",
  TRUE ~ "Error")
ethnicity$eth_group <- factor(ethnicity$eth_group, ordered=FALSE,
                              levels=c("White", "Black", "S. Asian", "Mixed", 
                                       "Other", "Unanswered"))


# Top level categorisation
ethnicity$eth_exact <- ethnicity$Eth_Ethnicity
ethnicity$eth_exact[ethnicity$eth_exact == "White"] <- "Any other white background"
ethnicity$eth_exact[ethnicity$eth_exact == "Mixed"] <- "Any other mixed background"
ethnicity$eth_exact[ethnicity$eth_exact == "Asian or Asian British"] <- "Any other Asian background"
ethnicity$eth_exact[ethnicity$eth_exact == "Black or Black British"] <- "Any other Black background"
ethnicity$eth_exact[is.na(ethnicity$eth_exact)] <- "Prefer not to answer"
ethnicity$eth_exact <- factor(ethnicity$eth_exact)

#--------------------------------------------------------------------------------------------------------------
# Add the genetic ethnic data
gep <- readRDS(paste0(config$cleaning$rawdata, "GeP.rds"))

# "Genetic ethnic group: Indicates samples who self-identified as "White British" according to Field 2100 
# (ethnicity$Eth_Ethnicity) and have very similar genetic ancestry based on a principal components analysis of the genotypes"
gep$genWhiteBrit <- as.character(gep$GeP_ethnic)
# If there's no batch info for the individual, then we assume there's no genetic data for them
gep$genWhiteBrit[is.na(gep$GeP_Batch.m0)] <- "No genetic data"
# Anyone who has genetic data but is not classed as Caucasian is classed as other
gep$genWhiteBrit[is.na(gep$genWhiteBrit)] <- "Other"
# Label it "White British" instead of Caucasian - more true to definition
gep$genWhiteBrit <- factor(gep$genWhiteBrit, levels=c("Caucasian", "Other", "No genetic data"),
                            labels=c("White British", "Other", "No genetic data"), ordered=FALSE)


# Merge into the regular ethnicity data set
ethnicity <- merge(ethnicity, gep[,c("ID", "genWhiteBrit", "GeP_ethnic.m0", "GeP_Batch.m0")], by="ID", all.x=TRUE)


#--------------------------------------------------------------------------------------------------------------
# save
saveRDS(ethnicity[,c("ID", "Eth_Ethnicity", "eth_group", "eth_exact", 
                     "genWhiteBrit", "GeP_ethnic.m0", "GeP_Batch.m0")], 
        file=paste0(config$cleaning$organised, "ethnicity.rds"))

