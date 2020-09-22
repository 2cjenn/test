# Jennifer Collister
# 30/03/2020
# Create functions 

# Formatting of existing UKB variables

FN_id <- function(x){x}

FN_unorder <- function(x){factor(x, ordered=FALSE)}

FN_reorderfactor <- function(levelorder){
  function(x){
    factor(x, levels=levelorder)
  }
}

# Derived variables

FN_labelfactor <- function(levels, labels, recodeNA=NULL){
  function(x){
    y <- factor(x, levels=levels, labels=labels)
    if(!is.null(recodeNA)){
      levels(y) <- unique(c(levels(y), recodeNA))
      y[is.na(y)] <- recodeNA
    }
  }
}

FN_average <- function(colnames, na.rm=TRUE){
  function(data){
    rowMeans(data[,colnames], na.rm)
  }
}

FN_MYtoDate <- function(day, monthField, yearField, format="%d%B%Y"){
  function(data){
    as.Date(paste0(as.character(day), as.character(data[[monthField]]), as.character(data[[yearField]])), format)
  }
}


ID <- list(name="ID",
           source="ID",
           mapper=FN_id,
           display_name="ID",
           description="The unique participant identifier"
           )

ethnicity <- list(name="Eth_ethnicity",
                  source="Eth_ethnicity",
                  mapper=FN_reorderfactor(levels=c("White", "British", "Irish", "Any other white background",
                                                   "Mixed", "White and Black Caribbean", "White and Black African",
                                                   "White and Asian", "Any other mixed background",
                                                   "Asian or Asian British", "Indian", "Pakistani", "Bangladeshi",
                                                   "Any other Asian background",
                                                   "Black or Black British", "Caribbean", "African", "Any other Black background",
                                                   "Chinese", "Other ethnic group", "Do not know", "Prefer not to answer")),
                  display_name="ethnicity",
                  description="The participant's self-reported ethnicity"
                  )

Neoethnicity <- list(name="Eth_ethnicgrp",
                     source="Eth_ethnicity",
                     mapper=function(x){
                       ifelse(x %in% c("White", "British", "Irish", "Any other white background"), 
                              "White",
                              ifelse(x %in% c("Do not know", "Prefer not to answer") | is.na(x), 
                                     "Unknown", 
                                     "Non-white"
                                     )
                              )
                     },
                     display_name="ethnic_group",
                     description="The participant's self-reported ethnicity"
)
ethnicity$ethnicity <- as.character(ethnicity$eth_group)
ethnicity$ethnicity <- ifelse(ethnicity$ethnicity=="White", "White", 
                              ifelse(ethnicity$ethnicity=="Prefer not to answer" | is.na(ethnicity$ethnicity), "Unknown",
                                     "Non-white"))
ethnicity$ethnicity <- factor(ethnicity$ethnicity, levels=c("White", "Non-white", "Unknown"))

rsnlostfu <- list(name="BaC_RsnLostFU",
                  source="BaC_RsnLostFU",
                  mapper=FN_unorder,
                  display_name="lfu_reason",
                  description="The reported reason for loss to follow-up"
                  )

dob <- list(name="TEU_DateOfBirth",
            source=c("BaC_BirthMonth", "BaC_BirthYear"),
            mapper=FN_MYtoDate(day=15, monthField="BaC_BirthMonth", yearField="BaC_BirthYear"),
            display_name="DateOfBirth",
            description="The patient's approximate date of birth, derived from self-reported month and year with date assumed to be 15th"
            )

SBP <- list(name="TEU_SBP.avg",
            source=c("BlP_SBPAuto.0", "BlP_SBPAuto.1", "BlP_SBPMan.0", "BlP_SBPMan.1"),
            mapper=FN_average(colnames=c("BlP_SBPAuto.0", "BlP_SBPAuto.1", "BlP_SBPMan.0", "BlP_SBPMan.1")),
            display_name="SBP",
            description="The average systolic blood pressure from two measurements at baseline"
            )

DBP <- list(name="TEU_DBP.avg",
            source=c("BlP_DBPAuto.0", "BlP_DBPAuto.1", "BlP_DBPMan.0", "BlP_DBPMan.1"),
            mapper=FN_average(colnames=c("BlP_DBPAuto.0", "BlP_DBPAuto.1", "BlP_DBPMan.0", "BlP_DBPMan.1")),
            display_name="DBP",
            description="The average diastolic blood pressure from two measurements at baseline"
)

measuredhyp <- list(name="TEU_measuredHTN",
                    source=c("TEU_SBP.avg", "TEU_DBP.avg"),
                    mapper=function(data){data[["TEU_SBP.avg"]]>=140 | data[["TEU_DBP.avg"]]>=90},
                    display_name="measuredHTN",
                    description="Whether the participant had hypertensive BP measured at baseline"
                    )



common <- list(ID, ethnicity, rsnlostfu, dob)

inpath <- readChar("./data/raw/filepath.txt", file.info("./data/raw/filepath.txt")$size)
# tables <- list("Alc_base", "ArS_base", "BaC", "BBC_base", "BlP_base", "BSM_base","CoF_base",
#             "Die_base", "Edu_base", "Emp_base", "Eth_base", "FaH_base", "GAC_base", "HMH_base",
#             "HoH_base", "PhA_base", "Rec_base", "Sle_base", "Smo_base", "Sun_base", "VeIcovars_base")
# dfs <- list()
# for(tabname in tables){
#   dfs[[tabname]] <- readRDS(paste0(inpath, tabname, ".rds"))
# }
# alldata <- Reduce(function(df1, df2) merge(df1, df2, by = "ID", all.x = TRUE), dfs)
# saveRDS(alldata, paste0(inpath, "AllData", ".rds"))
alldata <- readRDS(paste0(inpath, "AllData", ".rds"))


# Read in the baseline characteristics
covars <- readRDS("./data/clean/basechar.rds")

cols <- c("ID", "BaC_RsnLostFU", "TEU_DateOfBirth")

cols_TLA <- unique(sub("_.*", "", cols[-which(cols=="ID")]))

names(common) <- sapply(common, function(x) x$name)
df <- covars
for(colname in cols){
  colinfo <- common[[colname]]
  colfunc <- colinfo$mapper
  if(length(colinfo$source)>1){
    df[[colinfo$name]] <- colfunc(df)
  } else {
    df[[colinfo$name]] <- colfunc(df[[colinfo$source]])
  }
  print(colinfo$name)
  print(colinfo$description)
}
df <- df[,cols]
