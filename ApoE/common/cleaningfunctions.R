# Jennifer Collister
# 30/03/2020
# Create functions 

FN_id <- function(x){x}

FN_unorder <- function(x){factor(x, ordered=FALSE)}

FN_ethnicity <- function(x){
  factor(x, levels=c("White", "British", "Irish", "Any other white background",
                     "Mixed", "White and Black Caribbean", "White and Black African",
                     "White and Asian", "Any other mixed background",
                     "Asian or Asian British", "Indian", "Pakistani", "Bangladeshi",
                     "Any other Asian background",
                     "Black or Black British", "Caribbean", "African", "Any other Black background",
                     "Chinese", "Other ethnic group", "Do not know", "Prefer not to answer"))
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
           description="The unique participant identifier")
ethnicity <- list(name="Eth_ethnicity",
                  source="Eth_ethnicity",
                  mapper=FN_ethnicity,
                  display_name="ethnicity",
                  description="The participant's self-reported ethnicity")
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
            description="The patient's approximate date of birth, derived from self-reported month and year with date assumed to be 15th")





common <- list(ID, ethnicity, rsnlostfu, dob)



inpath <- readChar("./data/raw/filepath.txt", file.info("./data/raw/filepath.txt")$size)
# Read in the baseline characteristics
covars <- readRDS("./data/clean/basechar.rds")

cols <- c("ID", "BaC_RsnLostFU", "TEU_DateOfBirth")

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

