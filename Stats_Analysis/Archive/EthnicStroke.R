#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 25/11/2019
# Tabulate ethnic group and stroke incidence for Neo
#--------------------------------------------------------------------------------------------------------------

library(RSQLite)
library(epitools)
library(reshape2)
library(dplyr)
library(survival)
library(survminer)
library(car)

setwd("K:/TEU/APOE on Dementia/Data Management")
rm(list=(ls()[ls()!="bd"]))

conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")

# Investigate the list of tables
dbListTables(conn)
# and the columns of data in a table
dbGetQuery(conn, "PRAGMA table_info(HES)")


#--------------------------------------------------------------------------------------------------------------
# ICD 10 codes for stroke
haem_ICD10 <- c("I60", "I61", "I62")
infarct_ICD10 <- c("I63")
other_ICD10 <- c("I64")
stroke_ICD10 <- c(haem_ICD10, infarct_ICD10, other_ICD10)
# ICD 9 codes for stroke
stroke_ICD9 <- c("430", "431", "432")


# Function to filter HES diagnosis datasets
stroke_only <- function(df, diagcolname, datecolname=NULL, ncols, codelist, codelength=NA, separator=".") {
  if (is.na(codelength)) {
    codelength <- nchar(codelist[1])
    print(codelength)
  }
  for (m in 0:ncols) {
    print(m)
    codecol <- paste0(diagcolname, separator, m)
    if (!is.null(datecolname)) {
      datecol <- paste0(datecolname, separator, m)
      df[[datecol]][!substr(df[[codecol]], 1, codelength) %in% codelist] <- NA
    }
    df[[codecol]][!substr(df[[codecol]], 1, codelength) %in% codelist] <- NA
  }
  # To reduce the size of the data set, remove missing observations
  df <- df[rowSums(!is.na(df))>1,]
  df <- df[, unlist(lapply(df, function(x) !all(is.na(x))))]
  # Rearrange into long format
  df <- reshape(df, varying=sort(colnames(df[,-1])), direction="long", idvar="ID", sep=separator)
  df <- df[,!(names(df) == "time")][!is.na(df[[diagcolname]]),]
  names(df)[names(df)==diagcolname] <- "Code"
  if (!is.null(datecolname)) {
    df[[datecolname]] <- as.Date(df[[datecolname]], origin=as.Date("1970-01-01"))
    names(df)[names(df)==datecolname] <- "Date"
  }
  return(df)
}

# Function 
collist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

#--------------------------------------------------------------------------------------------------------------
# ICD10 diagnoses
# Grab the ICD10 combined primary and secondary diagnosis codes + dates
ICD10Diag <- dbGetQuery(conn, paste0("SELECT ID, ", collist("HES_ICD10Diag", ".m", 212), ", ", collist("HES_ICD10DateFirst", ".m", 212), "FROM HES"))
# Filter these to only keep the ones that were strokes
ICD10Diag <- stroke_only(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=stroke_ICD10, separator=".m")
# Keep the first stroke per person
ICD10DiagFirst <- do.call(rbind, by(ICD10Diag, ICD10Diag[,c("ID")], function(x) x[which.min(x$Date),]))
# Classify the stroke diagnoses by type of stroke
ICD10DiagFirst$type <- ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% haem_ICD10, "Haemorrhagic",
                              ifelse(substr(ICD10DiagFirst$Code, 1, 3) %in% infarct_ICD10, "Ischaemic", "Other"))

# Repeat for ICD9 diagnoses
ICD9Diag <- dbGetQuery(conn, paste0("SELECT ID, ", collist("HES_ICD9Diag", ".m", 46), ", ", collist("HES_ICD9DateFirst", ".m", 46), "FROM HES"))
ICD9Diag <- stroke_only(df=ICD9Diag, diagcolname="HES_ICD9Diag", datecolname="HES_ICD9DateFirst", ncols=46, codelist=stroke_ICD9, separator=".m")
ICD9DiagFirst <- do.call(rbind, by(ICD9Diag, ICD9Diag[,c("ID")], function(x) x[which.min(x$Date),]))
ICD9DiagFirst$type <- ifelse(substr(ICD9DiagFirst$Code, 1, 3) == "430", "Haemorrhagic",
                              ifelse(substr(ICD9DiagFirst$Code, 1, 3) == "431", "Ischaemic", "Other"))

# Combine the ICD10 and ICD9 diagnoses into one stroke diagnosis dataset
stroke <- rbind(ICD10DiagFirst, ICD9DiagFirst)
# And again keep only the first stroke per person
# (There were only 3 individuals who had both ICD9 and ICD10 stroke diagnoses)
stroke <- do.call(rbind, by(stroke, stroke[,c("ID")], function(x) x[which.min(x$Date),]))

# To exclude prevalent strokes, grab the recruitment dates - here's a table I made earlier!
keydates <- dbGetQuery(conn, "SELECT * FROM KEYDATES")

# Join the stroke diagnoses to the full list of all participants and their recruitment dates etc
df <- merge(stroke, keydates, by="ID", all=TRUE)
# Filter out the prevalent cases to be eyeballed
prevalent <- df[df$Date<df$recdate&!is.na(df$Date),]
# Exclude the prevalent cases from the working dataset
df <- df[(df$Date>df$recdate & !is.na(df$Date))|is.na(df$Date),]


# Table some stuff to write up nicely
table(df$type)
table(df$Eth_Ethnicity)
table(!is.na(df$type), df$Eth_Ethnicity)
table(is.na(df$Eth_Ethnicity), df$type)
round(prop.table(table(!is.na(df$type), df$Eth_Ethnicity),2)*100,2)


# dbDisconnect(conn)
