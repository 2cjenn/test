#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 12/11/2019
# Load the UKB data, rename the variables and split it into manageable separate tables
# Split by TLA and consider using a SQLite DB?
#--------------------------------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(reshape)
library(readxl)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the R file produced by the UKB converter
# fileloc <- "K:\\TEU\\CancerPRS\\6thData_07Nov2019\\R\\20200219\\ukb38358.R"
fileloc <- "K:\\TEU\\CancerPRS\\6thData_07Nov2019\\R\\20200302\\ukb38358.R"

# First make sure the biobank data has been loaded, if not then load it
if(exists("bd") && is.data.frame(get("bd"))) {
  print("Good to go")
} else {
  print("Please wait a moment")
  source(fileloc)
  # Remove all the other files that were created by the UKB R script to keep clean workspace and save memory
  rm(list=(ls()[ls()!="bd"]))
  print("Ok, ready!")
}

## Remove individuals who have withdrawn from UKB
withdrawn <- read.csv("K:\\TEU\\APOE on Dementia\\Data Management\\WithdrawnIDs.csv", header=FALSE)
bd <- bd[!bd$f.eid %in% withdrawn$V1,]

# Read in the various mapping files downloaded from UKB
dataProperties <- read_excel("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\DataFieldProperties.xlsx")
instances <- read_excel("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\instances.xlsx")
instvalues <- read_excel("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\insvalue.xlsx")

# Read in the data dictionary 
matching <- read.csv("K:\\TEU\\CancerPRS\\Data_Dictionary\\Renaming_List_UPDATE_Nov2019_TEU.csv")


#--------------------------------------------------------------------------------------------------------------
# Prep for labelling instances
# Find instance type per data field
dataInstances <- left_join(dataProperties[,c("field_id", "instance_id")], instvalues[,c("instance_id", "index", "title")], by="instance_id")


# Split the column names into data code, instance and measurement
cols <- data.frame("full"=colnames(bd)[-1])
cols <- separate(cols, col=full, into=c("f", "code", "instance", "measurement"), convert=TRUE, remove=FALSE, fill="right")
# Find instance type per data code
cols <- left_join(cols, dataInstances, by=c("code" = "field_id", "instance" = "index"))
cols$instlabel <- ifelse(is.na(cols$title), cols$instance, cols$title)


# # Use the data dictionary to find the codes that were in our data but not Alex's; 
# if anything prints out here then you need to add names for these variables to the matching list
antis <- anti_join(cols, matching, by=c("code" = "Field_ID"))
unique(antis$code)
# Join the column codes to the list of data names to get user-friendly variable names
cols <- inner_join(cols, matching, by=c("code" = "Field_ID"))
cols <- separate(cols, col=NewVarName, into=c("TLA", "Specific"), sep="_", convert=TRUE, remove=FALSE, fill="right", extra="merge")
cols$maxmeas <- with(cols, ave(measurement, TLA, FUN=max))
cols$maxinst <- with(cols, ave(instance, TLA, FUN=max))

#--------------------------------------------------------------------------------------------------------------
# Separate baseline measurements from repeated and give human-readable names
basecols <- bd[,c("f.eid",as.vector(cols$full[cols$instlabel=="init"]))]
cols$basecols <- ifelse(cols$maxmeas==0, paste0(cols$NewVarName), paste0(cols$NewVarName, ".", cols$measurement))
names(basecols) <- cols$basecols[match(names(basecols), cols$full)]
colnames(basecols)[1] <- "ID"

repcols <- bd[,c("f.eid", as.vector(cols$full[cols$instlabel %in% c("rep1", "img", "irep1")]))]
cols$repcols <- ifelse(cols$maxmeas==0, paste0(cols$NewVarName, ".", cols$instlabel), paste0(cols$NewVarName, ".", cols$instlabel, ".", cols$measurement))
names(repcols) <- cols$repcols[match(names(repcols), cols$full)]
colnames(repcols)[1] <- "ID"

othercols <- bd[,c("f.eid", as.vector(cols$full[cols$instlabel %in% c("0", "1")]))]
cols$othercols <- ifelse(cols$maxmeas==0, 
                         ifelse(cols$maxinst==0, paste0(cols$NewVarName), paste0(cols$NewVarName, ".i", cols$instance)), 
                         ifelse(cols$maxinst==0, paste0(cols$NewVarName, ".m", cols$measurement), paste0(cols$NewVarName, ".m", cols$measurement, ".i", cols$instance)))
names(othercols) <- cols$othercols[match(names(othercols), cols$full)]
colnames(othercols)[1] <- "ID"

#--------------------------------------------------------------------------------------------------------------
# Split into separate dataframes by TLA
TLA <- unique(cols[,c("TLA", "instlabel")])
tablelist <- c()

for (row in 1:nrow(TLA)) {
  prefix <- TLA$TLA[row]
  instlabel <- TLA$instlabel[row]
  if (instlabel=="init") { # Baseline measurements
    tabname <- paste0(prefix, "_base")
    assign(tabname, basecols[,c(1, grep(pattern=prefix, x=colnames(basecols), fixed=TRUE))])
    tablelist <- c(tablelist, tabname)
    print(tabname)
  } else if (instlabel == "0"){ # Other measurements
    # Since filtering on the TLA, only need to create a dataframe for the 0 not the 1 as both tables would be the same
    tabname <- paste0(prefix)
    temptab <- othercols[,c(1, grep(pattern=prefix, x=colnames(othercols), fixed=TRUE))]
    # temptab <- reshape(temptab, varying=sort(colnames(temptab[,-1])), direction="long", idvar="ID", sep=".i")
    assign(tabname, temptab)
    tablelist <- c(tablelist, tabname)
    print(tabname)
  } # Ignore repeated measures for now
}

#--------------------------------------------------------------------------------------------------------------
# Make these into individual dataframes and save
# If you're running this on a new set of downloaded data fields, 
# it should add them to the existing data files without overwriting the old stuff
# It won't update existing data fields (but they shouldn't change?)
df_to_table <- function(tablist, overwrite) {
  # conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")
  for (table in tablist) {
    print(table)
    df <- eval(as.name(table))
    filename <- paste0("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\", table, ".rds")
    if (file.exists(filename)){
      print("Already exists")
      temp <- readRDS(filename)
      newcols <- setdiff(colnames(df), colnames(temp))
      print(newcols)
      if (length(newcols>0)) {
        print("Adding new data")
        df <- merge(temp, df[,c("ID", newcols)], by="ID", all=TRUE)
        saveRDS(df, file=filename)
      }
    } else {
      print("Saving table")
      saveRDS(df, file=filename)
    }
    
    
     # tryCatch({dbWriteTable(conn, table, df, overwrite=overwrite)}, 
     #          error=function(e) {dbWriteTable(conn, paste0(table, "_Neo"), eval(as.name(table)), overwrite=TRUE)})
  }
  # dbDisconnect(conn)
}
df_to_table(tablist=tablelist, overwrite=FALSE)
