# Connective tissue for David

conn <- dbConnect(RSQLite::SQLite(), "K:/TEU/APOE on Dementia/Data Management/UKB.db")

# Get a dataframe of the HES diagnoses
HES <- dbGetQuery(conn, "SELECT * FROM HES")

connective10 <- c("M359")
connective9 <- c("7109")


# Function to filter HES diagnosis datasets
stroke_only <- function(df, diagcolname, datecolname=NULL, ncols, codelist, separator=".") {
  for (m in 0:ncols) {
    print(m)
    codecol <- paste0(diagcolname, separator, m)
    if (!is.null(datecolname)) {
      datecol <- paste0(datecolname, separator, m)
      df[[datecol]][!substr(df[[codecol]], 1, 4) %in% codelist] <- NA
    }
    df[[codecol]][!substr(df[[codecol]], 1, 4) %in% codelist] <- NA
  }
  # To reduce the size of the data set, remove missing observations
  df <- df[rowSums(!is.na(df))>1,]
  print(nrow(df))
  # if (nrow(df)) > 0 {
  #   df <- df[, unlist(lapply(df, function(x) !all(is.na(x))))]
  #   # Rearrange into long format
  #   df <- reshape(df, varying=sort(colnames(df[,-1])), direction="long", idvar="ID", sep=separator)
  #   df <- df[,!(names(df) == "time")][!is.na(df[[diagcolname]]),]
  #   if (!is.null(datecolname)) {
  #     df[[datecolname]] <- as.Date(df[[datecolname]], origin=as.Date("1970-01-01"))
  #   }
  # }
  return(df)
}

# ICD 10 codes
# Primary
ICD10Prim <- HES[,c(1,grep("ICD10Main", colnames(HES), fixed=TRUE))]
ICD10Prim <- stroke_only(df=ICD10Prim, diagcolname="HES_ICD10Main", datecolname="HES_ICD10MainDateFirst", ncols=65, codelist=connective10, separator=".m")
# Secondary
ICD10Sec <- HES[,c(1,grep("ICD10Sec", colnames(HES), fixed=TRUE))]
ICD10Sec <- stroke_only(df=ICD10Sec, diagcolname="HES_ICD10Secondary", ncols=183, codelist=connective10, separator=".m")
ICD10Sec <- unique(ICD10Sec)

ICD10Diag <- HES[,c(1,grep("ICD10Diag", colnames(HES), fixed=TRUE), grep("ICD10DateFirst", colnames(HES), fixed=TRUE))]
ICD10Diag <- stroke_only(df=ICD10Diag, diagcolname="HES_ICD10Diag", datecolname="HES_ICD10DateFirst", ncols=212, codelist=connective10, separator=".m")


# ICD 9 codes
# Primary
ICD9Prim <- HES[,c(1,grep("ICD9Main", colnames(HES), fixed=TRUE))]
ICD9Prim <- stroke_only(df=ICD9Prim, diagcolname="HES_ICD9Main", ncols=27, codelist=connective9, separator=".m")
# Secondary
ICD9Sec <- HES[,c(1,grep("ICD9Sec", colnames(HES), fixed=TRUE))]
ICD9Sec <- stroke_only(df=ICD9Sec, diagcolname="HES_ICD9Secondary", ncols=29, codelist=connective9, separator=".m")


all <- merge(ICD10Prim, ICD10Sec, by="ID", all=TRUE)
all <- merge(ICD10Diag, ICD9Prim, by="ID", all=TRUE)
all <- all[!(all$ID==4441257 & all$HES_ICD10Diag=="M3599"&!is.na(all$HES_ICD10Diag)),]
write.csv(all, "connectivetissue.csv")
all$CT <- 1


keydates <- dbGetQuery(conn, "SELECT ID, `BAC_SEX`, `BAC_BIRTHYEAR`, `BAC_BIRTHMONTH`, `BAC_DATELOSTFU`, `REC_DATEASSESS`, `ETH_ETHNICITY` 
                      FROM BaC JOIN REC_BASE USING (ID) JOIN ETH_BASE USING (ID)")
keydates$recdate <- as.Date(keydates$Rec_DateAssess, origin=as.Date("1970-01-01"))
keydates$lfudate <- as.Date(keydates$BaC_DateLostFU, origin=as.Date("1970-01-01"))
keydates$dob <- as.Date(paste0("15",keydates$BaC_BirthMonth, keydates$BaC_BirthYear), "%d%B%Y")
keydates$age <- round(as.numeric(difftime(keydates$recdate, keydates$dob, unit="days"), units="days")/365.25,2)

df <- merge(keydates, all, by="ID", all.x=TRUE)

df$CT[is.na(df$CT)] <- 0
df$agediag <- round(as.numeric(difftime(df$HES_ICD10DateFirst, df$dob, unit="days"))/365.25,2)
df$diagrec <- df$HES_ICD10DateFirst < df$Rec_DateAssess

table(df$CT, df$diagrec)
prop.table(table(df$CT, df$BaC_Sex),1)*100

mean(df$agediag[df$CT==1], na.rm=TRUE)
sd(df$agediag[df$CT==1], na.rm=TRUE)

median(df$age[df$CT==0], na.rm=TRUE)
IQR(df$age[df$CT==0], na.rm=TRUE)

hist(df$agediag[df$CT==1], main="Age at diagnosis", xlab="Age, years")
