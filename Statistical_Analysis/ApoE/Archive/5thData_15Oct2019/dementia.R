#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister
# Investigate dementia cases among the diagnoses data
#--------------------------------------------------------------------------------------------------------------
library(data.table)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("t_diagnoses.Rdata")

# ICD 10 codes for dementia
dem_codes <- c("F000", "F001", "F002", "F009", "F010", "F011", "F012", "F013", "F018", "F019")

# Filter out a set of individuals with diagnoses of dementia and associated conditions
dementia <- t.diagnoses
for (m in 0:212) {
  codecol <- paste0("HES_ICD10.m", m)
  datecol <- paste0("HES_ICD10DateFirst.m", m)
  dementia[[datecol]][!dementia[[codecol]] %in% dem_codes] <- NA
  dementia[[codecol]][!dementia[[codecol]] %in% dem_codes] <- NA
}

# To reduce the size of the data set, remove missing observations
# Rearrange into long format
dementia <- dementia[rowSums(!is.na(dementia))>1,]
dementia <- dementia[, unlist(lapply(dementia, function(x) !all(is.na(x))))]
dementia <- reshape(dementia, varying=sort(colnames(dementia[,-1])), direction="long", idvar="ID", sep=".m")
dementia <- dementia[rowSums(!is.na(dementia))>2,]

# Keep the minimum diagnosis date per individual
dementia %>%
  group_by(ID) %>%
  slice(which.min(HES_ICD10DateFirst))

dementia <- unique(setDT(dementia)[order(HES_ICD10DateFirst)],by="ID")

save("dementia", file="dementia.Rdata")

# Exclude individuals with diagnosis prior to recruitment into UKB


load("t_baseline.Rdata")

t.ado <- t.baseline[,c(1, grep("ADO_", names(t.baseline)))]
t.ado <- t.ado[rowSums(!is.na(t.ado))>1,]
colnames(t.ado)[colnames(t.ado)=="ADO_FTDementiaSouce"] <- "ADO_FTDementiaSource"
t.dementia <- t.ado[,c("ID", "ADO_DementiaDate", "ADO_DementiaSource", "ADO_AlzheimersDate", "ADO_AlzheimersSource", "ADO_VascDementiaDate", "ADO_VascDementiaSource", "ADO_FTDementiaDate", "ADO_FTDementiaSource")]
t.dementia <- t.dementia[rowSums(!is.na(t.dementia))>1,]
t.stroke <- t.ado[,c("ID", "ADO_DateFirstStroke", "ADO_StrokeSource", "ADO_DateFirstIStroke", "ADO_IStrokeSource", "ADO_DateFirstICH", "ADO_ICHSource", "ADO_DateFirstSAH", "ADO_SAHSource")]
t.stroke <- t.stroke[rowSums(!is.na(t.stroke))>1,]


t.age <- t.baseline[,c("ID", "BaC_BirthYear", "BaC_BirthMonth")]
load("t_repmeas.Rdata")
t.age <- merge(t.age, t.repmeas[t.repmeas$instance==0,][,c("ID", "Rec_DateAssess")])

t.dementia$year <- format(t.dementia$ADO_DementiaDate, "%Y")
# yearly_dementia <- count(t.dementia, year)
yearly_dementia <- table(t.dementia$year)
cols <- c("black", "blue", "red")[findInterval(rownames(yearly_dementia), vec=c(1900, 2006, 2010, 2020))]
barplot(yearly_dementia, col=cols, main="Number of all-cause dementia diagnoses by year", xlab="Year", ylab="Dementia diagnoses") 
legend("topleft", inset=0.2, legend=c("Prevalent", "Recruitment Period", "Incident"), fill=c("black", "blue", "red"))


t.dementia <- merge(x=t.dementia, y=t.age, by="ID", all.x=TRUE)
t.dementia$ftime <- difftime(t.dementia$ADO_DementiaDate, t.dementia$Rec_DateAssess, units="days")
t.dementia$incident <- ifelse(t.dementia$ftime > 0, t.dementia$ftime, NA)
t.dementia$dstat <- t.dementia$ftime>0
surv <- Surv(time=t.dementia$incident, event=t.dementia$dstat)
fit <- survfit(surv~1, data=t.dementia)
ggsurvplot(fit, risk.table=TRUE, title="Time from recruitment date to diagnosis of dementia")

