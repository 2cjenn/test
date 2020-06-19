#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister
# Explore the raw data on blood pressure - responding to David's questions from meeting on 17/10/2019
#--------------------------------------------------------------------------------------------------------------

library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("t_bp.Rdata")
load("t_baseline.Rdata")
load("t_repmeas.Rdata")
load("t_medsM.Rdata")
load("t_medsF.Rdata")

# Consider baseline data only for now
baserep <- t.repmeas[t.repmeas$instance==0,]
basechar <- merge(baserep, t.baseline)

basemedsM <- t.medsM[(t.medsM$instance==0 & t.medsM$HMH_MedCholBPDiab=="Blood pressure medication" & !is.na(t.medsM$HMH_MedCholBPDiab)),]
basemedsF <- t.medsF[(t.medsF$instance==0 & t.medsF$HMH_MedCholBPDiabHorm=="Blood pressure medication" & !is.na(t.medsF$HMH_MedCholBPDiabHorm)),]
colnames(basemedsF)[colnames(basemedsF) == "HMH_MedCholBPDiabHorm"] <- "HMH_MedCholBPDiab"
basemeds <- rbind(basemedsM, basemedsF)

basechar <- merge(x=basechar, y=basemeds[,c("ID", "HMH_MedCholBPDiab")], all.x=TRUE)

# Select which variables to consider as covariates etc
covar <- basechar[,c("ID", "Rec_DateAssess", "BaC_BirthYear", "BaC_BirthMonth", "BaC_Sex", "HMH_HBPAhe", "Alc_Status", "Smo_Status", "BSM_BMI", "HMH_MedCholBPDiab")]

#--------------------------------------------------------------------------------------------------------------
# We'll primarily consider baseline BP
# We may later use the repeated measures to consider regression dilution in secondary analyses
baseBP <- t.bp[t.bp$instance==0,]

table(!is.na(baseBP$BlP_SBPAuto), baseBP$BlP_MthMsrBP)

table(baseBP$BlP_MthMsrBP, baseBP$measurement)

# How many individuals measured in each way at baseline?
baseBP$auto <- complete.cases(baseBP[,c("BlP_SBPAuto", "BlP_DBPAuto")])
baseBP$manual <- complete.cases(baseBP[,c("BlP_SBPMan", "BlP_DBPMan")])
table(baseBP$auto, baseBP$measurement)
table(baseBP$manual, baseBP$measurement)
table(baseBP$auto[baseBP$measurement==0], baseBP$manual[baseBP$measurement==0])
table(baseBP$auto[baseBP$measurement==1], baseBP$manual[baseBP$measurement==1])


# Let's investigate the range of measurements obtained
par(mfrow=c(2,2))
hist(baseBP$BlP_DBPAuto, xlim=c(20,140))
hist(baseBP$BlP_SBPAuto, xlim=c(50,250))
hist(baseBP$BlP_DBPMan, xlim=c(20,140))
hist(baseBP$BlP_SBPMan, xlim=c(50,250))
par(mfrow=c(1,1))


# Consider whether first measurement typically higher than second - nervousness
basewide <- reshape(baseBP, v.names=c("BlP_SBPAuto", "BlP_SBPMan", "BlP_DBPAuto", "BlP_DBPMan"), 
                    drop=c("instance", "BlP_MthMsrBP", "BlP_PulseRate", "BlP_PulseRateAuto"), timevar="measurement", idvar="ID", direction="wide")
basewide$SBPdiff <- basewide$BlP_SBPAuto.0 - basewide$BlP_SBPAuto.1
basewide$DBPdiff <- basewide$BlP_DBPAuto.0 - basewide$BlP_DBPAuto.1
par(mfrow=c(1,2))
hist(basewide$SBPdiff, main="Difference in Systolic BP between measurements", xlab="First measurement - second measurement")
hist(basewide$DBPdiff, main="Difference in Diastolic BP between measurements", xlab="First measurement - second measurement")
par(mfrow=c(1,1))
# This doesn't really seem to be the case

# Is there anything in common between individuals who were unable to have the automatic measurement?
manual <- baseBP[complete.cases(baseBP$BlP_DBPMan),]


table(baseBP$BlP_MthMsrBP)

# Each individual had BP measured twice
# Where automatic measurement could not be obtained, a manual measurement was taken
# We want to take the average baseline BP per individual, using automatic measurements where possible,
# supplementing with manual measurements where necessary
bavgBP <- aggregate(x=baseBP[c("BlP_SBPAuto", "BlP_DBPAuto", "BlP_SBPMan", "BlP_DBPMan")], by=baseBP[c("ID")], FUN=mean, na.rm=TRUE)
bavgBP$SBP <- ifelse(is.na(bavgBP$BlP_SBPAuto), bavgBP$BlP_SBPMan, bavgBP$BlP_SBPAuto)
bavgBP$DBP <- ifelse(is.na(bavgBP$BlP_DBPAuto), bavgBP$BlP_DBPMan, bavgBP$BlP_DBPAuto)

plot(bavgBP$BlP_DBPAuto, bavgBP$BlP_DBPMan, main="Diastolic blood pressure, manual plotted against automatic readings for individuals who received both methods at baseline")
plot(bavgBP$BlP_SBPAuto, bavgBP$BlP_SBPMan, main="Systolic blood pressure, manual plotted against automatic readings for individuals who received both methods at baseline")

# Consider whether the difference between systolic and diastolic BP increases with age
covar$dob <- as.Date(paste0("15",covar$BaC_BirthMonth, covar$BaC_BirthYear), format="%d%B%Y")
covar$age <- interval(covar$dob, covar$Rec_DateAssess)/ years(1)

bp <- merge(bavgBP[,c("ID", "SBP", "DBP")], covar)
bp$diff <- bp$SBP - bp$DBP
cordiff <- cor(bp$age, bp$diff, use="complete.obs")
plot(bp$age, bp$diff, main="Difference between mean SBP and DBP by age", xlab="Age, years", ylab="SBP - DBP", pch=20)
abline(lm(bp$diff~bp$age), col="red")

# Correlation between baseline BP and age?
corSBP <- cor(bp$age, bp$SBP, use="complete.obs")
corDBP <- cor(bp$age, bp$DBP, use="complete.obs")
par(mfrow=c(1,2))
plot(bp$age, bp$SBP, main=paste0("SBP against age, correlation coefficient r = ", round(corSBP, 2)), xlab="Age, years", ylab="SBP, mmHg")
abline(lm(bp$SBP~bp$age), col="red")
plot(bp$age, bp$DBP, main=paste0("DBP against age, correlation coefficient r = ", round(corDBP, 2)), xlab="Age, years", ylab="DBP, mmHg")
abline(lm(bp$DBP~bp$age), col="red")
par(mfrow=c(1,1))

# Investigate individuals with excessively high SBP at baseline
highBP <- bp[(bp$SBP>=180&!is.na(bp$SBP)),]
highBP$diag <- highBP$HMH_HBPAhe
highBP$diag <- ifelse((highBP$diag>0&!is.na(highBP$diag)), 1, ifelse(is.na(highBP$diag), 0, highBP$diag))
highBP$diag <- factor(highBP$diag, levels=c(1, 0, -1, -3), labels=c("Yes", "No", "Do not know", "Prefer not to answer"))
table(highBP$diag, highBP$Alc_Status)
table(highBP$BaC_Sex)
table(highBP$HMH_MedCholBPDiab)










#--------------------------------------------------------------------------------------------------------------

means <- aggregate(x=t.bp[c("BlP_SBPAuto", "BlP_DBPAuto", "BlP_SBPMan", "BlP_DBPMan")], by=t.bp[c("ID", "instance")], FUN=mean, na.rm=TRUE)

manual <- t.bp[!is.na(t.bp$BlP_DBPMan),]

for(inst in unique(t.bp$instance)){
  # # Correlation between the two SBP and DBP measurements per individual per visit
  # print(paste0("Correlation between paired DBP measurements at visit ", inst))
  # print(cor(t.bp$BlP_DBPAuto[t.bp$instance==inst & t.bp$measurement==0], t.bp$BlP_DBPAuto[t.bp$instance==inst & t.bp$measurement==1], use="complete.obs"))
  # print(paste0("Correlation between paired SBP measurements at visit ", inst))
  # print(cor(t.bp$BlP_SBPAuto[t.bp$instance==inst & t.bp$measurement==0], t.bp$BlP_SBPAuto[t.bp$instance==inst & t.bp$measurement==1], use="complete.obs"))
  # 
  # # Correlation between mean SBP and mean DBP per individual per visit
  # print(paste0("Correlation between mean SBP and mean DBP measurements at visit ", inst))
  # print(cor(means$BlP_SBPAuto[means$instance==inst], means$BlP_DBPAuto[means$instance==inst], use="complete.obs"))
  
  # Correlation between automatic and manual SBP and DBP measurements per visit
  print(paste0("Correlation between mean SBP and mean DBP measurements at visit ", inst))
  print(cor(t.bp$BlP_SBPAuto[t.bp$instance==inst], t.bp$BlP_SBPMan[t.bp$instance==inst], use="complete.obs"))
  print(cor(t.bp$BlP_DBPAuto[t.bp$instance==inst], t.bp$BlP_DBPMan[t.bp$instance==inst], use="complete.obs"))
  
}
baseline <- t.bp[t.bp$instance==0,]
baselineavg <- aggregate(x=baseline[c("BlP_SBPAuto", "BlP_DBPAuto", "BlP_SBPMan", "BlP_DBPMan")], by=baseline[c("ID", "instance")], FUN=mean, na.rm=TRUE)
cor(baselineavg$BlP_SBPAuto, baselineavg$BlP_SBPMan, use="complete.obs")
cor(baselineavg$BlP_DBPAuto, baselineavg$BlP_DBPMan, use="complete.obs")
both <- baselineavg[complete.cases(baselineavg[,c("BlP_SBPAuto", "BlP_SBPMan")]),]

automatic <- t.bp[,c("ID", "instance", "measurement", "BlP_SBPAuto", "BlP_DBPAuto")]
cor(baseline$BlP_DBPAuto[baseline$measurement==0], baseline$BlP_DBPAuto[baseline$measurement==1], use="complete.obs")

plot(baselineavg$BlP_DBPAuto, baselineavg$BlP_DBPMan, main="Diastolic blood pressure, manual plotted against automatic readings for individuals who received both methods at baseline")
plot(baselineavg$BlP_SBPAuto, baselineavg$BlP_SBPMan, main="Systolic blood pressure, manual plotted against automatic readings for individuals who received both methods at baseline")
hist(means$BlP_DBPAuto[means$instance==0], main="Histogram of Mean Diastolic Blood Pressure at baseline (initial assessment visit)")
hist(means$BlP_SBPAuto[means$instance==0], main="Histogram of Mean Systolic Blood Pressure at baseline (initial assessment visit)")
