#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 07/01/2020
# Produce tables of crude and age-adjusted incidence rates of dementia 
# in individuals with/without APOE e4 and hypertension
#--------------------------------------------------------------------------------------------------------------

library(epitools)
library(yaml)
library(here)


# Load the project config file for filepaths etc
config = yaml.load_file(here::here("./config.yml"))

# Load data
apoe <- readRDS(file=paste0(config$analysisdata$apoe_old, "apoe_excl.rds"))
apoe$definedhypertension <- apoe$SBP >= 140 | apoe$DBP >= 90

# Risk of dementia in each category
case <- table(apoe$definedhypertension[apoe$dementia_status==TRUE], apoe$e4carrier[apoe$dementia_status==TRUE])
pop <- table(apoe$definedhypertension, apoe$e4carrier)
inc <- round(100*case/pop,2)
names(dimnames(inc)) <- c("Hypertension", "APOE e4 carrier")
inc

# Define categorical age groups
apoe$agegrp <- cut(apoe$age, c(50, 55, 60, 65, 70), right=FALSE)
# Number of individuals in each age group
agepop <- table(apoe$agegrp)
# Number of person-years in each age group
pyears <- aggregate(apoe$time_to_dementia_yrs, by=list(apoe$agegrp), FUN=sum)


# Crude incidence rate per age group in the whole population
# Number of individuals in each age group by dementia status
agedement <- table(apoe$agegrp, apoe$dementia_status)
# Divide dementia cases by person-years in each age group
wholepop <- agedement[,2]/(pyears[,2]/100000)

# Age-adjusted
# To Do: make this into a function instead of copy-pasting, and don't just copy the same variable names!
# Produce tables of crude and age-adjusted incidence rates in specified groups
incrates <- c(wholepop)
adjrates <- c()
for (df in list(apoe[apoe$e4carrier==TRUE,], 
                apoe[apoe$e4carrier==FALSE,],
                apoe[apoe$definedhypertension==TRUE,],
                apoe[apoe$definedhypertension==FALSE,])
) {
  
  agedement <- table(df$agegrp, df$dementia_status)
  pyears <- aggregate(df$time_to_dementia_yrs, by=list(df$agegrp), FUN=sum)
  
  # Crude incidence rates
  incrate <- round(agedement[,2]/(pyears[,2]/100000),2)
  incrates <- cbind(incrates, incrate)
  
  # Adjusted incidence rates
  adjrate <- ageadjust.direct(count=agedement[,2], pop=pyears[,2], stdpop=agepop)
  adjrate <- round(100000*adjrate,2)
  adjrates <- rbind(adjrates, adjrate)
}
colnames(incrates) <- c("Whole population", "e4 carriers", "no e4 alleles", "hypertensives", "not hypertensives")
print("Comparing crude incidence rates per 100,000 person-years in different groups")
incrates

rownames(adjrates) <- c("e4 carriers", "no e4 alleles", "hypertensives", "not hypertensives")
print("Comparing age-adjusted incidence rates per 100,000 person years in different groups")
adjrates


# Pair-wise groups
incrates <- c(wholepop)
adjrates <- c()
for (df in list(apoe[apoe$e4carrier==FALSE & apoe$definedhypertension==FALSE,], 
                apoe[apoe$e4carrier==TRUE & apoe$definedhypertension==FALSE,],
                apoe[apoe$e4carrier==FALSE & apoe$definedhypertension==TRUE,],
                apoe[apoe$e4carrier==TRUE & apoe$definedhypertension==TRUE,])
) {
  agedement <- table(df$agegrp, df$dementia_status)
  pyears <- aggregate(df$time_to_dementia_yrs, by=list(df$agegrp), FUN=sum)
  
  # Crude incidence rates
  incrate <- round(agedement[,2]/(pyears[,2]/100000),2)
  incrates <- cbind(incrates, incrate)
  
  # Adjusted incidence rates
  adjrate <- ageadjust.direct(count=agedement[,2], pop=pyears[,2], stdpop=agepop)
  adjrate <- round(100000*adjrate,2)
  adjrates <- rbind(adjrates, adjrate)
}
colnames(incrates) <- c("Whole population", "No e4 alleles, not hypertensive", "e4 carrier, not hypertensive", "No e4 alleles, hypertensive", "e4 carrier, hypertensive")
print("Comparing crude incidence rates per 100,000 person-years in different groups")
incrates

rownames(adjrates) <- c("No e4 alleles, not hypertensive", "e4 carrier, not hypertensive", "No e4 alleles, hypertensive", "e4 carrier, hypertensive")
print("Comparing age-adjusted incidence rates per 100,000 person years in different groups")
adjrates
