#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 24/07/2020
# Count Amyotrophic lateral sclerosis and Parkinson disease cases in HES and Death Registry
#--------------------------------------------------------------------------------------------------------------

config = yaml.load_file("K:/TEU/APOE on Dementia/config.yml")

alsHES <- readRDS(file.path(config$cleaning$organised, "als_incident.rds"))
parkHES <- readRDS(file.path(config$cleaning$organised, "parkinson_incident.rds"))
deaths <- readRDS(file.path(config$cleaning$organised, "deathdate.rds"))
VIalspark <- readRDS(file.path(config$cleaning$organised, "veint_ALSpark_hist.rds"))

als <- merge(alsHES, deaths[deaths$alsdeath==TRUE,c("ID", "deathdate", "alsdeath", "Dth_Cause.m0.i0")], 
             by="ID", sort=FALSE, all=TRUE)
als <- als[!als$ID %in% VIalspark$ID[VIalspark$VIals==TRUE],]
als$HESdiag <- !is.na(als$Code)
als$HESdiag_ <- factor(als$HESdiag, levels=c(TRUE, FALSE), labels=c("HES diagnosis", "Not in HES"))
als$alsdeath_ <- factor(!is.na(als$alsdeath), levels=c(TRUE, FALSE), labels=c("ALS in cause of death", "Alive/other causes"))
table(als$HESdiag_, als$alsdeath_)

park <- merge(parkHES, deaths[deaths$parkdeath==TRUE,c("ID", "deathdate", "parkdeath", "Dth_Cause.m0.i0")], 
              by="ID", sort=FALSE, all=TRUE)
park <- park[!park$ID %in% VIalspark$ID[VIalspark$VIpark==TRUE],]
park$HESdiag <- !is.na(park$Code)
park$HESdiag_ <- factor(park$HESdiag, levels=c(TRUE, FALSE), labels=c("HES diagnosis", "Not in HES"))
park$parkdeath_ <- factor(!is.na(park$parkdeath), levels=c(TRUE, FALSE), labels=c("Parkinson disease in cause of death", "Alive/other causes"))
table(park$HESdiag_, park$parkdeath_)