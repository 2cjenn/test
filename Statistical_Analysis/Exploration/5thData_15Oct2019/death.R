load("t_death.Rdata")
t.death <- reshape(t.death, varying=sort(colnames(t.death[,-1])), direction="long", idvar="ID", sep=".i")
t.death <- dplyr::rename(t.death, instance = time)
t.death <- t.death[rowSums(is.na(t.death[,-1:-2, drop=FALSE])) !=length(colnames(t.death))-2,]
t.death <- t.death[!duplicated(t.death[,c("ID", "Dth_Date", "Dth_ICD10Underlying")]),]
save("t.death", file="t_death.Rdata")