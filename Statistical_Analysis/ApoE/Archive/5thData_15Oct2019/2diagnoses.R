load("t_2diagnoses.Rdata")
t.2diagnoses <- reshape(t.2diagnoses, varying=sort(colnames(t.2diagnoses[,-1])), direction="long", idvar="ID", sep=".m")
t.2diagnoses <- dplyr::rename(t.2diagnoses, measurement = time)
save("t.2diagnoses", file="t_2diagnoses.Rdata")