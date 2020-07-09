load("t_1diagnoses.Rdata")
t.1diagnoses <- reshape(t.1diagnoses, varying=sort(colnames(t.1diagnoses[,-1])), direction="long", idvar="ID", sep=".m")
t.1diagnoses <- dplyr::rename(t.1diagnoses, measurement = time)
save("t.1diagnoses", file="t_1diagnoses.Rdata")