load("t_diagnoses.Rdata")
t.diagnoses <- reshape(t.diagnoses, varying=sort(colnames(t.diagnoses[,-1])), direction="long", idvar="ID", sep=".m")
t.diagnoses <- dplyr::rename(t.diagnoses, measurement = time)
save("t.diagnoses", file="t_diagnoses.Rdata")