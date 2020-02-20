load("dsecondary.Rdata")
t.dsecondary <- reshape(t.dsecondary, varying=sort(colnames(t.dsecondary[,-1])), direction="long", idvar="ID", sep=".i")
t.dsecondary <- dplyr::rename(t.dsecondary, instance = time)
t.dsecondary <- reshape(t.dsecondary, varying=sort(colnames(t.dsecondary[,-1:-2])), direction="long", idvar=c("ID", "instance"), sep=".m")
t.dsecondary <- dplyr::rename(t.dsecondary, measurement = time)
t.dsecondary <- t.dsecondary[rowSums(is.na(t.dsecondary[,-1:-3, drop=FALSE])) !=length(colnames(t.dsecondary))-3,]
save("t.dsecondary", file="t_dsecondary.Rdata")