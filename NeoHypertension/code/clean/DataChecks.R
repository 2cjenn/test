
# Check that where the participant has given a non-NA answer to TQ medication, it is in the first column
testm <- coalesce(alldata$HMH_MedCholBPDiab.0, alldata$HMH_MedCholBPDiab.1, alldata$HMH_MedCholBPDiab.2)
testm[!((testm==alldata$HMH_MedCholBPDiab.0 & !is.na(testm)) | (is.na(testm)&is.na(alldata$HMH_MedCholBPDiab.0)))]


testf <- coalesce(alldata$HMH_MedCholBPDiabHorm.0, alldata$HMH_MedCholBPDiabHorm.1,
                  alldata$HMH_MedCholBPDiabHorm.2, alldata$HMH_MedCholBPDiabHorm.3)
testf[!((testf==alldata$HMH_MedCholBPDiabHorm.0 & !is.na(testf)) | (is.na(testf)&is.na(alldata$HMH_MedCholBPDiabHorm.0)))]
