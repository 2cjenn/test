bp <- readRDS("K:/TEU/CancerPRS/Data_Downloads/v1_TLAtables/BP_repeat.rds")

colnames(bp)
collist <- c()
for(visit in c("rep1", "img", "irep1")){
  for(bptype in c("SBP", "DBP")){
    
    bp[[paste0("BlP_", bptype, ".", visit, ".0")]] <- coalesce(bp[[paste0("BlP_", bptype, "Man.", visit, ".0")]],
                                                           bp[[paste0("BlP_", bptype, "Auto.", visit, ".0")]])
    bp[[paste0("BlP_", bptype, ".", visit, ".1")]] <- coalesce(bp[[paste0("BlP_", bptype, "Man.", visit, ".1")]], 
                                                           bp[[paste0("BlP_", bptype, "Auto.", visit, ".1")]])
    
    bp[[paste0("n", bptype, ".", visit)]] <- rowSums(!is.na(bp[,c(paste0("BlP_", bptype, ".", visit, ".0"),
                                                      paste0("BlP_", bptype, ".", visit, ".1"))]))
    
    bp[[paste0("BlP_", bptype, ".", visit)]] <- rowMeans(bp[,c(paste0("BlP_", bptype, ".", visit, ".0"),
                                                               paste0("BlP_", bptype, ".", visit, ".1"))], na.rm=TRUE)
    
    print(visit)
    print(bptype)
    print(table(bp[[paste0("n", bptype, ".", visit)]]))
    collist <- c(collist, paste0("n", bptype, ".", visit))
  }
}
bprep <- bp[,c("ID", collist)]

treated <- readRDS("K:/TEU/APOE on Dementia/Data_Management/Data/HTN/HTN_trt.rds")

treated <- merge(treated, bp[,c("ID", collist)], all.x=TRUE)

for(visit in c("rep1", "img", "irep1")){
  for(bptype in c("SBP", "DBP")){
    print(visit)
    print(bptype)
    print(table(treated[[paste0("n", bptype, ".", visit)]]), useNA='ifany')
  }
}
table(treated$nDBP.irep1, treated$nDBP.rep1)
