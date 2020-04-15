#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 24/01/2020
# Break down the non-cancer illness code variables into specific conditions within the hierarchical structure
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)

#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
# Load the codings
coding3 <- read.table("K:/TEU/CancerPRS/Data_Dictionary/Mappings/coding3.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)

# And rearrange them into a more sensible format
joinables <- coding3[,c("coding", "meaning", "node_id", "parent_id")]

# Basically: start with the top levels
# Iteratively merge on each subsequent level, joining the "parent_id" of the new level to the "node_id" of the previous
# Rename columns as necessary to avoid duplicate column names and to make sense
# Because not only the bottom level options are selectable, need to make sure that selectable fields are retained as options
# even when they have possible child nodes
# Hence filtering on "selectable" and appending these rows
# Then remove duplication between the ones which are selectable and do not have child rows which would otherwise appear twice
# This fixes problem where previously those with children did not appear separately and could not be selected on their own

toplevel <- coding3[,c("coding", "meaning", "node_id")][coding3$parent_id==0,]
toplevel$selectable <- toplevel$coding!=-1
names(toplevel)[names(toplevel) == "node_id"] <- "join_on"
names(toplevel)[names(toplevel) == "meaning"] <- "TL"
names(toplevel)[names(toplevel) == "coding"] <- "topcode"

level1 <- merge(toplevel, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
toplevel[setdiff(names(level1), names(toplevel))] <- NA
level1 <- rbind(level1, toplevel[toplevel$selectable==TRUE & !is.na(toplevel$selectable),])
level1$medcode <- coalesce(level1$coding, level1$topcode)
level1$selectable <- level1$coding!=-1
level1 <- level1[,c("medcode", "TL", "meaning", "node_id", "selectable")]
names(level1)[names(level1) == "node_id"] <- "join_on"
names(level1)[names(level1) == "meaning"] <- "L1"
level1 <- unique(level1)

level2 <- merge(level1, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level1[setdiff(names(level2), names(level1))] <- NA
level2 <- rbind(level2, level1[level1$selectable==TRUE & !is.na(level1$selectable),])
level2$medcode <- coalesce(level2$coding, level2$medcode)
level2$selectable <- level2$coding!=-1
level2 <- level2[,c("medcode", "TL", "L1", "meaning", "node_id", "selectable")]
names(level2)[names(level2) == "node_id"] <- "join_on"
names(level2)[names(level2) == "meaning"] <- "L2"
level2 <- unique(level2)

level3 <- merge(level2, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level2[setdiff(names(level3), names(level2))] <- NA
level3 <- rbind(level3, level2[level2$selectable==TRUE & !is.na(level2$selectable),])
level3$medcode <- coalesce(level3$coding, level3$medcode)
level3$selectable <- level3$coding!=-1
level3 <- level3[,c("medcode", "TL", "L1", "L2", "meaning")]
names(level3)[names(level3) == "meaning"] <- "L3"
names(level3)[names(level3) == "medcode"] <- "coding"
level3 <- unique(level3)


#--------------------------------------------------------------------------------------------------------------
# Sort the table of codings into alphabetical order by levels
level3 <- level3[order(level3$TL, level3$L1, level3$L2, level3$L3),]
for(col in c("TL", "L1", "L2", "L3")){
  level3[[col]] <- factor(level3[[col]])
}

# Save the full table of codings to a csv so mappings can be produced in an Excel
write.csv(level3, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\CancerCodes.csv", 
          na="", row.names=FALSE)

# And save it as an R data file, why not
saveRDS(level3, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\CancerCodes.rds")


#--------------------------------------------------------------------------------------------------------------
# Finally, join the coded participant data to the code labels

VI_diag <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_cancer_base.rds")
VI_diagdur <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\VeI_cancerdur_base.rds")

# Long-format verbal interview data
# Prepare a list of all participants and their diagnosis codes 
# And drop rows where individuals do not have any illness recorded

# Diagnosis codes
VI_diaglong <- gather(data=VI_diag, key=medno, value=coding, VeI_CancerCode.0:VeI_CancerCode.5, factor_key=FALSE)
VI_diaglong <- VI_diaglong[!is.na(VI_diaglong$coding),]
VI_diaglong <- VI_diaglong %>% tidyr::separate(medno, into=c("VeI", "CancerCode", "instance"))
# And durations
VI_durlong <- gather(data=VI_diagdur, key=medno, value=year, VeI_CancerYr.0:VeI_CancerYr.5, factor_key=FALSE)
VI_durlong <- VI_durlong[!is.na(VI_durlong$year),]
VI_durlong <- VI_durlong %>% tidyr::separate(medno, into=c("VeI", "CancerYear", "instance"))
# Merge the diagnosis codes with the corresponding durations
VI_diaglong <- merge(VI_diaglong[,c("ID", "instance", "coding")], VI_durlong[,c("ID", "instance", "year")], 
                     by=c("ID", "instance"), all.x=TRUE)
# Note that when the year is coded -1 it means unknown, and -3 means preferred not to answer
VI_diaglong$year[VI_diaglong$year %in% c(-1,-3)] <- NA
# remove rows where individuals have the same illness recorded twice - 
# two subtly different conditions that fit in the same UKB category?
VI_diaglong <- unique(VI_diaglong[,c("ID", "coding", "year")])
# Save this ready to be joined to any diagnosis mapping
saveRDS(VI_diaglong, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\VICancerCodes_long.rds")



veint <- readRDS("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\VICancerCodes_long.rds")
# And merge
final <- merge(veint, level3, by="coding")


# This is a version that lists the full tree-structure for each condition for each patient
saveRDS(final, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\Cancer_pts.rds")
