#-----------------------------------------------------------------------------------------------------------------------
# Combine the different tiers of job code
#-----------------------------------------------------------------------------------------------------------------------
coding2 <- read.table("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\coding2.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)


# And rearrange them into a more sensible format
joinables <- coding2[,c("coding", "meaning", "node_id", "parent_id")]

# Basically: start with the top levels
# Iteratively merge on each subsequent level, joining the "parent_id" of the new level to the "node_id" of the previous
# Rename columns as necessary to avoid duplicate column names and to make sense
# Because not only the bottom level options are selectable, need to make sure that selectable fields are retained as options
# even when they have possible child nodes
# Hence filtering on "selectable" and appending these rows
# Then remove duplication between the ones which are selectable and do not have child rows which would otherwise appear twice
# This fixes problem where previously those with children did not appear separately and could not be selected on their own

toplevel <- coding2[,c("coding", "meaning", "node_id")][coding2$parent_id==0,]
names(toplevel)[names(toplevel) == "node_id"] <- "join_on"
names(toplevel)[names(toplevel) == "meaning"] <- "TL"
names(toplevel)[names(toplevel) == "coding"] <- "topcode"

level1 <- merge(toplevel, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level1 <- level1[,c("TL", "meaning", "node_id")]
names(level1)[names(level1) == "node_id"] <- "join_on"
names(level1)[names(level1) == "meaning"] <- "L1"
level1 <- unique(level1)

level2 <- merge(level1, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level2 <- level2[,c("TL", "L1", "meaning", "node_id")]
names(level2)[names(level2) == "node_id"] <- "join_on"
names(level2)[names(level2) == "meaning"] <- "L2"
level2 <- unique(level2)

level3 <- merge(level2, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level3 <- level3[,c("TL", "L1", "L2", "meaning", "node_id")]
names(level3)[names(level3) == "node_id"] <- "join_on"
names(level3)[names(level3) == "meaning"] <- "L3"
level3 <- unique(level3)

level4 <- merge(level3, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level4 <- level4[,c("TL", "L1", "L2", "L3", "meaning", "node_id")]
names(level4)[names(level4) == "node_id"] <- "Emp_JobCode.0"
names(level4)[names(level4) == "meaning"] <- "L4"
level4 <- unique(level4)

level4$TL <- factor(level4$TL)
level4$L1 <- factor(level4$L1)
level4$L2 <- factor(level4$L2)
level4$L3 <- factor(level4$L3)
level4$L4 <- factor(level4$L4)
# Save the full table of codings to a csv so mappings can be produced in an Excel
write.csv(level4, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\JobCodes.csv", 
          na="", row.names=FALSE)

# And save it as an R data file, why not
saveRDS(level4, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\JobCodes.rds")


#--------------------------------------------------------------------------------------------------------------
# Finally, join the coded participant data to the code labels
# Load the long-format verbal interview data
employment <- readRDS(paste0("K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Emp_base", ".rds"))

# And merge
final <- merge(employment[,c("ID", "Emp_JobCode.0")], level4, by="Emp_JobCode.0", all.x=TRUE)

# This is a version that lists the full tree-structure for each condition for each patient
saveRDS(final, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\JobCodes_pts.rds")


