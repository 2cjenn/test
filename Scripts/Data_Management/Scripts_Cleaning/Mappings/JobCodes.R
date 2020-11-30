#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 24/01/2020
# Break down the non-cancer illness code variables into specific conditions within the hierarchical structure
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

config = yaml.load_file("config.yml")

#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
# Load the codings
coding2 <- read.table(file.path(config$cleaning$coding, "coding2.csv"), 
                      sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)

# And rearrange them into a more sensible format
joinables <- coding2[,c("coding", "meaning", "node_id", "parent_id", "selectable")][coding2$parent_id!=0,]

# Basically: start with the top levels
# Iteratively merge on each subsequent level, joining the "parent_id" of the new level to the "node_id" of the previous
# Rename columns as necessary to avoid duplicate column names and to make sense
# Because not only the bottom level options are selectable, need to make sure that selectable fields are retained as options
# even when they have possible child nodes
# Hence filtering on "selectable" and appending these rows
# Then remove duplication between the ones which are selectable and do not have child rows which would otherwise appear twice
# This fixes problem where previously those with children did not appear separately and could not be selected on their own

toplevel <- coding2[,c("coding", "meaning", "node_id", "selectable")][coding2$parent_id==0,]
names(toplevel)[names(toplevel) == "node_id"] <- "join_on"
names(toplevel)[names(toplevel) == "meaning"] <- "TL"
names(toplevel)[names(toplevel) == "coding"] <- "topcode"
names(toplevel)[names(toplevel) == "selectable"] <- "prev_selectable"

level1 <- merge(toplevel, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level1$jobcode <- coalesce(level1$coding, level1$topcode)
level1 <- level1[,c("jobcode", "TL", "meaning", "node_id", "selectable")]
names(level1)[names(level1) == "meaning"] <- "L1"
names(level1)[names(level1) == "node_id"] <- "join_on"
names(level1)[names(level1) == "selectable"] <- "prev_selectable"
level1 <- unique(level1)

level2 <- merge(level1, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level1[setdiff(names(level2), names(level1))] <- NA
level2 <- rbind(level2, level1[level1$prev_selectable=="Y"& !is.na(level1$prev_selectable),])
level2$jobcode <- coalesce(level2$coding, level2$jobcode)
level2 <- level2[,c("jobcode", "TL", "L1", "meaning", "node_id", "selectable")]
names(level2)[names(level2) == "meaning"] <- "L2"
names(level2)[names(level2) == "node_id"] <- "join_on"
names(level2)[names(level2) == "selectable"] <- "prev_selectable"
level2 <- unique(level2)

level3 <- merge(level2, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level2[setdiff(names(level3), names(level2))] <- NA
level3 <- rbind(level3, level2[level2$prev_selectable=="Y" & !is.na(level2$prev_selectable),])
level3$jobcode <- coalesce(level3$coding, level3$jobcode)
level3 <- level3[,c("jobcode", "TL", "L1", "L2", "meaning", "node_id", "selectable")]
names(level3)[names(level3) == "meaning"] <- "L3"
names(level3)[names(level3) == "node_id"] <- "join_on"
names(level3)[names(level3) == "selectable"] <- "prev_selectable"
level3 <- unique(level3)

level4 <- merge(level3, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level3[setdiff(names(level4), names(level3))] <- NA
level4 <- rbind(level4, level3[level3$prev_selectable=="Y" & !is.na(level3$prev_selectable),])
level4$jobcode <- coalesce(level4$coding, level4$jobcode)
level4 <- level4[,c("jobcode", "TL", "L1", "L2", "L3", "meaning")]
names(level4)[names(level4) == "meaning"] <- "L4"
names(level4)[names(level4) == "jobcode"] <- "Emp_JobCode.0"
names(level4)[names(level4) == "selectable"] <- "prev_selectable"
level4 <- unique(level4)

level4$TL <- factor(level4$TL)
level4$L1 <- factor(level4$L1)
level4$L2 <- factor(level4$L2)
level4$L3 <- factor(level4$L3)
level4$L4 <- factor(level4$L4)


#--------------------------------------------------------------------------------------------------------------
# Save the full table of codings to a csv so mappings can be produced in an Excel
write.csv(level4, file.path(config$data$derived, "JobCodes.csv"), 
          na="", row.names=FALSE)

# And save it as an R data file, why not
saveRDS(level4, file.path(config$data$derived, "JobCodes.rds"))


#--------------------------------------------------------------------------------------------------------------
# Finally, join the coded participant data to the code labels
# Load the long-format verbal interview data
employment <- readRDS(file.path(config$data$received, "Emp_base.rds"))
jobs <- employment[,c("ID", "Emp_JobCode.0")]
jobs$TEU_Emp_CurrStat <- factor(coalesce(as.character(employment$Emp_CurrStat.0), 
                                  as.character(employment$Emp_CurrStatUnc.0)),
                                ordered=FALSE)

# And merge
final <- merge(jobs, level4, by="Emp_JobCode.0", all.x=TRUE)

# Combine the employment status and current/most recent job code
final$TEU_EmpCat <- factor(coalesce(as.character(final$TL), 
                             as.character(final$TEU_Emp_CurrStat)),
                           ordered=FALSE,
                           levels=c("Managers and Senior Officials",
                                    "Professional Occupations",
                                    "Associate Professional and Technical Occupations",
                                    "Administrative and Secretarial Occupations",
                                    "Skilled Trades Occupations",
                                    "Personal Service Occupations",
                                    "Sales and Customer Service Occupations",
                                    "Process, Plant and Machine Operatives",
                                    "Elementary Occupations",
                                    "Other job (free text entry)",
                                    "In paid employment or self-employed",
                                    "Retired",
                                    "Looking after home and/or family",
                                    "Unable to work because of sickness or disability",
                                    "Unemployed",
                                    "Doing unpaid or voluntary work",
                                    "Full or part-time student",
                                    "None of the above",
                                    "Prefer not to answer"))

# This is a version that lists the full tree-structure for each condition for each patient
saveRDS(final, file.path(config$data$derived, "JobCodes_pts.rds"))
