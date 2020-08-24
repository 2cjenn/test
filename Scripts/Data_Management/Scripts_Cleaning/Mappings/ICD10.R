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
coding19 <- read.table(paste0(config$cleaning$coding, "coding19.tsv"), sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)

# Basically: start with the top levels
# Iteratively merge on each subsequent level, joining the "parent_id" of the new level to the "node_id" of the previous
# Rename columns as necessary to avoid duplicate column names and to make sense
# Because not only the bottom level options are selectable, need to make sure that selectable fields are retained as options
# even when they have possible child nodes
# Hence filtering on "selectable" and appending these rows
# Then remove duplication between the ones which are selectable and do not have child rows which would otherwise appear twice
# This fixes problem where previously those with children did not appear separately and could not be selected on their own

toplevel <- coding19[coding19$parent_id==0,] %>%
  transmute(TL = factor(meaning),
         topcode = factor(coding),
         join_on = node_id,
         select = selectable)

level1 <- merge(toplevel, coding19, by.x="join_on", by.y="parent_id", all.x=TRUE)
toplevel[setdiff(names(level1), names(toplevel))] <- NA
level1 <- rbind(level1, toplevel[toplevel$select=="Y" & !is.na(toplevel$select),]) %>%
  transmute(TL,
         topcode,
         L1 = factor(meaning),
         code1 = factor(coding),
         join_on = node_id,
         select = selectable) %>%
  distinct


level2 <- merge(level1, coding19, by.x="join_on", by.y="parent_id", all.x=TRUE)
level1[setdiff(names(level2), names(level1))] <- NA
level2 <- rbind(level2, level1[level1$select=="Y" & !is.na(level1$select),]) %>%
  transmute(TL,
         topcode,
         L1,
         code1,
         L2 = factor(meaning),
         code2 = factor(coding),
         join_on = node_id,
         select = selectable) %>%
  distinct

level3 <- merge(level2, coding19, by.x="join_on", by.y="parent_id", all.x=TRUE)
level2[setdiff(names(level3), names(level2))] <- NA
level3 <- rbind(level3, level2[level2$select=="Y" & !is.na(level2$select),]) %>%
  transmute(TL,
            topcode,
            L1,
            code1,
            L2,
            code2,
            L3 = factor(meaning),
            code3 = factor(coding),
            join_on = node_id,
            select = selectable) %>%
  distinct

level4 <- merge(level3, coding19, by.x="join_on", by.y="parent_id", all.x=TRUE)
level3[setdiff(names(level4), names(level3))] <- NA
level4 <- rbind(level4, level3[level3$select=="Y" & !is.na(level3$select),]) %>%
  transmute(TL,
            topcode,
            L1,
            code1,
            L2,
            code2,
            L3,
            code3,
            L4 = factor(meaning),
            code4 = factor(coding),
            join_on = node_id,
            select = selectable) %>%
  distinct

#--------------------------------------------------------------------------------------------------------------
# Add a column for overall ICD10 code and order by it
ICD10 <- level4 %>%
  mutate(ICD10 = factor(coalesce(code4, code3, code2, code1, topcode))) %>%
  select(-join_on, -select) %>%
  arrange(as.character(ICD10))

# Save the full table of codings to a csv so mappings can be produced in an Excel
write.csv(ICD10, paste0(config$data$derived, "ICD10codes.csv"), 
          na="", row.names=FALSE)

# And save it as an R data file, why not
saveRDS(ICD10, file.path(config$data$derived, "ICD10codes.rds"))

