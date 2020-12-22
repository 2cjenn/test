#--------------------------------------------------------------------------------------------------------------
# Jennifer Collister 24/01/2020
# Break down the non-cancer illness code variables into specific conditions within the hierarchical structure
#--------------------------------------------------------------------------------------------------------------
library(tidyr)
library(reshape2)
library(dplyr)
library(yaml)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

VI_op_codes <- function() {
  #--------------------------------------------------------------------------------------------------------------
  # Load the codings
  coding <- read.csv(file.path(config$cleaning$coding, "coding5.csv"), header=TRUE) %>%
    rename(Code = value,
           node_id = code_id)
  
  # And rearrange them into a more sensible format
  joinables <- coding[,c("Code", "meaning", "node_id", "parent_id")]
  
  # Basically: start with the top levels
  # Iteratively merge on each subsequent level, joining the "parent_id" of the new level to the "node_id" of the previous
  # Rename columns as necessary to avoid duplicate column names and to make sense
  # Because not only the bottom level options are selectable, need to make sure that selectable fields are retained as options
  # even when they have possible child nodes
  # Hence filtering on "selectable" and appending these rows
  # Then remove duplication between the ones which are selectable and do not have child rows which would otherwise appear twice
  # This fixes problem where previously those with children did not appear separately and could not be selected on their own
  
  toplevel <- coding[,c("Code", "meaning", "node_id")][coding$parent_id==0,]
  names(toplevel)[names(toplevel) == "node_id"] <- "join_on"
  names(toplevel)[names(toplevel) == "meaning"] <- "TL"
  names(toplevel)[names(toplevel) == "Code"] <- "topcode"
  
  level1 <- merge(toplevel, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
  level1$medcode <- coalesce(level1$Code, level1$topcode)
  level1$selectable <- level1$Code!=-1
  level1 <- level1[,c("medcode", "TL", "meaning", "node_id", "selectable")]
  names(level1)[names(level1) == "node_id"] <- "join_on"
  names(level1)[names(level1) == "meaning"] <- "L1"
  level1 <- unique(level1)
  
  level2 <- merge(level1, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
  level1[setdiff(names(level2), names(level1))] <- NA
  level2 <- rbind(level2, level1[level1$selectable==TRUE & !is.na(level1$selectable),])
  level2$medcode <- coalesce(level2$Code, level2$medcode)
  level2$selectable <- level2$Code!=-1
  level2 <- level2[,c("medcode", "TL", "L1", "meaning", "node_id", "selectable")]
  names(level2)[names(level2) == "node_id"] <- "join_on"
  names(level2)[names(level2) == "meaning"] <- "L2"
  level2 <- unique(level2)
  
  level3 <- merge(level2, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
  level2[setdiff(names(level3), names(level2))] <- NA
  level3 <- rbind(level3, level2[level2$selectable==TRUE & !is.na(level2$selectable),])
  level3$medcode <- coalesce(level3$Code, level3$medcode)
  level3$selectable <- level3$Code!=-1
  level3 <- level3[,c("medcode", "TL", "L1", "L2", "meaning", "node_id", "selectable")]
  names(level3)[names(level3) == "node_id"] <- "join_on"
  names(level3)[names(level3) == "meaning"] <- "L3"
  level3 <- unique(level3)
  
  level4 <- merge(level3, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
  level3[setdiff(names(level4), names(level3))] <- NA
  level4 <- rbind(level4, level3[level3$selectable==TRUE & !is.na(level3$selectable),])
  level4$medcode <- coalesce(level4$Code, level4$medcode)
  level4 <- level4[,c("medcode", "TL", "L1", "L2", "L3", "meaning", "node_id", "selectable")]
  names(level4)[names(level4) == "node_id"] <- "join_on"
  names(level4)[names(level4) == "meaning"] <- "L4"
  level4 <- unique(level4)
  
  level5 <- merge(level4, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
  level4[setdiff(names(level5), names(level4))] <- NA
  level5 <- rbind(level5, level4[level4$selectable==TRUE & !is.na(level4$selectable),])
  level5$medcode <- coalesce(level5$Code, level5$medcode)
  level5 <- level5[,c("medcode", "TL", "L1", "L2", "L3", "L4", "meaning")]
  names(level5)[names(level5) == "meaning"] <- "L5"
  names(level5)[names(level5) == "medcode"] <- "Code"
  level5 <- unique(level5)
  
  
  # Coalesce to get lowest level diagnosis per code
  level5$dx <- coalesce(level5$L4, level5$L3, level5$L2, level5$L1, level5$TL)
  
  #--------------------------------------------------------------------------------------------------------------
  # Sort the table of codings into alphabetical order by levels
  level5 <- level5[order(level5$TL, level5$L1, level5$L2, level5$L3, level5$L4),]
  
  # Save the full table of codings to a csv so mappings can be produced in an Excel
  write.csv(level5, file.path(config$data$derived, "OperationCodes_Map.csv"), 
            na="", row.names=FALSE)

  return(level5)

}
