# Jennifer Collister
# 03/03/2020

library(readxl)
#-----------------------------------------------------------------------------------------------------------------------
# Combine the different tiers of country code
#-----------------------------------------------------------------------------------------------------------------------
coding89 <- read.table("K:\\TEU\\CancerPRS\\Data_Dictionary\\Mappings\\coding89.tsv", sep="\t", header=TRUE, quote="", comment.char="$", fill=FALSE)

# And rearrange them into a more sensible format
joinables <- coding89[,c("coding", "meaning", "node_id", "parent_id")]

# Basically: start with the top levels
# Iteratively merge on each subsequent level, joining the "parent_id" of the new level to the "node_id" of the previous
# Rename columns as necessary to avoid duplicate column names and to make sense
# Because not only the bottom level options are selectable, need to make sure that selectable fields are retained as options
# even when they have possible child nodes
# Hence filtering on "selectable" and appending these rows
# Then remove duplication between the ones which are selectable and do not have child rows which would otherwise appear twice
# This fixes problem where previously those with children did not appear separately and could not be selected on their own

toplevel <- coding89[,c("coding", "meaning", "node_id")][coding89$parent_id==0,]
names(toplevel)[names(toplevel) == "node_id"] <- "join_on"
names(toplevel)[names(toplevel) == "meaning"] <- "Continent"
names(toplevel)[names(toplevel) == "coding"] <- "topcode"
toplevel$Continent <- factor(toplevel$Continent)

level1 <- merge(toplevel, joinables, by.x="join_on", by.y="parent_id", all.x=TRUE)
level1 <- level1[,c("Continent", "meaning", "node_id")]
names(level1)[names(level1) == "node_id"] <- "VeI_BirthCountry"
names(level1)[names(level1) == "meaning"] <- "Country"
level1 <- unique(level1)
level1$Country <- factor(level1$Country)

# Save the full table of codings to a csv so mappings can be produced in an Excel
write.csv(level1, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\CountryCodes.csv", 
          na="", row.names=FALSE)

# And save it as an R data file, why not
saveRDS(level1, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\CountryCodes.rds")


#--------------------------------------------------------------------------------------------------------------
# Use the World Bank spreadsheet to determine which countries are classified as high, middle or low income
# Load in the income levels of the countries
incomelevel <- read_xlsx("K:\\TEU\\APOE on Dementia\\Data Management\\Mappings\\CountryIncomeCategory.xlsx", 
                         sheet="UKB_CountryNames", col_names=TRUE)
# incomelevel <- incomelevel[!is.na(incomelevel$X__1),]
# names(incomelevel)[names(incomelevel)=="X__1"] <- "CountryShort"
# names(incomelevel)[names(incomelevel)=="UKB country"] <- "Country"
# names(incomelevel)[names(incomelevel)=="Bank's fiscal year:"] <- "WBCountry"
# names(incomelevel)[names(incomelevel)=="FY20"] <- "IncomeLevel"
names(incomelevel)[names(incomelevel)=="meaning"] <- "Country"
names(incomelevel)[names(incomelevel)=="World Bank Name"] <- "WBCountry"

# # Try and use grep to approx match (eg Bahamas = Bahamas, the)
# matches <- unlist(lapply(level1$Country, function(i) grep(i, incomelevel$WBCountry, fixed=TRUE)[1]))
# t1 <- as.character(level1$Country[!is.na(matches)])
# t2 <- incomelevel$WBCountry[matches[!is.na(matches)]]
# incomelevel$Country <- NA
# incomelevel$Country[matches[!is.na(matches)]] <- as.character(level1$Country[!is.na(matches)])

# # Manually match a couple of obvious/important ones
# incomelevel$Country[incomelevel$WBCountry=="Faeroe Islands"] <- "Faroe Islands"
# incomelevel$Country[incomelevel$WBCountry=="United States"] <- "USA"
# incomelevel$Country[incomelevel$WBCountry=="Guyana"] <- "The Guianas"
# incomelevel$Country[incomelevel$WBCountry=="Korea, Rep."] <- "South Korea"
# incomelevel$Country[incomelevel$WBCountry=="Korea, Dem. Rep."] <- "North Korea"
# incomelevel$Country[incomelevel$WBCountry=="Slovak Republic"] <- "Slovakia"
# incomelevel$Country[incomelevel$WBCountry=="São Tomé and Principe"] <- "Sao Tome and Principe"
# incomelevel$Country[incomelevel$WBCountry=="Kosovo"] <- "Republic of Kosovo"
# incomelevel$Country[incomelevel$WBCountry=="Kyrgyz Republic"] <- "Kyrgyzstan"
# incomelevel$Country[incomelevel$WBCountry=="Myanmar"] <- "Myanmar (Burma)"
# incomelevel$Country[incomelevel$WBCountry=="Macao SAR, China"] <- "Macau (Macao)"
# incomelevel$Country[incomelevel$WBCountry=="Côte d'Ivoire"] <- "Ivory Coast"
# incomelevel$Country[incomelevel$WBCountry=="Cabo Verde"] <- "Cape Verde"
# # Return to look at the Carribean

# Merge on the new field
countries <- merge(level1, incomelevel[,c("IncomeLevel", "WBCountry", "Country")], by="Country", all.x=TRUE)

# View(countries[is.na(countries$FY20),])
# And save it as an R data file
saveRDS(countries, "K:\\TEU\\APOE on Dementia\\Data Management\\R_Dataframes_TLA\\38358\\Organised\\CountryIncome.rds")


