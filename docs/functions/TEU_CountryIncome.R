# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_CountryIncome <- function(data){
      
      # Generate mapping first 
      UKB_mapper=read.csv_kdrive(file.path(config$cleaning$coding,'coding89_flat_CountryOfBirth.csv'))
      Income_mapper=read.xlsx_kdrive(file.path(config$cleaning$mapping,'CountryIncomeCategory.xlsx'),sheet = 'UKB_CountryNames')
      
      mapper=left_join(UKB_mapper[,c('Code','meaning')],Income_mapper[,c('coding','IncomeLevel')],by=c('Code'='coding'))%>%
        mutate(TEU_Incomelevel=case_when(meaning=='United Kingdom' ~ 'UK',
                                         IncomeLevel=='H' ~ 'Other high income',
                                         IncomeLevel %in% c('UM','LM') ~ 'Middle income',
                                         IncomeLevel=='L' ~ 'Low income'))
      
      # Merge with data
      data=data%>%
        rename(TQ=ELF_BirthCountry.0.0,VI=VeI_BirthCountry.0.0)%>%
        mutate(VI=as.numeric(VI))%>%
        left_join(.,mapper,by=c('VI'='Code'))%>%
        # create column to derive Country of birth (by income)
        mutate(CountryIncome=case_when(!is.na(VI) ~ TEU_Incomelevel,
                                       is.na(VI) & TQ %in% c('England','Wales','Scotland','Northern Ireland') ~ 'UK',
                                       is.na(VI) & TQ=='Republic of Ireland' ~ mapper[which(mapper$meaning=='Ireland'),]$TEU_Incomelevel,
                                       is.na(VI) & TQ %in% c('Elsewhere','Prefer not to answer','Do not know') ~ 'Unknown',
                                       is.na(VI) & is.na(TQ) ~ 'Unknown',
                                       TRUE ~ 'Error?'))
      
      y<-factor(data[['CountryIncome']],levels = c('UK','Other high income','Middle income','Low income','Unknown'),
                labels = c('UK','Other high income','Middle income','Low income','Unknown'),ordered = FALSE)
      
      return(y)
      
      
    }


