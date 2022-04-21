This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_MACE_fucomp <- function(data){
      data=data%>%
        mutate(TEU_MACE_fucomp=case_when(TEU_MACE_status==0  ~ NA_character_,
                                          TEU_MACE_status==1 & !is.na(TEU_MACE_eventdate) & TEU_MACE_eventdate==TEU_Dth_MACE_dthdate ~ 'CVD death',
                                          TRUE ~ TEU_HES_MACE_fucomp
        ))
      return(data$TEU_MACE_fucomp)
    }
```


