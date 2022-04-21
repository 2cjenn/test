This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_HTN_dur <- function(data){
      data=data%>%
        mutate(
          # Duration from TQ
          TEU_TQ_HTN_dur=TEU_BaC_AgeAtRec-HMH_HTNAge,
          # Combine duration from TQ & VI
          # Only take values from TQ when VI is not available, otherwise always take values from VI
          dur=ifelse(!is.na(TEU_TQ_HTN_dur) & is.na(TEU_VeI_HTN_dur),TEU_TQ_HTN_dur,TEU_VeI_HTN_dur)
          )
      # Should set duration<0 to be NA
      y<-data[['dur']];y[y<0]=NA
      
      return(y)
    }
```


