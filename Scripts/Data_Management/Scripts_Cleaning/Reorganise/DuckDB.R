library(tidyverse)
library(DBI)
library(vroom)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), "duck.db")

df <- vroom("K:/TEU/UKB33952_Data/Data_Downloads/v2.0_B2006022_R38358/R/Alldata/ukb38358.tab", 
            col_names=TRUE, delim="\t", 
            n_max=25000)
copy_to(con, df, name="UKB_1", overwrite=TRUE, temporary=FALSE)
tbl1 <- tbl(con, from="UKB_1")
data <- collect(tbl1)


for(row in seq(1, 502520, by=25000)){
  
  overwrite <- row==1
  
  for(col in seq(1, 9367,by=500)){
    print(paste0("row ", row, ", col ", col))
    
    if(col==1){
      cols <- c(1, seq(2, 500, by=1))
    } else if (col==9001){
      cols <- c(1, seq(9001, 9367, by=1))
    } else {
      cols <- c(1, seq(col, col+499, by=1))
    }
    df <- vroom("K:/TEU/UKB33952_Data/Data_Downloads/v2.0_B2006022_R38358/R/Alldata/ukb38358.tab", 
                col_names=TRUE, delim="\t", 
                skip=row-1, n_max=25000, col_select=cols)
    copy_to(con, df, name=paste0("UKB_", col), overwrite=overwrite, append=TRUE, temporary=FALSE)
  }
}
for(col in seq(1, 3500,by=500)){
  tbl <- tbl(con, from=paste0("UKB_", col))
  
  if(col==1){
    pendingtable <- tbl
  } else if (col > 1) {
    pendingtable <- inner_join(pendingtable, tbl, by="f.eid")
  }
}

db <- pendingtable %>% select(f.eid, f.21.0.0, f.35.0.0) %>% collect
db <- collect(pendingtable)

dbDisconnect(con)