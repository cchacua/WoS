dbGetQuery(con, "SET search_path TO raw_data;")

dlist <- dbListTables(con)
dlist <- as.vector(dlist)

count_null_wos<-function(row_d){
  wos_sum <- dbGetQuery(con, paste0("SELECT * FROM raw_data.", as.character(row_d)))
  na_count <- as.data.frame(sapply(wos_sum, function(y) sum(is.na(y))))
  #print(colnames(na_count))
  if(nrow(na_count)>0){
    colnames(na_count)<-"number"
    na_count <- rbind(paste0(as.character(row_d),": ", as.character(nrow(wos_sum)), " rows"), na_count) 
    write.csv(na_count, paste0("../output/countings_sample/by_table/",as.character(row_d),".csv"))
  }
  else(print(as.character(row_d)))
}

count_null_wos(dlist[2])
lapply(dlist, count_null_wos)


merge_all<-merge.csv("../output/countings_sample/by_table/")
write.csv(merge_all, paste0("../output/countings_sample/","All_tables",".csv"))
