library(stringr)
library(tidyr)
#https://stackoverflow.com/questions/10554741/fill-in-data-frame-with-values-from-rows-above

path_files<-list.files("../output/path_summary",full.names=TRUE)
path_files
file<-read.table(path_files[7], header = FALSE, sep = ":", stringsAsFactors=FALSE)
#print(file$V1[7])
file$level<-(nchar(file$V1)-nchar(gsub("^\\s+","", file$V1)))/2
file$tag<-gsub("^\\s+","", file$V1)

file$obs_count<-as.numeric(str_extract(file$V2,"^([ 0-9]+)"))
#file$obs_count<-as.numeric(regmatches(file$V2,regexpr("^([ 0-9]+)", file$V2)))
file$is_attribute<-str_detect(file$V2,"@")
file$is_leaf<-str_detect(file$V2,"leaf")
file$type<-str_extract(file$V2,"strings|integers|integer|string|values|numeric")

levels<-as.numeric(unique(file$level))

fill_col<-function(nlevel, df=file){
  df$tem_lev<-fill(data=data.frame(column=ifelse(df$level==nlevel, df$tag, NA)), column , .direction="down")
  #print(colnames(df)[ncol(df)])
  colnames(df)[ncol(df)]<-paste0("l",nlevel)
  #print(colnames(df)[ncol(df)])
  return(df)
}

for (i in 0:length(levels)) {
  file<-fill_col(i)
  }

file<-as.data.frame(file)

for (i in 1:nrow(file)) {
  file[i, (ncol(file)-length(levels)+as.numeric(file$level[i])+1):ncol(file)]<-NA
}


#file$l3<-fill(data=data.frame(column=ifelse(file$level==3, file$tag, NA)), column , .direction="down")
  
