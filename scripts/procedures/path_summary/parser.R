library(stringr)
library(tidyr)
#https://stackoverflow.com/questions/10554741/fill-in-data-frame-with-values-from-rows-above

fill_col<-function(nlevel, df){
  df$tem_lev<-fill(data=data.frame(column=ifelse(df$level==nlevel, df$tag, NA)), column , .direction="down")
  #print(colnames(df)[ncol(df)])
  colnames(df)[ncol(df)]<-paste0("l",nlevel)
  #print(colnames(df)[ncol(df)])
  return(df)
}


path_files<-list.files("../output/path_summary",full.names=TRUE)
path_files


parse_pathfile<-function(path_file){
  file<-read.table(path_file, header = FALSE, sep = ":", stringsAsFactors=FALSE)
  #print(file$V1[7])
  file$level<-(nchar(file$V1)-nchar(gsub("^\\s+","", file$V1)))/2
  file$tag<-gsub("^\\s+","", file$V1)
  file$obs_count<-as.numeric(str_extract(file$V2,"^([ 0-9]+)"))
  #file$obs_count<-as.numeric(regmatches(file$V2,regexpr("^([ 0-9]+)", file$V2)))
  file$is_attribute<-str_detect(file$V2,"@")
  file$is_leaf<-str_detect(file$V2,"leaf")
  file$type<-str_extract(file$V2,"strings|integers|integer|string|values|numeric")
  
  levels<-as.numeric(unique(file$level))
  
  for (i in 0:length(levels)) {
    file<-fill_col(i, df=file)
  }
  
  for (i in 1:nrow(file)) {
    file[i, (ncol(file)-length(levels)+as.numeric(file$level[i])+1):ncol(file)]<-NA
  }
  
  file<-as.data.frame(file, stringsAsFactors=FALSE)
  class(file$l10)
  
  file[] <- lapply(file, function(x) if(is.data.frame(x)) as.character(x[,1]) else x)
  file[] <- lapply(file, function(x) if(is.factor(x)) as.character(x) else x)
  file$path<-""
  for (i in 1:nrow(file)) {
    pp<-file[i, (ncol(file)-length(levels)-1):(ncol(file)-length(levels)+as.numeric(file$level[i])-1)]
    #do.call(paste, as.data.frame(m, stringsAsFactors=FALSE))
    pp[] <- lapply(pp, function(x) if(is.factor(x)) as.character(x) else x)
    pp<-do.call(paste, c(pp, sep = "/")) 
    file$path[i]<-as.character(pp)
  }
  return(file)
}


df1<-parse_pathfile(path_files[7])

