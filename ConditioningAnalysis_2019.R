
#2019 One Month Conditioning Data

rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/October2019/")
temp = list.files(pattern="*.csv")
oct_data = lapply(temp, read.csv,header= T)

groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/TurtleGroups_Fa2019.csv",header=T)

detach(package:dplyr)
library(plyr)

columns<- c("turtle_id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")


renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

oct_data= ldply(oct_data,renameFunction,columns)

detach(package:plyr)
library(dplyr)
oct_data <- oct_data %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/JS_OctoberData/")
temp = list.files(pattern="*.csv")
js_data = lapply(temp, read.csv,skip=15,header= T)


detach(package:dplyr)

library(plyr)
columns2 <- c("time","media_file","length","fps","subject","behavior","comment","status","start_stop")

js_data = ldply(js_data,renameFunction,columns2)

detach(package:plyr)
library(dplyr)
js_data <- js_data %>% select(-c("length","fps","behavior","comment","status","subject"))

library(tidyr)
js_data <- js_data %>% separate(media_file, into = c('file_path', 'date'), sep = -9, convert = TRUE)

js_data <- js_data %>% separate(date, into = c('date','file'),sep=-4,convert=TRUE)

js_data <- js_data %>% separate(file_path, into = c('file_path','id'),sep=-5,convert=TRUE)

js_data <- js_data %>% separate(id, into=c('id','delete'),sep=-1,convert=TRUE)

js_data <- js_data %>% select(-c("file_path","file","delete"))

oct_data <- oct_data %>% separate(turtle_id, into = c('id','date'), sep = 4, convert=TRUE)
oct_data <- oct_data %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
oct_data <- oct_data %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
oct_data <- oct_data %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
oct_data <- oct_data %>% select(-c("delete","delete2"))


js_data_start <- js_data %>% filter(start_stop =="START")
js_data_stop <- js_data %>% filter(start_stop == "STOP")

js_data_start <- js_data_start %>% mutate(start = time/60)
js_data_stop <- js_data_stop %>% mutate(stop = time/60)

js <- merge(js_data_start,js_data_stop,by=c("date","id"))

js <- js %>% mutate(duration = stop-start)
js <- js %>% select(c("id","date","start","stop","duration"))
js <- js %>% mutate(observer = rep("JS"))
