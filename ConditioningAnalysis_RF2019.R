#2019 RF Conditioning Data

rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/December_RF/")
temp = list.files(pattern="*.csv")
rf_data = lapply(temp, read.csv,header= T)

groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/TurtleGroups_Fa2019.csv",header=T)



library(dplyr)
rf_data = rf_data[order(sapply(rf_data,ncol),decreasing = T)]

rf_data_15 <- rf_data[c(1:19)]
rf_data_14 <- rf_data[c(20:34)]


detach(package:dplyr)
library(plyr)

columns_15<- c("turtle_id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

columns_14<- c("turtle_id","obs_date","media_file","length","fps","subject","behavior","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

rf_data_15 = ldply(rf_data_15,renameFunction,columns_15)

rf_data_14 = ldply(rf_data_14,renameFunction,columns_14)

detach(package:plyr)
library(dplyr)
rf_data_15 <- rf_data_15 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))

rf_data_14 <- rf_data_14 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))

rf_data <- rbind(rf_data_15,rf_data_14)


#setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/JS_OctoberData/")
#temp = list.files(pattern="*.csv")
#js_data = lapply(temp, read.csv,skip=15,header= T)


#detach(package:dplyr)

#library(plyr)
#columns2 <- c("time","media_file","length","fps","subject","behavior","comment","status","start_stop")

#js_data = ldply(js_data,renameFunction,columns2)

#detach(package:plyr)
#library(dplyr)
#js_data <- js_data %>% select(-c("length","fps","behavior","comment","status","subject"))


#js_data <- js_data %>% separate(media_file, into = c('file_path', 'date'), sep = -9, convert = TRUE)

#js_data <- js_data %>% separate(date, into = c('date','file'),sep=-4,convert=TRUE)

#js_data <- js_data %>% separate(file_path, into = c('file_path','id'),sep=-5,convert=TRUE)

#js_data <- js_data %>% separate(id, into=c('id','delete'),sep=-1,convert=TRUE)

#js_data <- js_data %>% select(-c("file_path","file","delete"))

library(tidyr)

rf_data <- rf_data %>% separate(turtle_id, into = c('id','date'), sep = 4, convert=TRUE)
rf_data <- rf_data %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
rf_data <- rf_data %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
rf_data <- rf_data %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
rf_data <- rf_data %>% select(-c("delete","delete2"))

rf_data <- rf_data %>% separate(observer, into = c('experiment','observer'), sep=2,convert=TRUE)
rf_data <- rf_data %>% separate(observer, into = c('delete','observer'),sep=-2,convert=TRUE)
rf_data <- rf_data %>% select(-c("delete"))




rf_data <- rf_data %>% mutate(minutes=start/60)

col_order <- c("id","date","observer","minutes","start","stop","duration")

#js_data <- js_data[ , col_order]
rf_data <- rf_data[ , col_order]

#october_data <- bind_rows(oct_data,js_data)

#rf_data <- rf_data %>% filter(minutes != "NA")

rf_data <- rf_data %>% mutate(year=rep(2019))

rf_data$date <- paste(rf_data$date,rf_data$year,sep = "")

rf_data$date <- as.Date(rf_data$date, format = "%b%d%Y")

#write.csv(js,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/js_october_data.csv")


#groupdata alterations

groupdata <- groupdata %>% separate(time.in,into = c("time.in.hr","time.in.mins"),sep = ":",convert = TRUE) %>% separate(time.change, into = c("time.change.hr","time.change.mins"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.in.mins=time.in.mins/60) %>% mutate(time.change.mins=time.change.mins/60)

groupdata <- groupdata %>% mutate(time.in=time.in.hr+time.in.mins) %>% mutate(time.change=time.change.hr+time.change.mins) %>% mutate(video.change=time.change-time.in)

groupdata <- groupdata %>% mutate(video.change=video.change*60)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("id","date","group","field","experiment","video.change"))


#combine data sheets

detach(package:plyr)
rf_data <- merge(rf_data,groupdata,by=c("id","date"))

rf_data <- rf_data %>% group_by(id,date,observer,field,group) %>% filter(minutes >= video.change)

rf_data_obs <- rf_data %>% group_by(id,date,observer,field,group) %>% summarise(total.duration=sum(duration))

rf_data_obs <- rf_data_obs %>% arrange(id)

rf_data <- rf_data_obs %>% group_by(id,date,field,group) %>% summarise(mean.duration=mean(total.duration))

write.csv(rf_data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/rf_data_observers.csv")

#one_monthdata_alt <- one_monthdata %>% filter(id != "L190") #%>% filter(id != "L191")


wilcox.test(mean.duration~field,rf_data)

