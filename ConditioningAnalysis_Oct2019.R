
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


js_data_start <- js_data %>% filter(start_stop =="START") %>% group_by(id,date)
js_data_stop <- js_data %>% filter(start_stop == "STOP") %>% group_by(id,date)

js_data_start <- js_data_start %>% mutate(start = time)
js_data_stop <- js_data_stop %>% mutate(stop = time)

js_data <- cbind(js_data_start,js_data_stop)

js_data <- js_data %>% select(-c("start_stop","start_stop1","id1","time","time1","date1"))

js_data <- js_data %>% mutate(duration = stop-start)
#js <- js %>% select(c("id","date","start","stop","duration"))
js_data <- js_data %>% mutate(observer = rep("JS"))

js_data <- js_data %>% mutate(minutes=start/60)

oct_data <- oct_data %>% mutate(minutes=start/60)

col_order <- c("id","date","observer","minutes","start","stop","duration")

js_data <- js_data[ , col_order]
oct_data <- oct_data[ , col_order]

october_data <- bind_rows(oct_data,js_data)

october_data <- october_data %>% filter(minutes != "NA")

october_data <- october_data %>% mutate(year=rep(2019))

october_data$date <- paste(october_data$date,october_data$year,sep = "")

write.csv(js,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/js_october_data.csv")

#october_data <- october_data %>% filter(minutes>=20)

october_data$date <- as.Date(october_data$date, format = "%b%d%Y")


#groupdata alterations

groupdata <- groupdata %>% separate(time.in,into = c("time.in.hr","time.in.mins"),sep = ":",convert = TRUE) %>% separate(time.change, into = c("time.change.hr","time.change.mins"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.in.mins=time.in.mins/60) %>% mutate(time.change.mins=time.change.mins/60)

groupdata <- groupdata %>% mutate(time.in=time.in.hr+time.in.mins) %>% mutate(time.change=time.change.hr+time.change.mins) %>% mutate(video.change=time.change-time.in)

groupdata <- groupdata %>% mutate(video.change=video.change*60)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("id","date","group","field","experiment","video.change"))

detach(package:plyr)
one_monthdata <- merge(october_data,groupdata,by=c("id","date"))

one_monthdata <- one_monthdata %>% group_by(id,date,observer,field,group) %>% filter(minutes >= video.change)

one_monthdata_obs <- one_monthdata %>% group_by(id,date,observer,field,group) %>% summarise(total.duration=sum(duration))

one_monthdata <- one_monthdata_obs %>% group_by(id,date,field,group) %>% summarise(mean.duration=mean(total.duration))

write.csv(one_monthdata,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/one_monthdata_observers.csv")

one_monthdata_alt <- one_monthdata %>% filter(id != "L190") #%>% filter(id != "L191")


wilcox.test(mean.duration~field,one_monthdata)
