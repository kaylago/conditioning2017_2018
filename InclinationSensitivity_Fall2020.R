
rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Fall/DataSheets/2020_Fall_InclSensExp/")
temp1 = list.files(pattern="*.csv",full.name=TRUE)
data = ldply(temp1, read.csv, header= T)



#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Fall/DataSheets/TurtleGroupData_Fall2020_B.csv",header=T)


columns <- c("turtle_id","obs_date","description","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data <- renameFunction(data,columns)


detach(package:plyr)
library(dplyr)



data <- data %>% dplyr::select(c("turtle_id","behavior","start","stop","duration"))



library(tidyr)

data <- data %>% separate(turtle_id, into = c('turtle.id','observer'),sep=-3,convert=TRUE)

data <-data %>% separate(turtle.id, into = c('turtle.id','date'),sep=5,convert=TRUE)

data <- data %>% separate(date, into = c('date','delete1'),sep="_",convert=TRUE)

data <- data %>% separate(observer, into = c('del2','observer'),sep="_",convert=TRUE)

data$observer <- ifelse(is.na(data$observer), data$del2, data$observer)

data <- data %>% separate(turtle.id, into = c('turtle.id','del3'),sep=-1,convert=TRUE)

data <- data %>% mutate(year=rep(2020))

data$date <- paste(data$year,data$date,sep = "")

data$date <- as.Date(data$date, format = "%Y%b%d")

data <- data %>% select(-c("delete1","del2","del3","year"))

col_order <- c("turtle.id","date","observer","start","stop","duration")

data <- data %>% mutate(minutes = start/60)


#groupdata alterations

groupdata <- groupdata %>% separate(time.change, into = c("time.change.mins","time.change.sec"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.change.sec=time.change.sec/60) %>% mutate(video.change = time.change.mins+time.change.sec)


groupdata <- groupdata %>% mutate(end.trial=video.change+20)
#l194 <- l194  %>% mutate(end.trial=video.change+15)

#groupdata<- rbind(groupdata,l194)

groupdata$date <- as.Date(groupdata$date,format= "%m/%d/%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","field.type","experiment","video.change","field.type","end.trial"))

groupdata <- groupdata %>% drop_na()

#combine data

detach(package:plyr)
data <- merge(data,groupdata,by=c("turtle.id","date"))

data <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% filter(minutes >= video.change) %>% filter(minutes <= end.trial)

data_obs <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% summarise(total.duration=sum(duration))

#data_obs<- data_obs %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))



data <- data_obs %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.duration))


data<- data %>% mutate(freq=mean.duration/1200)


