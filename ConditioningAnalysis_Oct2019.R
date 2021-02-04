
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

js_data <- js_data %>% separate(file_path, into = c('file_path','turtle.id'),sep=-5,convert=TRUE)

js_data <- js_data %>% separate(turtle.id, into=c('turtle.id','delete'),sep=-1,convert=TRUE)

js_data <- js_data %>% select(-c("file_path","file","delete"))

oct_data <- oct_data %>% separate(turtle_id, into = c('turtle.id','date'), sep = 4, convert=TRUE)
oct_data <- oct_data %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
oct_data <- oct_data %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
oct_data <- oct_data %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
oct_data <- oct_data %>% select(-c("delete","delete2"))


js_data_start <- js_data %>% filter(start_stop =="START") %>% group_by(turtle.id,date)
js_data_stop <- js_data %>% filter(start_stop == "STOP") %>% group_by(turtle.id,date)

js_data_start <- js_data_start %>% mutate(start = time)
js_data_stop <- js_data_stop %>% mutate(stop = time)

js_data <- cbind(js_data_start,js_data_stop)

js_data <- js_data %>% select(-c("start_stop","start_stop1","turtle.id1","time","time1","date1"))

js_data <- js_data %>% mutate(duration = stop-start)
#js <- js %>% select(c("id","date","start","stop","duration"))
js_data <- js_data %>% mutate(observer = rep("JS"))

js_data <- js_data %>% mutate(minutes=start/60)

oct_data <- oct_data %>% mutate(minutes=start/60)

col_order <- c("turtle.id","date","observer","minutes","start","stop","duration")

js_data <- js_data[ , col_order]
oct_data <- oct_data[ , col_order]

october_data <- bind_rows(oct_data,js_data)

october_data <- october_data %>% filter(minutes != "NA")

october_data <- october_data %>% mutate(year=rep(2019))

october_data$date <- paste(october_data$date,october_data$year,sep = "")

#write.csv(js,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/js_october_data.csv")

#october_data <- october_data %>% filter(minutes>=20)

october_data$date <- as.Date(october_data$date, format = "%b%d%Y")


#groupdata alterations

groupdata <- groupdata %>% separate(time.in,into = c("time.in.hr","time.in.mins"),sep = ":",convert = TRUE) %>% separate(time.change, into = c("time.change.hr","time.change.mins"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.in.mins=time.in.mins/60) %>% mutate(time.change.mins=time.change.mins/60)

groupdata <- groupdata %>% mutate(time.in=time.in.hr+time.in.mins) %>% mutate(time.change=time.change.hr+time.change.mins) %>% mutate(video.change=time.change-time.in)

groupdata <- groupdata %>% mutate(video.change=video.change*60)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","experiment","video.change"))

detach(package:plyr)
one_monthdata <- merge(october_data,groupdata,by=c("turtle.id","date"))

one_monthdata <- one_monthdata %>% group_by(turtle.id,date,observer,field,group) %>% filter(minutes >= video.change)

one_monthdata_obs <- one_monthdata %>% group_by(turtle.id,date,observer,field,group) %>% summarise(total.duration=sum(duration))

one_monthdata <- one_monthdata_obs %>% group_by(turtle.id,date,field,group) %>% summarise(mean.duration=mean(total.duration))

write.csv(one_monthdata,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/one_monthdata_observers.csv")





one_monthdata <- one_monthdata %>% mutate(freq=mean.duration/1200)

one_monthdata_alt <- one_monthdata %>% filter(turtle.id != "L193") 

one_monthdata_alt <- one_monthdata_alt  %>% filter(turtle.id != "L191")

wilcox.test(freq~field,one_monthdata)

wilcox.test(freq~field,one_monthdata_alt)

yellow <- one_monthdata_alt %>% filter(group=="yellow")

green <- one_monthdata_alt %>% filter(group=="green")

wilcox.test(freq~field,yellow)
wilcox.test(freq~field,green)

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)



annotation_df1 <- data.frame(field.type=rep(c("FL","FL")),
                             y=c(0.045,0.048))

annotation_df2 <- data.frame(field.type=rep(c("FL","MA")),
                             y=c(0.048,0.048))

annotation_df3 <- data.frame(field.type=rep(c("MA","MA")),
                             y=c(0.048,0.045))


oct_plot<-ggplot(one_monthdata_alt,aes(x=field,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="grey",fill="grey")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.16))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  ggtitle("") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="FL",xend="MA",y=0.048,yend=0.048))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.051),
           label = c("p = 0.07692"),
           family = "Calibri", fontface = 3, size=5)
oct_plot

