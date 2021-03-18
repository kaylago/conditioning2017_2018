#############Conditioning 2017_Shorter File


rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/Behavioral_Observations/Waddling/Updated_CSV_WaddleFiles/")
temp = list.files(pattern="*.csv")
data = lapply(temp, read.csv, header= T)

groupdata<-read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/2017_Group_Data.csv",header=T)

data = data[order(sapply(data,nrow),decreasing = F)]


data_9 <-data[0:30]


library(plyr)

columns_9 <- c("time","media.file.path","total.length","fps","subject","behavior","comment","status","start_stop")

#columns_8 <- c("time","media.file.path","total.length","fps","subject","behavior","comment","status")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

#data_8 = ldply(data_8,renameFunction,columns_8)
data = ldply(data_9,renameFunction,columns_9)



detach(package:plyr)
library(dplyr)

data <- data %>% select(-c("total.length","fps","subject","comment","status"))

library(tidyr)
data <- data %>% separate(media.file.path, into = c('media.file.path', 'date'), sep = -9, convert = TRUE)

data <- data %>% separate(date, into = c('date','file.type'),sep=-4,convert=TRUE)

data <- data %>% separate(date,into = c('month','day'),sep=-2,convert=TRUE)

data <- data %>% mutate(day=ifelse(day=="c9",9,day)) %>% mutate(month = ifelse(month=="_De","Dec","Dec"))

data$day <- as.numeric(data$day)

data <- data %>% drop_na(day)

data <- data %>% mutate(day=ifelse(day<10,paste0("0",day),day))

#data <- data %>% filter(day>1)

data <- data %>% mutate(date=paste(month,day,sep = ""))

data <- data %>% mutate(year=rep(2017)) %>% mutate(date=paste(date,year,sep=""))

data$date <- as.Date(data$date, format = "%b%d%Y")

data <- data %>% separate(media.file.path, into = c('media.file.path','turtle.id'),sep=-5,convert=TRUE)

data <- data %>% arrange((date))

data_09 <- data %>% filter(day=="09")

data_09 <- data_09 %>% separate(turtle.id, into=c('delete','turtle.id'),sep=1,convert=TRUE)

data_09 <- data_09 %>% select (-c(file.type,delete,month,day,year))

data_d <- data %>% filter(day!="09")

data_d <- data_d %>% separate(turtle.id, into=c('turtle.id','delete'),sep=-1,convert=TRUE)

data_d <- data_d %>% select (-c(file.type,delete,month,day,year))

data <- rbind(data_09,data_d)

data <- data %>% filter(behavior=="waddling")

data_start<-subset(data,start_stop== "START")
data_stop <- subset(data,start_stop== "STOP")

names(data_start)<-c("start","observer","turtle.id","behavior","start_stop","date")
names(data_stop)<-c("stop","observer","turtle.id","behavior","start_stop","date")

dec_data <- cbind(data_start,data_stop)
names(dec_data)<- c("start","observer","turtle.id","behavior","start_stop","date","stop","del1","del2","del3","del4","del5")

dec_data <- dec_data %>% select(-c("del1","del2","del3","del4","del5"))

dec_data <- dec_data %>% mutate(duration=stop-start)

dec_data <- dec_data %>% mutate(minutes=start/60)


groupdata <- groupdata %>% separate(time.in,into = c("time.in.hr","time.in.mins"),sep = ":",convert = TRUE) %>% separate(time.change, into = c("time.change.hr","time.change.mins"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.in.mins=time.in.mins/60) %>% mutate(time.change.mins=time.change.mins/60)

groupdata <- groupdata %>% mutate(time.in=time.in.hr+time.in.mins) %>% mutate(time.change=time.change.hr+time.change.mins) %>% mutate(video.change=time.change-time.in)

groupdata <- groupdata %>% mutate(video.change=video.change*60)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","field.type","video.change"))

detach(package:plyr)

dec_data <- merge(dec_data,groupdata,by=c("turtle.id","date"))

dec_data <- dec_data %>% arrange(turtle.id,date,observer,minutes)

dec_data <- dec_data %>% mutate(trial.end=video.change+20)

dec_data <- dec_data %>% filter(minutes>=video.change) %>% filter(minutes<=trial.end)

dec_data_obs <- dec_data %>% group_by(turtle.id,date,observer,field,field.type)%>% summarize(total.dur=sum(duration))

#write.csv(dec_data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/2017_data_observers_11-10-20.csv")



###DATA Skip 15 Lines

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/Behavioral_Observations/Waddling/Waddle_CSV_Skip15/")
temp = list.files(pattern="*.csv")
data = lapply(temp, read.csv,skip=15, header= T)

library(plyr)
data = ldply(data,renameFunction,columns_9)

detach(package:plyr)
library(dplyr)

data <- data %>% select(-c("total.length","fps","subject","comment","status"))

library(tidyr)
data <- data %>% separate(media.file.path, into = c('media.file.path', 'date'), sep = -9, convert = TRUE)

data <- data %>% separate(date, into = c('date','file.type'),sep=-4,convert=TRUE)

data <- data %>% separate(date,into = c('month','day'),sep=-2,convert=TRUE)

data <- data %>% mutate(day=ifelse(day=="c9",9,day)) %>% mutate(month = ifelse(month=="_De","Dec","Dec"))

data$day <- as.numeric(data$day)

data <- data %>% mutate(day=ifelse(day<10,paste0("0",day),day))

#data <- data %>% filter(day>1)

data <- data %>% mutate(date=paste(month,day,sep = ""))

data <- data %>% mutate(year=rep(2017)) %>% mutate(date=paste(date,year,sep=""))

data$date <- as.Date(data$date, format = "%b%d%Y")

data <- data %>% separate(media.file.path, into = c('media.file.path','turtle.id'),sep=-5,convert=TRUE)

data <- data %>% arrange((date))

data_09 <- data %>% filter(day=="09")

data_09 <- data_09 %>% separate(turtle.id, into=c('delete','turtle.id'),sep=1,convert=TRUE)

data_09 <- data_09 %>% select (-c(file.type,delete,month,day,year))

data_d <- data %>% filter(day!="09")

data_d <- data_d %>% separate(turtle.id, into=c('turtle.id','delete'),sep=-1,convert=TRUE)

data_d <- data_d %>% select (-c(file.type,delete,month,day,year))

data <- rbind(data_09,data_d)

data <- data %>% filter(behavior=="waddling")

data_start<-subset(data,start_stop== "START")
data_stop <- subset(data,start_stop== "STOP")

names(data_start)<-c("start","observer","turtle.id","behavior","start_stop","date")
names(data_stop)<-c("stop","observer","turtle.id","behavior","start_stop","date")

data <- cbind(data_start,data_stop)
names(data)<- c("start","observer","turtle.id","behavior","start_stop","date","stop","del1","del2","del3","del4","del5")

data <- data %>% select(-c("del1","del2","del3","del4","del5"))

data <- data %>% mutate(duration=stop-start)

data <- data %>% mutate(minutes=start/60)

data <- merge(data,groupdata, by=c("turtle.id","date"))

data <- data %>% mutate(trial.end=video.change+20)

data <- data %>% filter(minutes>=video.change) %>% filter(minutes<=trial.end)

data <- data %>% group_by(turtle.id,date,observer,field,field.type)%>% summarize(total.dur=sum(duration))

######NEW DATA

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/Behavioral_Observations/Waddling/NEW_DATA/")
temp = list.files(pattern="*.csv")
data2 = lapply(temp, read.csv, header= T)

library(dplyr)
data2 = data2[order(sapply(data2,ncol),decreasing = F)]


data_15 <- data2[c(1:24)]
data_16 <- data2[c(25:29)]

  
detach(package:dplyr)
library(plyr)

columns_15<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

columns_16<- c("turtle.id","obs_date","description","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data_15 = ldply(data_15,renameFunction,columns_15)
data_16 =  ldply(data_16,renameFunction,columns_16)

detach(package:plyr)
library(dplyr)
data_15 <- data_15 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))

data_16 <- data_16 %>% select(-c("delete","obs_date","description","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))

#nov_data_14 <- nov_data_14 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))
data2 <- rbind(data_15,data_16)

#data2 <- data2 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))

library(tidyr)
#library(dplyr)

data2 <- data2 %>% separate(turtle.id, into = c('turtle.id','date'), sep = 4, convert=TRUE)
data2 <- data2 %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
data2 <- data2 %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
data2 <- data2 %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
data2 <- data2 %>% select(-c("delete","delete2"))


col_order <- c("turtle.id","date","observer","minutes","start","stop","duration")

data2 <- data2[ , col_order]

data2 <- data2 %>% mutate(year=rep(2017))

data2$date <- paste(data2$date,data2$year,sep = "")

data2$date <- as.Date(data2$date, format = "%b%d%Y")

data2 <- merge(data2,groupdata,by=c("turtle.id","date"))

data2 <- data2 %>% mutate(minutes=start/60)

data2 <- data2 %>% mutate(trial.end=video.change+20)

data2 <- data2 %>% filter(minutes>=video.change) %>% filter(minutes<=trial.end)

data2_obs <- data2 %>% group_by(turtle.id,date,observer,field,field.type)%>% summarize(total.dur=sum(duration))

dec_data_obs <- rbind(dec_data_obs,data2_obs)

dec_data_obs <- rbind(dec_data_obs,data)

dec_data_obs <- dec_data_obs %>% arrange(turtle.id,date)

dec_data_obs <- dec_data_obs %>% mutate(group= ifelse(field=="BH"  & field.type=="conditioned","purple", ifelse(field=="NB"&field.type=="control","purple","red")))



waddle_zero <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/Behavioral_Observations/Waddling/Waddling_zero.csv",header=T)

waddle_zero$date <- as.Date(waddle_zero$date,format="%m/%d/%Y")

detach(package:tidyr)

dec_data_total_obs <- bind_rows(dec_data_obs,waddle_zero)

dec_data_total_obs <- dec_data_total_obs %>% arrange(turtle.id,date)



write.csv(dec_data_total_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/2017_data_observers_2-17-2021_updated.csv")

library(dplyr)

observer_difference <- dec_data_total_obs %>% group_by(turtle.id,date) %>% summarize(difference=max(total.dur)-min(total.dur))

mean(observer_difference$difference) #15 seconds

dec_data_total <- dec_data_total_obs %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.dur))

dec_data_total<- dec_data_total %>% mutate(freq=mean.duration/1200)


attach(subset(dec_data_total,field.type=="conditioned"))
mean(freq)
detach()

attach(subset(dec_data_total,field.type=="control"))
mean(freq)
detach()

#dec_data_total <- dec_data_total %>% mutate(group= ifelse(field=="BH"  & field.type=="conditioned","purple", ifelse(field=="NB"&field.type=="control","purple","red")))

#write.csv(dec_data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/2017_data_observers_12-3.csv")
library(car)
leveneTest(mean.duration~field,data=dec_data_total)

wilcox.test(freq~field.type,data=dec_data_total,paired=TRUE)

wilcox.test(freq~field.type,data=dec_data_total)

wilcox.test(freq~field.type,subset(dec_data_total,group=="purple"),paired=TRUE)

t.test(freq~field.type,subset(dec_data_total,group=="purple"))

t.test(mean.duration~field.type,subset(dec_data_total,group=="purple"))

wilcox.test(freq~field.type,subset(dec_data_total,group=="red"),paired=TRUE)

t.test(freq~field.type,subset(dec_data_total,group=="red"))

t.test(mean.duration~field.type,subset(dec_data_total,group=="red"))

wilcox.test(freq~field.type,subset(dec_data_total,group=="purple"),paired=TRUE)

wilcox.test(freq~field.type,subset(dec_data_total,group=="red"),paired=TRUE)


library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)



annotation_df1 <- data.frame(field.type=rep(c("conditioned","conditioned")),
                             y=c(0.2,0.22))

annotation_df2 <- data.frame(field.type=rep(c("control","conditioned")),
                             y=c(0.22,0.22))

annotation_df3 <- data.frame(field.type=rep(c("control","control")),
                             y=c(0.22,0.2))


plot_17 <-ggplot(dec_data_total,aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="grey",fill="grey")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.18))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2),expand=c(0,0),limits=c(0,0.3))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  ggtitle("") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.0002"),
           family = "Calibri", fontface = 3, size=5)
plot_17

ggsave(plot_17, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/2017_plot.png",  bg = "transparent")


red_plot_17 <-ggplot(subset(dec_data_total,group=="red"),aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="#990000",fill="#990000")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2),expand=c(0,0),limits=c(0,0.3))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  ggtitle("") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.038"),
           family = "Calibri", fontface = 3, size=5)
red_plot_17

ggsave(red_plot_17, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/novascotia_2017_plot.png",  bg = "transparent")


pur_plot_17 <-ggplot(subset(dec_data_total,group=="purple"),aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="#CC99FF",fill="#CC99FF")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2),expand=c(0,0),limits=c(0,0.3))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  ggtitle("") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.004"),
           family = "Calibri", fontface = 3, size=5)
pur_plot_17

ggsave(pur_plot_17, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/bahamas_2017_plot.png",  bg = "transparent")









