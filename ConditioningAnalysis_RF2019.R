#2019 RF Conditioning Data

rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/December_RF/")
temp = list.files(pattern="*.csv")
rf_data = lapply(temp, read.csv,header= T)

groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/TurtleGroups_Fa2019.csv",header=T)



library(dplyr)
rf_data = rf_data[order(sapply(rf_data,ncol),decreasing = F)]

rf_data_15 <- rf_data[c(15:58)]
rf_data_14 <- rf_data[c(1:14)]
rf_data_16 <- rf_data[c(59:74)]

detach(package:dplyr)
library(plyr)

columns_15<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

columns_14<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","modifiers","type","start","stop","duration","comment_start","comment_stop")

columns_16<- c("turtle.id","obs_date","description","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

rf_data_15 = ldply(rf_data_15,renameFunction,columns_15)

rf_data_14 = ldply(rf_data_14,renameFunction,columns_14)

rf_data_16 = ldply(rf_data_16,renameFunction,columns_16)

detach(package:plyr)
library(dplyr)
rf_data_15 <- rf_data_15 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))

rf_data_14 <- rf_data_14 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))

rf_data_16 <- rf_data_16 %>% select(-c("delete","obs_date","description","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))


rf_data <- rbind(rf_data_15,rf_data_14)

rf_data <- rbind(rf_data,rf_data_16)





library(tidyr)

rf_data <- rf_data %>% separate(turtle.id, into = c('turtle.id','date'), sep = 4, convert=TRUE)
rf_data <- rf_data %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
rf_data <- rf_data %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
rf_data <- rf_data %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
rf_data <- rf_data %>% select(-c("delete","delete2"))

rf_data <- rf_data %>% separate(observer, into = c('experiment','observer'), sep=2,convert=TRUE)
rf_data <- rf_data %>% separate(observer, into = c('delete','observer'),sep=-2,convert=TRUE)
rf_data <- rf_data %>% select(-c("delete"))




rf_data <- rf_data %>% mutate(minutes=start/60)

col_order <- c("turtle.id","date","observer","minutes","start","stop","duration")

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

groupdata <- groupdata %>% mutate(trial.end=video.change+20)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","experiment","video.change","trial.end"))


#combine data sheets

detach(package:plyr)
rf_data <- merge(rf_data,groupdata,by=c("turtle.id","date"))

rf_data <- rf_data %>% group_by(turtle.id,date,observer,field,group) %>% filter(minutes >= video.change) %>% filter(minutes<= trial.end)

rf_data <- rf_data  %>% mutate(end.trial=video.change+20) %>% filter(minutes <= end.trial)


rf_data_obs <- rf_data %>% group_by(turtle.id,date,observer,field,group) %>% summarise(total.duration=sum(duration))

rf_data_obs <- rf_data_obs %>% arrange(turtle.id,date)

rf_data <- rf_data_obs %>% group_by(turtle.id,date,field,group) %>% summarise(mean.duration=mean(total.duration))

rf_data <- rf_data %>% arrange(turtle.id,date)

rf_data <- rf_data %>% mutate(freq=mean.duration/1200)

observer_difference <- rf_data_obs %>% group_by(turtle.id,date) %>% summarize(difference=max(total.duration)-min(total.duration))

mean(observer_difference$difference)


#######
#write.csv(rf_data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/rf_data_observers_3-9-21.csv")
################
#write.csv(rf_data,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/rf_data_6-28.csv")

#one_monthdata_alt <- one_monthdata %>% filter(id != "L190") #%>% filter(id != "L191")
library(car)
leveneTest(mean.duration~field,data=rf_data) #passes

wilcox.test(freq~field,rf_data)

wilcox.test(freq~field,rf_data,paired=TRUE)

t.test(freq~field,rf_data)

nov_data <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/nov_data_observers_2-17-2021_updated.csv")

nov_data <- nov_data %>% select(-c(X))

nov_data <- nov_data %>% group_by(turtle.id,date,field,group) %>% summarise(mean.duration=mean(total.duration))

nov_data <- nov_data %>% mutate(exper=rep("2mo"))

nov_data <- nov_data %>% mutate(freq=mean.duration/1200)

nov_data$date <- as.Date(nov_data$date,format= "%m/%d/%Y")

#nov_data <- nov_data %>% select(-c("X"))

rf_data <- rf_data %>% mutate(exper=rep("rf"))

detach(package:plyr)
library(dplyr)

nov_data_obs<-as.data.frame(nov_data_obs)
rf_data_obs<-as.data.frame(rf_data_obs)

all_obs <- rbind(rf_data_obs,nov_data_obs)

observer_difference_all <- all_obs %>% group_by(turtle.id,date) %>% summarize(difference=max(total.duration)-min(total.duration))

mean(observer_difference_all$difference)

nov_data<- as.data.frame(nov_data)
rf_data<- as.data.frame(rf_data)

alldata <-rbind(nov_data,rf_data)

alldata <- alldata %>% arrange(turtle.id)

wilcox.test(freq~exper,subset(alldata,field=="MA"),paired=TRUE)

wilcox.test(freq~exper,subset(alldata,field=="MA"),paired=TRUE)
t.test(freq~exper,subset(alldata,field=="MA"))

alldata$field <- as.character(alldata$field)

alldata <- alldata %>% mutate(field=ifelse(exper=="2mo"&field=="MA","MA1",ifelse(exper=="rf" & field=="MA","MA2",field)))

alldata$field <- factor(alldata$field, levels=c("FL","MA1","MA2","MA/RF"),labels = c("FL","MA-1","MA-2","MA-RF"))


wilcox.test(freq~field,nov_data,paired=TRUE)

attach(alldata)
pairwise.wilcox.test(freq,field,data=alldata,paired=TRUE,p.adjust.method = "BH")
detach(alldata)

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)



annotation_df1 <- data.frame(field.type=rep(c("MA","MA")), y=c(0.076,0.08))

annotation_df2 <- data.frame(field.type=rep(c("MA","MA/RF")),y=c(0.08,0.08))

annotation_df3 <- data.frame(field.type=rep(c("MA/RF","MA/RF")),y=c(0.08,0.076))

rfplot<-ggplot(rf_data,aes(x=field,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="black",fill=c("olivedrab","palegreen3"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.16))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment",labels=c("MA"="Massachusetts","MA/RF"="Massachusetts-RF"))+
  #ggtitle("Radiofrequency") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="MA",xend="MA/RF",y=0.08,yend=0.08))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.09),
           label = c("p = 0.9"),
           family = "Calibri", fontface = 3, size=5)
rfplot

ggsave(rfplot, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/rf_ma_only_12-15-21.png",  bg = "transparent")


annotation_df1 <- data.frame(field.type=rep(c("FL","FL")),
                             y=c(0.086,0.09))

annotation_df2 <- data.frame(field.type=rep(c("FL","MA")),
                             y=c(0.09,0.09))

annotation_df3 <- data.frame(field.type=rep(c("MA","MA")),
                             y=c(0.09,0.086))



nov_plot<-ggplot(nov_data,aes(x=field,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="black",fill=c("goldenrod","springgreen4"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.16))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment",labels=c("FL"="Florida \n(control)","MA"="Massachusetts \n(conditioned)"))+
  ggtitle("") +
  theme(text=element_text(size=22,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="FL",xend="MA",y=0.09,yend=0.09))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.1),
           label = c("p = 0.002*"),
           family = "Calibri", fontface = 3, size=5)
nov_plot

ggsave(nov_plot, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/novplot_2019_12-15-21.png",  bg = "transparent")


anno<- data.frame(field=c("FL","MA-1","MA-2","MA-RF"),
                             y=c(0.086,0.09,0.1,0))


all_plot<-ggplot(alldata,aes(x=field,y=freq),group=exper)+
  stat_summary(fun.y= mean,geom="bar",color="black",fill=c("goldenrod","springgreen4","olivedrab","palegreen3"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.2))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment",labels=c("FL"="Florida \n(control)","MA-1"="Massachusetts-1","MA-2"="Massachusetts-2","MA-RF"="Massachusetts-RF"))+
  ggtitle("") +
  theme(text=element_text(size=22,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  geom_segment(aes(x="FL",xend="MA-1"),y=0.08,yend=0.08)+
  geom_segment(aes(x="FL",xend="FL"),y=0.075,yend=0.08)+
  geom_segment(aes(x="MA-1",xend="MA-1"),y=0.075,yend=0.08)+
  geom_segment(aes(x="MA-1",xend="MA-2"),y=0.13,yend=0.13)+
  geom_segment(aes(x="MA-1",xend="MA-1"),y=0.13,yend=0.125)+
  geom_segment(aes(x="MA-2",xend="MA-2"),y=0.13,yend=0.125)+
  geom_segment(aes(x="MA-2",xend="MA-RF"),y=0.16,yend=0.16)+
  geom_segment(aes(x="MA-2",xend="MA-2"),y=0.155,yend=0.16)+
  geom_segment(aes(x="MA-RF",xend="MA-RF"),y=0.16,yend=0.155)+
  #geom_segment(aes(x="MA-RF",xend="MA-RF"),y=0.15,yend=0.155)+
  annotate("text",
           x = c(1.5),
           y = c(0.09),
           label = c("p = 0.002*"),
           family = "Calibri", fontface = 3, size=5)+
  annotate("text",
         x = c(2.5),
         y = c(0.14),
         label = c("p = 0.9"),
         family = "Calibri", fontface = 3, size=5)+
  annotate("text",
           x = c(3.5),
           y = c(0.175),
           label = c("p = 0.9"),
           family = "Calibri", fontface = 3, size=5)


all_plot

ggsave(all_plot, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/rf_all_2019_12-15-21.png",  bg = "transparent")
