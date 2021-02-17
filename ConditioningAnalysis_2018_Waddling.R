
#2018 Conditioning - Waddling Analysis


rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/Conditioning2018_Analysis/")
temp = list.files(pattern="*.csv")
data = lapply(temp, read.csv, skip=15, header= T)

groupdata<-read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/2018Groups.csv",header=T)

library(plyr)

columns<- c("Time","Media.file.path","Total.length","FPS","Subject","Behavior","Comment","Status","Start_Stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data= ldply(data,renameFunction,columns)


detach(package:plyr)
library(dplyr)

data <- data %>% select(-c("Total.length","FPS","Subject","Comment","Status"))



library(tidyr)
data <- data %>% separate(Media.file.path, into = c('media.file.path', 'date'), sep = -9, convert = TRUE)

data <- data %>% separate(date, into = c('date','file.type'),sep=-4,convert=TRUE)

data <- data %>% separate(media.file.path, into = c('media.file.path','ID'),sep=-5,convert=TRUE)

data <- data %>% separate(ID, into=c('ID','delete'),sep=-1,convert=TRUE)

data <- data %>% select (-c(file.type,delete))

#dfs <- dfs %>% filter(Time != "Time")

names(data)<- c("time","observer.file","id","date","behavior","start_stop")

class(data$date)


data <- data %>% mutate (date = ifelse(date=="NOV28","28-Nov-18",ifelse(date=="NOV29","29-Nov-18",ifelse(date=="NOV30","30-Nov-18",ifelse(date=="DEC01","01-Dec-18","02-Dec-18")))))

data$date<-as.Date(data$date,format="%d-%b-%y")

groupdata$date<-as.Date(groupdata$date,format="%m/%d/%Y")
class(groupdata$date)


data_start<-subset(data,start_stop== "START")
names(data_start)<- c("start.time","observer.file","id","date","behavior","start_stop")


data_stop<-subset(data,start_stop== "STOP")
names(data_stop)<- c("stop.time","observer.file","id","date","behavior","start_stop")


dfs_behaviors<-cbind(data_start,data_stop)

names(dfs_behaviors)<- c("start","observer.file", "id","date","behavior","start_stop","stop","observer.file.2","id.2", "date.2","behavior.2","start_stop.2")

dfs_behaviors <-dfs_behaviors %>% select(-c("observer.file.2", "date.2","behavior.2","start_stop","start_stop.2","id.2"))

class(groupdata$id)
class(dfs_behaviors$id)
class(dfs_behaviors$date)

dfs_behaviors$id <- as.factor(dfs_behaviors$id)

dfs_behaviors<-merge(dfs_behaviors,groupdata,by=c("id","date"))

dfs_behaviors$start<- as.numeric(dfs_behaviors$start)
dfs_behaviors$stop<- as.numeric(dfs_behaviors$stop)

dfs_behaviors<- mutate(dfs_behaviors,time.el= stop-start)
dfs_behaviors<- dfs_behaviors %>% mutate(minutes= start/60)

timechange.file <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/TrialTimes2018.csv",header=T)

class(dfs_behaviors$id)
class(timechange.file$id)
class(dfs_behaviors$date)
class(timechange.file$date)

timechange.file$date <- as.Date(timechange.file$date,format= "%m/%d/%Y")



dfs_behaviors<- merge(dfs_behaviors,timechange.file, by = c("id","date"))

dfs_acclimation<- dfs_behaviors %>% filter(minutes< field.change)
dfs_test<-dfs_behaviors %>% filter(minutes>field.change)

dfs_test<-dfs_test %>% filter(minutes < (field.change+20))

dfs_acclim_summary <- dfs_acclimation %>% group_by(date,id,observer.file,group,field,field.type) %>% summarise(total.time.el=sum(time.el))
dfs_test_summary <- dfs_test %>% group_by(date,id,observer.file,group,field,field.type) %>% summarise(total.time.el=sum(time.el))

dfs_test_summary <- dfs_test_summary %>% arrange(id,date)

nov18_observers<- dfs_test_summary

names(nov18_observers) <- c("date","turtle.id","observer","group","field","field.type","mean.duration")


######NEW DATA

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/Conditioning2018_NewAnalysis/")
temp = list.files(pattern="*.csv")
data2 = lapply(temp, read.csv, header= T)

library(dplyr)
data2 = data2[order(sapply(data2,ncol),decreasing = F)]


data_15 <- data2[c(1:11)]
data_16 <- data2[c(12:22)]


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


col_order <- c("date","turtle.id","observer","start","stop","duration")

data2 <- data2[ , col_order]

data2 <- data2 %>% mutate(minutes=start/60)

data2 <- data2 %>% mutate(year=rep(2018))

data2$date <- paste(data2$date,data2$year,sep = "")

data2$date <- as.Date(data2$date, format = "%b%d%Y")

data2 <- data2 %>% filter(turtle.id != "")

names(groupdata) <- c("date","turtle.id","group","field","field.type")

data2<- as.data.frame(data2)

data2 <- merge(data2,groupdata,by=c("turtle.id","date"))

names(timechange.file) <- c("date","turtle.id","time.in","time.field.change","video.change")

data2 <- merge(data2,timechange.file,by=c("turtle.id","date"))

data2 <- data2 %>% mutate(trial.end=video.change+20)

data2 <- data2 %>% filter(minutes>=video.change) %>% filter(minutes<=trial.end)

#data2 <- unique(data2)

data2_obs <- data2 %>% group_by(date,turtle.id,observer,group,field,field.type)%>% summarise(mean.duration=sum(duration))

col_order <- c("date","turtle.id","observer","group","field","field.type","mean.duration")
data2_obs <- data2_obs[ , col_order]

data2_obs$turtle.id <- as.factor(data2_obs$turtle.id)

class(nov18_observers$turtle.id)

class(data2_obs$turtle.id)

class(nov18_observers$mean.duration)

class(data2_obs$mean.duration)

data2_obs <- as.data.frame(data2_obs)
########

nov18_observers <- as.data.frame(nov18_observers)

nov18_observers<- rbind(nov18_observers,data2_obs)

#L181_nov30 <- data2 %>% filter(turtle.id=="L181") %>% filter(observer != "KG")

#L181_nov30 <- L181_nov30 %>% group_by(turtle.id,date) %>% summarize(duration=sum(duration))

waddle_zero <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/Waddling_zero_2018.csv",header=T)

waddle_zero$date <- as.Date(waddle_zero$date,format="%m/%d/%Y")

nov18_observers <- rbind(nov18_observers,waddle_zero)

nov18_observers <- na.omit(nov18_observers)

nov18_observers <- nov18_observers %>% arrange(turtle.id,date)

observer_difference <- nov18_observers %>% group_by(turtle.id,date) %>% summarize(difference=max(mean.duration)-min(mean.duration))

mean(observer_difference$difference) #16.3 second

median(observer_difference$difference)

#write.csv(dfs_acclim_summary,file="C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/dfs_acclim_summary.csv")
write.csv(nov18_observers,file="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/2018_observers_data_2-13-2021_updated.csv")



#dfs_acclim_mean <- dfs_acclim_summary %>% group_by(date,id,group,field,field.type) %>% summarise(mean.time=mean(total.time.el))

dfs_nov18_total <- nov18_observers %>% group_by(date,turtle.id,group,field,field.type) %>% summarise(mean.duration=mean(mean.duration))

dfs_nov18_total <- dfs_nov18_total %>% mutate(freq=mean.duration/1200)

dfs_nov18_total <- dfs_nov18_total %>% arrange(turtle.id,date)



#dfsclean <- read.csv("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/dfs_cleaned.csv")

#fs_clean_mean <- dfsclean %>% group_by(date,id,group,field,field.type) %>% summarise(mean.time=mean(total.time.el))

#dfs_clean_mean <- dfs_clean_mean %>% filter(id != "L179" & id != "L184")

#dfs_test_mean <- dfs_test_mean  %>% filter(id != "L179" & id != "L184")

#class(dfs_test_mean$mean.time)

library(car)
leveneTest(mean.duration~field,data=dfs_nov18_total) #passes
bartlett.test(mean.duration~field,data=dfs_nov18_total) #passes
shapiro.test(dfs_nov18_total$mean.duration) # not normal

#dfs_test_mean <- dfs_test_mean %>% mutate(trans= log10(mean.time))

summary(lm(mean.time~field*group+id,data=dfs_test_mean))
anova(lm(mean.time~field*group+id,data=dfs_test_mean))

#library(nlme)
#lme1<-lme(mean.time~field.type+id,random = ~1|group,data=dfs_test_mean)
#summary(lme1)
a#nova(lme1)

#nov_2018 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2018/2018_observers_data_12-2-2020_num2.csv",header=TRUE)
observer_difference <- nov18_observers %>% group_by(turtle.id,date) %>% summarize(difference=max(mean.duration)-min(mean.duration))

mean(observer_difference$difference)#15


wilcox.test(freq~field.type,dfs_nov18_total,paired=TRUE)
#t.test(mean.time~field.type,dfs_test_mean)

wilcox.test(freq~field.type,dfs_nov18_total)

orange <- dfs_nov18_total %>% filter(group=="orange")

#orange <- orange %>% arrange(turtle.id,date)

#orange<- orange%>% filter(id !="L179" & id != "L184")
wilcox.test(freq~field.type,orange,paired=TRUE)

wilcox.test(freq~field.type,orange)

#t.test(mean.time~field.type,orange)

blue <- dfs_nov18_total %>% filter(group=="blue")

#blue <- blue %>% arrange(turtle.id,date)

wilcox.test(freq~field.type,blue,paired=TRUE)

wilcox.test(freq~field.type,blue)

#t.test(freq~field.type,blue)

#names(dfs_test_mean)<- c("date","turtle.id","group","field","field.type","mean.duration","freq")

write.csv(dfs_nov18_total,"C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/2018_testperiod_meansFINAL.csv")


#dec_data_total <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/2017_data_observers_12-01-20_num4.csv",header=T)

#library(dplyr)


#dec_data_total <- dec_data_total %>% group_by(turtle.id,date,field,field.type) %>% summarise(mean.duration=mean(total.dur))

#dec_data_total<- dec_data_total %>% mutate(freq=mean.duration/1200)

#dec_data_total <- dec_data_total %>% mutate(group= ifelse(field=="BH"  & field.type=="conditioned","purple", ifelse(field=="NB"&field.type=="control","purple","red")))

#wilcox.test(freq~field.type,data=dec_data_total,paired=TRUE)

#wilcox.test(freq~field.type,subset(dec_data_total,group=="purple"),paired=TRUE)

#wilcox.test(freq~field.type,subset(dec_data_total,group=="red"),paired=TRUE)



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


orangeplot<-ggplot(orange,aes(x=field.type,y=prop))+
  stat_summary(fun.y="mean",geom="bar",color="#FF9933",fill="#FF9933")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),geom="errorbar",color="black")+
  theme_bw()+
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15),expand = c(0,0),"Proportion of Time")+
  coord_cartesian(ylim=c(0,0.15))+
  scale_x_discrete("Treatment")+
  ggtitle("New England Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))
orangeplot

or_plot_18 <- ggplot(orange,aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="#FF9933",fill="#FF9933")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.17))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2,.3,0.4),expand=c(0,0),limits=c(0,0.5))+
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
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.039"),
           family = "Calibri", fontface = 3, size=5)
or_plot_18



blueplot<-ggplot(blue,aes(x=field.type,y=freq))+
  stat_summary(fun.y="mean",geom="bar",color="#99CCFF",fill="#99CCFF")+stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),geom="errorbar",color="black")+
  theme_bw()+
  theme(text=element_text(size=20,family="calibri")) +
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15),expand=c(0,0),name="Proportion of Time")+
  coord_cartesian(ylim=c(0,0.15))+
  scale_x_discrete(name="Treatment")+
  ggtitle("Cuba Group")+ 
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5))+
  theme(plot.margin=unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))
blueplot

library(ggpubr)
figure<- ggarrange(orangeplot,blueplot,labels= c("A","B"),ncol = 2,nrow = 1)
figure
#par(oma=c(2,2,2,2))
annotate_figure(figure,
                top = text_grob("Food-Seeking Behavior", color = "black", face = "bold", size = 28),
                bottom = text_grob("Treatment", color = "black",
                                   hjust = 0.5, face = "italic", size = 24),
                left = text_grob("Proportion of Time", color = "black", rot = 90,size=24,face="italic")
)



blu_plot_18 <- ggplot(blue,aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="#99CCFF",fill="#99CCFF")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.17))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2,.3,0.4),expand=c(0,0),limits=c(0,0.5))+
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
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.007"),
           family = "Calibri", fontface = 3, size=5)
blu_plot_18



red_plot_17 <-ggplot(subset(dec_data_total,group=="red"),aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="#990000",fill="#990000")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2,0.3,0.4),expand=c(0,0),limits=c(0,0.5))+
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
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.05"),
           family = "Calibri", fontface = 3, size=5)
red_plot_17

pur_plot_17 <-ggplot(subset(dec_data_total,group=="purple"),aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="#CC99FF",fill="#CC99FF")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks=c(0,0.04,0.08,0.12,0.2,0.3,0.4),expand=c(0,0),limits=c(0,0.5))+
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
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.007"),
           family = "Calibri", fontface = 3, size=5)
pur_plot_17



fig3<-ggplot(dfs_test_mean,aes(x=field.type,y=prop))+
  stat_summary(fun.y="mean",geom="bar",fill="grey48")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) +
                 sd(x)/length(x),geom="errorbar",color="black")+
  theme_bw()+
  scale_y_continuous(name="Proportion of Time",breaks=c(0.00,0.04,0.08,0.12,0.16),expand = c(0,0))+
  scale_x_discrete(name="Treatment")+
  theme(plot.margin=margin(20,20,20,20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(text=element_text(size=20,family="Calibri"))+
  coord_cartesian(ylim=c(0,0.14))
fig3



#####Boxplots


my_comparisons<- list(c("conditioned","control"))

figure1<-ggplot(dfs_test_mean,aes(x=field.type,y=prop))+
  stat_boxplot(geom="errorbar",color="black")+
  geom_boxplot(outlier.shape=NA,fill="#CCCCCC",color="black")+
  geom_point(color="black",position="jitter")+
  theme_bw()+scale_y_continuous(name = "Proportion of Time",breaks=c(0.00,0.04,0.08,0.12,0.16,0.2,0.24,0.28,0.32,0.36,0.4))+
  theme(axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20))+
  theme(axis.title.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20))+
  scale_x_discrete(name="Treatment")+theme(text=element_text(size=18,family="Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_compare_means(comparison=my_comparisons,label="p.signif",label.y = 0.4,method="wilcox.test")
figure1




figure2<-ggplot(blue,aes(x=field.type,y=prop))+
  stat_boxplot(geom="errorbar",color="black")+
  geom_boxplot(outlier.shape=NA,fill="#99CCFF",color="black")+
  geom_point(color="black",position="jitter")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.00,0.04,0.08,0.12,0.16,0.2,0.24,0.28,0.32,0.36,0.4,0.44,0.48,0.52,0.56),limits=c(0,0.44))+
  theme(axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20))+
  theme(axis.title.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20))+
  theme(text=element_text(size=18,family="Calibri"),axis.title.x = element_blank(),axis.title.y=element_blank())+
  theme(plot.margin = margin(t=40,r=20,b=10,l=20))+
  ggtitle("Cuba Group")+
  theme(plot.title = element_text(size = 22,hjust=0.5,family = "Calibri",margin = margin(t=0,r=0,b=20,l=0)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_compare_means(comparison=my_comparisons,label="p.signif",label.y = 0.4,method="wilcox.test")
figure2


figure3<-ggplot(orange,aes(x=field.type,y=prop))+
  stat_boxplot(geom="errorbar",color="black")+
  geom_boxplot(outlier.shape=NA,fill="#FF9933",color="black")+
  geom_point(color="black",position="jitter")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.00,0.04,0.08,0.12,0.16,0.2,0.24,0.28,0.32,0.36,0.4,0.44,0.48,0.52,0.56),limits=c(0,0.44))+
  theme(axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20))+
  theme(axis.title.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20))+
  theme(text=element_text(size=18,family="Calibri"),axis.title.x = element_blank(),axis.title.y=element_blank())+
  theme(plot.margin = margin(t=40,r=20,b=10,l=20))+
  ggtitle("New England Group")+
  theme(plot.title = element_text(size = 22,hjust=0.5,family = "Calibri",margin = margin(t=0,r=0,b=20,l=0)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_compare_means(comparison=my_comparisons,label="p.signif",label.y = 0.4,method="wilcox.test")
figure3




library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)
figure4a<-plot_grid(figure2,figure3,rel_widths = c(1,1))+draw_label("A",fontfamily = "Calibri", x = 0, y = 1, hjust = -0.5, vjust = 1.5)+draw_label("B",fontfamily = "Calibri",x=0.5,y=1,hjust=0.5,vjust=1.5)
figure4a
grid.arrange(arrangeGrob(figure4a,top=text_grob(""),
                         bottom = text_grob("Treatment", color = "black",
                                            hjust = 0.5, size = 20,family="Calibri"),
                         left = text_grob("Proportion of Time", color = "black", rot = 90,size=20,family="Calibri"),padding=unit(2,"line"
                         )))



#ggplot(dfs_clean_mean,x=field.type,y=mean.time)+geom_boxplot(aes(x=field.type,y=mean.time))+geom_point(aes(x=field.type,y=mean.time))







