## Conditioning Analyses April

rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/BehavioralObservations_AprTests/")
temp1 = list.files("NewAprAnalyses",pattern="*.csv",full.name=TRUE)
newdata = ldply(temp1, read.csv, header= T)



#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2017/2017_AprTests_Groups.csv",header=T) 



columns <- c("turtle_id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

newdata <- renameFunction(newdata,columns)

columnsold <- c("time","media.file.path","totallength","delete","delete2","delete3","comment","status","start_stop")

#olddata <- renameFunction(olddata,columnsold)



detach(package:plyr)
library(dplyr)

#olddata <- olddata %>% select(c("time","media.file.path","start_stop"))

newdata <- newdata %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))



library(tidyr)

#olddata <- olddata %>% separate(media.file.path, into = c('media.file.path', 'turtle.id'), sep = -14, convert = TRUE)

#olddata <- olddata %>% separate(turtle.id, into = c('turtle.id','delete'),sep=-4,convert=TRUE)

#olddata <- olddata %>% separate(turtle.id, into = c('turtle.id','date'),sep=-5,convert=TRUE)

#olddata <- olddata %>% separate(turtle.id, into = c('turtle.id','delete2'),sep=-1,convert=TRUE)

#olddata <- olddata %>% select(-c("media.file.path","delete","delete2"))

#olddata <- olddata %>% mutate(year=rep(2018))

#olddata$date <- paste(olddata$year,olddata$date,sep = "")

#olddata$date <- as.Date(olddata$date, format = "%Y%b%d")

#olddata_start <- olddata %>% filter(start_stop =="START") %>% group_by(turtle.id,date)
#olddata_stop <- olddata %>% filter(start_stop == "STOP") %>% group_by(turtle.id,date)

#olddata_start <- olddata_start %>% mutate(start = time)
#olddata_stop <- olddata_stop %>% mutate(stop = time)

#olddata <- cbind(olddata_start,olddata_stop)

#olddata <- olddata %>% select(-c("start_stop","start_stop1","turtle.id1","time","time1","date1","year1","year"))

#olddata <- olddata %>% mutate(duration = stop-start)

#olddata <- olddata %>% mutate(observer = rep("KG"))

#names(data)<- c("time","observer.file","id","date","behavior","start_stop")

newdata <- newdata %>% separate(turtle_id, into = c('turtle.id','observer'),sep=-2,convert=TRUE)

newdata <- newdata %>% separate(turtle.id, into = c('turtle.id','date'),sep=-7,convert=TRUE)

newdata <- newdata %>% separate(date, into = c('date','delete1'),sep=-1,convert=TRUE)

newdata <- newdata %>% separate(date, into = c('delete2','date'),sep=1,convert=TRUE)

newdata <- newdata %>% mutate(year=rep(2018))

newdata$date <- paste(newdata$year,newdata$date,sep = "")

newdata$date <- as.Date(newdata$date, format = "%Y%b%d")

newdata <- newdata %>% select(-c("delete1","delete2","year"))

col_order <- c("turtle.id","date","observer","start","stop","duration")

#olddata <- olddata[ , col_order]
newdata <- newdata[ , col_order]

#data <- bind_rows(olddata,newdata)

data <- newdata

data <- data %>% mutate(minutes = start/60)


#groupdata alterations

groupdata <- groupdata %>% separate(time.in,into = c("time.in.hr","time.in.mins"),sep = ":",convert = TRUE) %>% separate(time.change, into = c("time.change.hr","time.change.mins"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.in.mins=time.in.mins/60) %>% mutate(time.change.mins=time.change.mins/60)

groupdata <- groupdata %>% mutate(time.in=time.in.hr+time.in.mins) %>% mutate(time.change=time.change.hr+time.change.mins) %>% mutate(video.change=time.change-time.in)

groupdata <- groupdata %>% mutate(video.change=video.change*60)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","field.type","video.change"))

#combine data 

detach(package:plyr)
data <- merge(data,groupdata,by=c("turtle.id","date"))

data <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% filter(minutes >= video.change) %>% filter(minutes <= video.change+20)

data_obs <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% summarise(total.duration=sum(duration))

apr2017_data <- data_obs %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.duration))

apr2017_data <- apr2017_data %>% mutate(freq = mean.duration/1200)

wilcox.test(freq~field.type,apr2017_data,paired=TRUE)

t.test(freq~field.type,apr2017_data)

red_apr17 <- apr2017_data %>% filter(group=="red")

pur_apr17 <- apr2017_data %>% filter(group=="purple")

wilcox.test(freq~field.type,red_apr17,paired=TRUE)

wilcox.test(freq~field.type,pur_apr17,paired=TRUE)

#plots

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)



annotation_df1 <- data.frame(field.type=rep(c("conditioned","conditioned")),
                             y=c(0.036,0.04))

annotation_df2 <- data.frame(field.type=rep(c("conditioned","control")),
                             y=c(0.04,0.04))

annotation_df3 <- data.frame(field.type=rep(c("control","control")),
                             y=c(0.04,0.036))


plot<-ggplot(apr2017_data,aes(x=field.type,y=freq))+
  stat_summary(fun.y="mean",geom="bar",color="grey50",fill="grey50")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.1))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.04,yend=0.04))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.043),
           label = c("p = 0.04"),
           family = "Calibri", fontface = 3, size=5)
plot


redplot<-ggplot(red_apr17,aes(x=field.type,y=freq))+
  stat_summary(fun.y="mean",geom="bar",color="#990000",fill="#990000")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.1))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.04,yend=0.04))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.043),
           label = c("p = 0.15"),
           family = "Calibri", fontface = 3, size=5)
redplot


#purstats <- pur_apr17 %>% group_by(field.type) %>%  summarise(meanfreq=mean(freq))

purplot<-ggplot(pur_apr17,aes(x=field.type,y=(freq)))+
  stat_summary(fun.y= mean,geom="bar",color="#CC99FF",fill="#CC99FF")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.1))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #ggtitle("Bahamas Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.04,yend=0.04))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.043),
           label = c("p = 0.19"),
           family = "Calibri", fontface = 3, size=5)
purplot
  
  #stat_summary(fun.y= function (x) log(mean(x)),fun.ymin = function(x) 0,geom="bar",color="#CC99FF",fill="#CC99FF")




figure6<-plot_grid(plot,redplot,purplot,rel_widths = c(1,1),rel_heights = c(1,1),ncol=3,nrow=1)+
  draw_label("A",fontfamily = "Calibri Light", x = 0.01, y = 1, hjust = -0.5, vjust = 1.5)+
  draw_label("B",fontfamily = "Calibri Light",x=0.35,y=1,hjust=0.5,vjust=1.5)+
  draw_label("C",fontfamily = "Calibri Light",x=0.68,y=1,hjust=0.5,vjust=1.5)
  
figure6 <- grid.arrange(arrangeGrob(figure6,
                         bottom = text_grob("Treatment", color = "black",
                                            hjust = 0.5, size = 18,family="Calibri",face="bold"),
                         left = text_grob("Proportion of Time", color = "black",
                                          rot=90,size=18,family="Calibri",face="bold"),padding=unit(1,"line")))
figure6

ggsave(figure6, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/april2017_plot.png",  bg = "white")
