
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


#data$turtle.id <- paste(data$turtle.id,data$delete1,sep="_")

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

data <- data %>% mutate(field.type2=ifelse(field.type=="ht1","-1",
                                           ifelse(field.type=="ht2","-2",
                                                  ifelse(field.type=="ht3","+1",
                                                         ifelse(field.type=="ht4","+2",as.character(field.type))))))


data <- data %>% mutate(field.type2=ifelse(field.type=="tc1","-1",
                                           ifelse(field.type=="tc2","-2",
                                                  ifelse(field.type=="tc3","+1",
                                                         ifelse(field.type=="tc4","+2",as.character(field.type2))))))


data <- data %>% mutate(field.type3=ifelse(field.type2=="-1","1deg",
                                           ifelse(field.type2=="+1","1deg",
                                                  ifelse(field.type2=="-2","2deg",
                                                         ifelse(field.type2=="+2","2deg",
                                                                "conditioned")))))
data <- data %>% mutate(field.type4=ifelse(field.type2=="ht","conditioned",
                                           ifelse(field.type2=="tc","conditioned",as.character(field.type2))))


attach(data)
pairwise.wilcox.test(freq,field.type3,data=data,paired=T,p.adjust.method = "BH")
pairwise.wilcox.test(freq,field.type4,data=data,p.adjust.method = "BH")
detach()

library(lme4)
library(nlme)

lm <- lmer(freq~field.type4+(1|turtle.id),data=data)
anova(lm)
summary(lm)

lm <- lmer(round(freq)~field.type4+(1|turtle.id),family="poisson",data=data)
anova(lm)
summary(lm)

#ORIGINAL EXP
og_data <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/CompactObserverData_AllYears/2020fall_data_observers_FINAL_12-2-21.csv")

og_data <- og_data %>% select(-c(X))

og_data <- og_data %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.duration))

og_data <- og_data %>% mutate(exper=rep("OG"))

og_data <- og_data %>% mutate(freq=mean.duration/1200)

og_data$date <- as.Date(og_data$date,format= "%Y-%m-%d")

og_data <- og_data %>% mutate(field.type2=ifelse(field.type=="conditioned","magnetic field\nwith food","magnetic field\nwithout food"))

og_data <- og_data %>% mutate(field.type3=as.character(field.type2)) %>% mutate(field.type4=as.character(field.type2)) 

data <- data %>% mutate(exper=rep("INCSENS"))

all_data <- rbind(og_data,data)

wilcox.test(freq~field.type,og_data,paired=TRUE)

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)


plot<-ggplot(data,aes(x=field.type,y=freq))+
  stat_summary(fun.y="mean",geom="bar")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,0.3))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"),
        axis.text.x = element_text(angle=45,hjust = 1))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plot


data$field.type2 <- factor(data$field.type2,levels=c("ht","tc","-1","-2","+1","+2"),labels=c("Haiti","Turks & Caicos","-1 deg","-2 deg","+1 deg","+2 deg"))

plotHT<-ggplot(subset(data,field=="HT"),aes(x=field.type2,y=freq))+
  stat_summary(fun.y="mean",geom="bar",fill="grey56")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,0.5))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=12,family="Helvetica"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Helvetica",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=12,family = "Helvetica"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=12,family = "Helvetica"),
        axis.text.x = element_text(angle=45,hjust = 1))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plotHT

ggsave(plotHT, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/2020_HTIncSens_5-10-22.tiff",  bg = "transparent")



plotTC<-ggplot(subset(data,field=="TC"),aes(x=field.type2,y=freq))+
  stat_summary(fun.y="mean",geom="bar",fill="rosybrown")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,0.5))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=12,family="Helvetica"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Helvetica",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=12,family = "Helvetica"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=12,family = "Helvetica"),
        axis.text.x = element_text(angle=45,hjust = 1))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plotTC

ggsave(plotTC, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/2020_TCIncSens_5-10-22.tiff",  bg = "transparent")

all_data$field.type3 <- factor(all_data$field.type3,levels=c("magnetic field\nwith food","magnetic field\nwithout food","conditioned","1deg","2deg"),labels=c("initial experiments\nrewarded\nmagnetic field","initial experiments\nunrewarded\nmagnetic field","rewarded\nmagnetic field","1 degree\ndifference","2 degree\ndifference"))

plot<-ggplot(all_data,aes(x=field.type3,y=mean.duration))+
  stat_summary(fun.y="mean",geom="bar",fill=c("maroon","maroon","coral","coral","coral"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Time spent exhibiting the turtle dance (seconds)",expand=c(0,0),limits=c(0,350),breaks=c(0.1,100,200,300))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=16,family="Helvetica"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Helvetica",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=16,family = "Helvetica"),
        axis.title.x = element_blank())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_segment(aes(x="rewarded\nmagnetic field",xend="rewarded\nmagnetic field"),y=200,yend=220)+
  geom_segment(aes(x="rewarded\nmagnetic field",xend="1 degree\ndifference"),y=220,yend=220)+
  geom_segment(aes(x="1 degree\ndifference",xend="1 degree\ndifference"),y=200,yend=220)+
  annotate("text",
           x = c(3.5),
           y = c(240),
           label = c("p = 0.02"),
           family = "Helvetica", fontface = 1, size=4)+
  geom_segment(aes(x="rewarded\nmagnetic field",xend="rewarded\nmagnetic field"),y=270,yend=290)+
  geom_segment(aes(x="rewarded\nmagnetic field",xend="2 degree\ndifference"),y=290,yend=290)+
  geom_segment(aes(x="2 degree\ndifference",xend="2 degree\ndifference"),y=290,yend=270)+
  annotate("text",
           x = c(4),
           y = c(310),
           label = c("p = 0.02"),
           family = "Helvetica", fontface = 1, size=4)+
  geom_segment(aes(x="initial experiments\nrewarded\nmagnetic field",xend="initial experiments\nrewarded\nmagnetic field"),y=270,yend=290)+
  geom_segment(aes(x="initial experiments\nrewarded\nmagnetic field",xend="initial experiments\nunrewarded\nmagnetic field"),y=290,yend=290)+
  geom_segment(aes(x="initial experiments\nunrewarded\nmagnetic field",xend="initial experiments\nunrewarded\nmagnetic field"),y=290,yend=270)+
  annotate("text",
           x = c(1.5),
           y = c(310),
           label = c("p = 0.003"),
           family = "Helvetica", fontface = 1, size=4)
#theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plot

ggsave(plot, dpi=300,width=9,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/2020_IncSens_12-13-22.tiff",  bg = "transparent")

plot<-ggplot(all_data,aes(x=field.type3,y=mean.duration))+
  stat_summary(fun.y="mean",geom="bar",fill=c("maroon","maroon","coral","coral","coral"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Time spent exhibiting\nfood anticipatory behavior (seconds)",expand=c(0,0),limits=c(0,350))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=12,family="Helvetica"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Helvetica",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=12,family = "Helvetica"),
        axis.title.x = element_blank())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_segment(aes(x="magnetic field\nwith food",xend="magnetic field\nwith food"),y=200,yend=220)+
  geom_segment(aes(x="magnetic field\nwith food",xend="1 degree\ndifference"),y=220,yend=220)+
  geom_segment(aes(x="1 degree\ndifference",xend="1 degree\ndifference"),y=200,yend=220)+
  annotate("text",
           x = c(3.5),
           y = c(230),
           label = c("p = 0.02"),
           family = "Calibri", fontface = 3, size=4)+
  geom_segment(aes(x="magnetic field\nwith food",xend="magnetic field\nwith food"),y=270,yend=290)+
  geom_segment(aes(x="magnetic field\nwith food",xend="2 degree\ndifference"),y=290,yend=290)+
  geom_segment(aes(x="2 degree\ndifference",xend="2 degree\ndifference"),y=290,yend=270)+
  annotate("text",
           x = c(4),
           y = c(300),
           label = c("p = 0.02"),
           family = "Helvetica", fontface = 3, size=4)+
  geom_segment(aes(x="initial experiments\nmagnetic field\nwith food",xend="initial experiments\nmagnetic field\nwith food"),y=270,yend=290)+
  geom_segment(aes(x="initial experiments\nmagnetic field\nwith food",xend="initial experiments\nmagnetic field\nwithout food"),y=290,yend=290)+
  geom_segment(aes(x="initial experiments\nmagnetic field\nwithout food",xend="initial experiments\nmagnetic field\nwithout food"),y=290,yend=270)+
  annotate("text",
           x = c(1.5),
           y = c(300),
           label = c("p = 0.003"),
           family = "Helvetica", fontface = 3, size=4)
#theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plot


