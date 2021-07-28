


rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/DataSheets/Jul_Sensitivity/")
temp1 = list.files(pattern="*.csv",full.name=TRUE)
data = ldply(temp1, read.csv, header= T)



#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/DataSheets/TurtleGroupData_Spr2020.csv",header=T)


columns <- c("turtle_id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop","extra")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data <- renameFunction(data,columns)






detach(package:plyr)
library(dplyr)



data <- data %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop","extra"))



library(tidyr)

data <- data %>% separate(turtle_id, into = c('turtle.id','observer'),sep=-2,convert=TRUE)

data <-data %>% separate(turtle.id, into = c('turtle.id','date'),sep=-7,convert=TRUE)

data <- data %>% separate(date, into = c('date','delete1'),sep=-1,convert=TRUE)

data <- data %>% separate(date, into = c('delete2','date'),sep=1,convert=TRUE)

data <- data %>% mutate(year=rep(2020))

data$date <- paste(data$year,data$date,sep = "")

data$date <- as.Date(data$date, format = "%Y%b%d")

data <- data %>% select(-c("delete1","delete2","year"))

col_order <- c("turtle.id","date","observer","start","stop","duration")

data <- data %>% mutate(minutes = start/60)


#groupdata alterations

groupdata <- groupdata %>% separate(time.change, into = c("time.change.mins","time.change.sec"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.change.sec=time.change.sec/60) %>% mutate(video.change = time.change.mins+time.change.sec)

#l194 <- groupdata %>% filter(turtle.id=="L194")
#groupdata <- groupdata %>% filter(turtle.id!="L194")

groupdata <- groupdata %>% mutate(end.trial=video.change+20)
#l194 <- l194  %>% mutate(end.trial=video.change+15)

#groupdata<- rbind(groupdata,l194)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","field.type","experiment","video.change","field.type","end.trial"))

groupdata <- groupdata %>% drop_na()

#combine data 

detach(package:plyr)
data <- merge(data,groupdata,by=c("turtle.id","date"))

data <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% filter(minutes >= video.change) %>% filter(minutes <= end.trial)

data_obs <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% summarise(total.duration=sum(duration))

data_obs<- data_obs %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))

data <- data_obs %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))


data <- data %>% mutate(field.type2=ifelse(field.type=="teal1"|field.type=="maroon1","sens1",
                                           ifelse(field.type=="teal2"|field.type=="maroon2","sens2",
                                                  ifelse(field.type=="teal3"|field.type=="maroon3","sens3",
                                                         ifelse(field.type=="teal4"|field.type=="maroon4","sens4","conditioned")))))

data<- data %>% mutate(freq=mean.duration/1200)

data_kg <- data_obs %>% filter(observer=="KG") %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))

obx_kg <- data_kg  %>% filter(group.total=="teal")

#data<- data %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))

obx <- data %>% filter(group.total=="teal")

nb <- data %>% filter(group.total=="maroon")



attach(obx_kg)
pairwise.wilcox.test(mean.duration,field,data=obx,paired=TRUE)
detach()

attach(obx)
pairwise.wilcox.test(mean.duration,field,data=obx,paired=TRUE)
detach()

attach(nb)
pairwise.wilcox.test(mean.duration,field,data=nb,paired=TRUE)
detach()

attach(data)
pairwise.wilcox.test(freq,field,data=data)

pairwise.wilcox.test(freq,field.type2,data=data,paired=TRUE)
detach()

write.csv(data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/DataSheets/observer_sens_data_4-16-21.csv")


library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)

nb$field.type2 <- factor(nb$field.type2,levels=c("conditioned","sens1","sens2","sens3","sens4"))

obx$field.type2 <- factor(obx$field.type2,levels=c("conditioned","sens1","sens2","sens3","sens4"))


plot_nb<-ggplot(nb,aes(x=field.type2,y=freq))+
  stat_summary(fun.y="mean",geom="bar",fill=c("violetred4","maroon4","deeppink3","violetred2","magenta"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,0.08))+
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
plot_nb

ggsave(plot_nb, width = 10, height=8,units="in",filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/Figures/nb_sens.png",  bg = "transparent")


plot_obx<-ggplot(obx,aes(x=field.type2,y=freq))+
  stat_summary(fun.y="mean",geom="bar",fill= c("sienna4","darkorange3","sienna2","darkorange1","coral1"))+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,.08))+
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
 # theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plot_obx

ggsave(plot_obx, width = 10, height=8,units="in",filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/Figures/obx_sens.png",  bg = "transparent")


plot<-ggplot(data,aes(x=field.type2,y=freq))+
  stat_summary(fun.y="mean",geom="bar",color="grey50",fill="grey50")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,.1))+
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
        panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plot

plot<-ggplot(data,aes(x=field,y=freq))+
  stat_summary(fun.y="mean",geom="bar",color="grey50",fill="grey50")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",expand=c(0,0),limits=c(0,.1))+
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
        panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())
plot



