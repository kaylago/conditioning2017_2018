


rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Spring/DataSheets/AllExperiments/")
temp1 = list.files(pattern="*.csv",full.name=TRUE)
data = ldply(temp1, read.csv, header= T)



#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Spring/DataSheets/TurtleGroupData_Sp2021.csv",header=T)


columns <- c("turtle_id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","state","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data <- renameFunction(data,columns)






detach(package:plyr)
library(dplyr)



data <- data %>% dplyr::select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","state","comment_start","comment_stop"))



library(tidyr)

#data <- data %>% separate(turtle_id, into = c('turtle.id','observer'),sep=-3,convert=TRUE)

data <-data %>% separate(turtle_id, into = c('turtle.id','date'),sep=4,convert=TRUE)

data <- data %>% separate(date, into = c('date','observer'),sep=6,convert=TRUE)

data <- data %>% separate(date, into = c('del1','date'),sep="_",convert=TRUE)


data$observer <- ifelse(!grepl("_", data$observer), paste0("_", data$observer), as.character(data$observer))

data <- data %>% separate(observer, into = c('delete2','observer'),sep="_",convert=TRUE)

data <- data %>% mutate(year=rep(2021))

data$date <- paste(data$year,data$date,sep = "")

data$date <- as.Date(data$date, format = "%Y%b%d")

data <- data %>% dplyr::select(-c("del1","delete2","year"))

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

groupdata$date <- as.Date(groupdata$date,format= "%m/%d/%Y")

groupdata <- groupdata %>% mutate(field.type=ifelse(field=="OBX","conditioned",
                                                    ifelse(field=="NB","control",
                                                           ifelse(field=="MD","sens1",
                                                                  ifelse(field=="NJ","sens2",
                                                                         ifelse(field=="NH","sens3",
                                                                                ifelse(field=="NS","sens4","nonsense")))))))
groupdata$group <- rep("teal")

groupdata <- groupdata %>% dplyr::select(c("turtle.id","date","group","field","field.type","experiment","video.change","field.type","end.trial"))

groupdata <- groupdata %>% drop_na()

groupdata$turtle.id <- as.character(groupdata$turtle.id)
data$turtle.id <- as.character(data$turtle.id)

#combine data 

detach(package:plyr)
data <- merge(data,groupdata,by=c("turtle.id","date"),all.x=T,all.y=T)

data <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% filter(minutes >= video.change) %>% filter(minutes <= end.trial)

data_obs <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% summarise(total.duration=sum(duration))

data_obs<- data_obs %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))

data <- data_obs %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))


data<- data %>% mutate(freq=mean.duration/1200)

#data_kg <- data_obs %>% filter(observer=="KG") %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))

#obx_kg <- data_kg  %>% filter(group.total=="teal")

#data<- data %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))

#obx <- data %>% filter(group.total=="teal")

#nb <- data %>% filter(group.total=="maroon")


groupdata <- groupdata %>% arrange(turtle.id,date)

attach(data)
pairwise.wilcox.test(freq,field,data=data2,p.adjust.method = "none")

pairwise.wilcox.test(freq,field.type2,data=data2,paired=TRUE,p.adjust.method = "BH")
pairwise.wilcox.test(freq,field.type2,data=data2,paired=TRUE,p.adjust.method = "none")
detach()

write.csv(data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Spring/DataSheets/observer_sens_data_5-16-22.csv")


library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)

nb$field.type2 <- factor(nb$field.type2,levels=c("conditioned","sens1","sens2","sens3","sens4"))

nb$field <- factor(nb$field,levels=c("NB","PEI","NOSCO","NH","NJ"),labels=c("New Brunswick","Prince Edward\nIsland","Nova Scotia","New Hampshire"," New Jersey"))


obx$field.type2 <- factor(obx$field.type2,levels=c("conditioned","sens1","sens2","sens3","sens4"))

obx$field <- factor(obx$field,levels=c("OBX","MD","NJ","NH","NOSCO"),labels=c("North Carolina","Maryland","New Jersey","New Hampshire","Nova Scotia"))

plot_nb<-ggplot(nb,aes(x=field,y=freq))+
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

ggsave(plot_nb, width = 10, height=8,units="in",filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/nb_sens_5-10-22.tiff",  bg = "transparent")


plot_obx<-ggplot(obx,aes(x=field,y=freq))+
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

ggsave(plot_obx, width = 10, height=8,units="in",filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/obx_sens_5-10-22.tiff",  bg = "transparent")


plot<-ggplot(data,aes(x=field.type2,y=freq))+
  stat_summary(fun.y="mean",geom="bar",color="seagreen3",fill="seagreen3")+
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
plot

ggsave(plot, width = 10, height=8,units="in",filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/allsens_5-9-22.tiff",  bg = "transparent")


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



