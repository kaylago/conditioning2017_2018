rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Fall/DataSheets/")
temp1 = list.files(pattern="*.csv",full.name=TRUE)
data = ldply(temp1, read.csv, header= T)



#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Fall/TurtleGroupData_Fall2021.csv",header=T)


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

data <- data %>% mutate(year=rep(2021))

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

groupdata$date <- as.Date(groupdata$date,format= "%m/%d/%Y")

groupdata <- groupdata %>% select(c("turtle.id","date","field","experiment","video.change","end.trial","group","field.type"))

groupdata <- groupdata %>% drop_na()

groupdata <- groupdata %>% mutate(field.type=ifelse(field.type=="condtioned","conditioned",as.character(field.type)))

#combine data

detach(package:plyr)
data <- merge(data,groupdata,by=c("turtle.id","date"))

data1 <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% filter(minutes >= video.change) %>% filter(minutes <= end.trial) #%>% filter(duration>=1)

data1_obs <- data1 %>% group_by(turtle.id,date,observer,field,field.type,group) %>% summarise(total.duration=sum(duration))


data2 <- data %>% group_by(turtle.id,date,observer,field,field.type,group) %>% filter(minutes >= video.change+3) %>% filter(minutes <= end.trial) #%>% filter(duration>=1.5)

data2_obs <- data2 %>% group_by(turtle.id,date,observer,field,field.type,group) %>% summarise(total.duration=sum(duration))
#data_obs<- data_obs %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))



data1f <- data1_obs %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.duration))

data2f <- data2_obs %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.duration))

#data<- data %>% mutate(freq=mean.duration/1200)

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)

data1f_alt <- data1f %>% filter(turtle.id!="L217") #%>% filter(turtle.id!="L223") %>% filter(turtle.id!="L227")

plot<-ggplot(data1f,aes(x=field.type,y=mean.duration))+
  stat_summary(fun="mean",geom="bar",color="turquoise4",fill="turquoise4")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=3)+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Time spent exhibiting food seeking behavior (seconds)",expand=c(0,0),limits=c(0,600))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Magnetic Field Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=12,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=12,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=12,family = "Calibri"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot

ggsave(plot,dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/Averaging2021.tiff")

library(lme4)
library(emmeans)
glm1 <- glmer(round(mean.duration)~field.type+(1|turtle.id),family="poisson", data=data1f)
summary(glm1)
emmeans(glm1, list(pairwise ~ field.type), adjust = "tukey")
#emmeans(glm1, list(pairwise ~ count), adjust = "tukey")
qqnorm(resid(glm1))

attach(data1f_alt)
pairwise.wilcox.test(mean.duration,field.type,data=data1f_alt,paired=F)


