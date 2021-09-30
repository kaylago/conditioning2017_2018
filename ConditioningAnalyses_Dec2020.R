

rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Fall/DataSheets/2020_Fall_2mo/")
temp1 = list.files(pattern="*.csv",full.name=TRUE)
data = ldply(temp1, read.csv, header= T)



#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Fall/DataSheets/TurtleGroupData_Fall2020.csv",header=T)


columns <- c("turtle_id","obs_date","description","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data <- renameFunction(data,columns)


detach(package:plyr)
library(dplyr)



data <- data %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop","description"))



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

data_obs<- data_obs %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))

data <- data_obs %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))



data<- data %>% mutate(freq=mean.duration/1200)

data_kg <- data_obs %>% filter(observer=="KG") %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))

pink <- data %>% filter(group == "pink")
grey <- data %>% filter(group == "grey")

#attach(data_kg)
#pairwise.wilcox.test(freq,field.type,data=data)

#pairwise.wilcox.test(freq,field.type,data=data_kg,paired=TRUE)
#detach()

attach(pink)
pairwise.wilcox.test(freq,field.type,data=pink,paired=TRUE)
detach()

attach(data)
pairwise.wilcox.test(freq,field.type,data=data,paired=TRUE)
detach()


attach(grey)
pairwise.wilcox.test(freq,field.type,data=grey,paired=TRUE)
detach()

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)



annotation_df1 <- data.frame(field.type=rep(c("conditioned","conditioned")),
                             y=c(0.15,0.17))

annotation_df2 <- data.frame(field.type=rep(c("conditioned","control")),
                             y=c(0.17,0.17))

annotation_df3 <- data.frame(field.type=rep(c("control","control")),
                             y=c(0.15,0.17))


plot<-ggplot(data,aes(x=field.type,y=freq))+
  stat_summary(fun="mean",geom="bar",color="grey50",fill="grey50")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.5))+
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
  theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.17,yend=0.17))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.19),
           label = c("p = 0.004"),
           family = "Calibri", fontface = 3, size=5)
plot

ggsave(plot, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/fall2020_plot.png",  bg = "white")
