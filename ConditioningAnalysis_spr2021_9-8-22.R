rm(list=ls())

library(plyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Spring/DataSheets/AllExperiments/")
temp1 = list.files(pattern="*.csv",full.name=TRUE)
data = ldply(temp1, read.csv, header= T)

#temp2 = list.files(pattern="*.CSV",full.name=TRUE)
#data2 = ldply(temp2, read.csv, header= T)

#setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2017/BehavioralObservations_AprTests/OldAprAnalyses/OldAprAnalyses/")
#temp2 = list.files("OldAprAnalyses",pattern="*.csv",full.name=TRUE)
#olddata = ldply(temp2, read.csv,skip=15, header= T)


groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Spring/DataSheets/TurtleGroupData_Sp2021.csv",header=T)


columns <- c("turtle.id","obs_date","description","media.file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data <- renameFunction(data,columns)


detach(package:plyr)
library(dplyr)



data <- data %>% select(-c("obs_date","media.file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop","description"))



library(tidyr)

data <- data %>% separate(turtle.id, into = c('turtle.id','observer'),sep=5,convert=TRUE)

data <-data %>% separate(turtle.id, into = c('turtle.id','delete1'),sep="_",convert=TRUE)

data <- data %>% separate(observer, into = c('date','observer'),sep="_",convert=TRUE)

#data <- data %>% separate(date, into = c('delete2','date'),sep=1,convert=TRUE)

data <- data %>% mutate(year=rep(2021))

data$date <- paste(data$year,data$date,sep = "")

data$date <- as.Date(data$date, format = "%Y%b%d")

data <- data %>% select(-c("delete1","year"))

col_order <- c("turtle.id","date","observer","start","stop","duration")

data <- data %>% mutate(minutes = start/60)

#data <- data %>% filter(duration>=1)
#groupdata alterations

groupdata <- groupdata %>% separate(time.change, into = c("time.change","extra"),sep=5,convert = TRUE)

groupdata <- groupdata %>% separate(time.change, into = c("time.change.mins","time.change.sec"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.change.sec=time.change.sec/60) %>% mutate(video.change = time.change.mins+time.change.sec)


groupdata <- groupdata %>% mutate(end.trial=video.change+20)
#l194 <- l194  %>% mutate(end.trial=video.change+15)

#groupdata<- rbind(groupdata,l194)

groupdata$date <- as.Date(groupdata$date,format= "%m/%d/%Y")

groupdata <- groupdata %>% dplyr::select(c("turtle.id","date","group","field","field.type","experiment","video.change","field.type","end.trial"))

#groupdata <- groupdata %>% drop_na()

#combine data

#dataskg <- data %>% filter(observer=="SKG")

#detach(package:plyr)
data <- merge(data,groupdata,by=c("turtle.id","date"))

data <- data %>% group_by(turtle.id,date,observer,field,experiment) %>% filter(minutes >= video.change) %>% filter(minutes <= end.trial)

#write.csv(data,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Post-Hoc Analyses/data_all_byobs_2020Fall.csv")

data_obs <- data %>% group_by(turtle.id,date,observer,field,experiment) %>% summarise(total.duration=sum(duration))

data_obs_og <- data_obs %>% filter(experiment=="OG")
#data_obs<- data_obs %>% mutate(group.total=ifelse(group=="teal" |group=="grey","teal","maroon"))

data_obs_ns <- data_obs %>% filter(experiment=="NONSENSE")

data_obs_sens <- data_obs %>% filter(experiment=="SENSITIVITY")

data_og <- data_obs_og %>% group_by(turtle.id,date,field,experiment) %>% summarise(mean.duration=mean(total.duration))

data <- data_obs %>% group_by(turtle.id,date,field,experiment) %>% summarise(mean.duration=mean(total.duration))

data_og <- data_obs_og %>% group_by(turtle.id,date,field,experiment) %>% summarise(mean.duration=mean(total.duration))

data_og<- data_og %>% mutate(freq=mean.duration/1200)

data_ns <- data_obs_ns %>% group_by(turtle.id,date,field,experiment) %>% summarise(mean.duration=mean(total.duration))


attach(data_og)
pairwise.wilcox.test(mean.duration,field,data=data_og,paired=T,p.adjust.method = "none")
detach()
attach(data_ns)
pairwise.wilcox.test(mean.duration,field,data=data_ns,paired=T,p.adjust.method = "none")
detach()

data_ns <- data_ns %>% group_by(turtle.id) %>% mutate(count=row_number())
data_ns <- data_ns %>% mutate(field.type=ifelse(field=="OBX","conditioned",ifelse(field=="NB","control","mismatch")))

data_ns2 <- data_ns %>% filter(turtle.id!="L216") #%>% filter(turtle.id!="L214")

data_ns3 <- data_ns %>% filter(field!="NB") %>% filter(turtle.id!="L216")

write.csv(data_obs_ns,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2021_Spring/nonsense_exp_sp2021_11-9-21.csv")

attach(data_ns3)
pairwise.wilcox.test(mean.duration,field,data=data_ns3,paired=T,p.adjust.method = "none")
detach()

library(nlme)
lm1 <- lme(mean.duration~field.type*count,random=~1|turtle.id, data=data_ns)
summary(lm1)
anova(lm1)
library(emmeans)
emmeans(lm1, list(pairwise ~ field.type), adjust = "tukey")

library(lme4)
glm1 <- glmer(round(mean.duration)~field+(1|turtle.id),family="poisson", data=data_ns2)
summary(glm1)
emmeans(glm1, list(pairwise ~ field), adjust = "tukey")
#emmeans(glm1, list(pairwise ~ count), adjust = "tukey")
qqnorm(resid(glm1))


data_og2 <- data_og %>% filter(turtle.id!="L213")

plot<-ggplot(data_og,aes(x=field,y=mean.duration))+
  stat_summary(fun="mean",geom="bar",color="cornflowerblue",fill="cornflowerblue")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=3)+
  theme_bw()+
  coord_trans(y="sqrt")+

  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  #scale_y_continuous("Proportion of Time \nExhibiting Food Seeking Behavior",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.5))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Magnetic Field Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"),
        axis.text.x = element_text(angle=45,hjust = 1,size=16),
        axis.text.y = element_text(size=16))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  #theme(axis.title.x = element_text("Magnetic Field Treatment"),axis.title.y=element_text())+
  #labs(y="Proportion of Time \nExhibiting Food Seeking Behavior")+
  #geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  #geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.17,yend=0.17))+
  #geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  #annotate("text",
         #  x = c(1.5),
          # y = c(0.19),
           #label = c("p = 0.003"),
           #family = "Calibri", fontface = 3, size=5)
plot


plot<-ggplot(data_ns,aes(x=field,y=mean.duration))+
  stat_summary(fun="mean",geom="bar",color="cornflowerblue",fill="cornflowerblue")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=3)+
  theme_bw()+
  coord_trans(y="sqrt")+
  
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  #scale_y_continuous("Proportion of Time \nExhibiting Food Seeking Behavior",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.5))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Magnetic Field Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"),
        axis.text.x = element_text(angle=45,hjust = 1,size=16),
        axis.text.y = element_text(size=16))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#theme(axis.title.x = element_text("Magnetic Field Treatment"),axis.title.y=element_text())+
#labs(y="Proportion of Time \nExhibiting Food Seeking Behavior")+
#geom_line(data=annotation_df1,aes(x=field.type,y=y))+
#geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.17,yend=0.17))+
#geom_line(data=annotation_df3,aes(x=field.type,y=y))+
#annotate("text",
#  x = c(1.5),
# y = c(0.19),
#label = c("p = 0.003"),
#family = "Calibri", fontface = 3, size=5)
plot

plotb<-ggplot(data_ns2,aes(x=count,y=mean.duration,color=field.type))+
 # Magnetic Field Treatmentstat_summary(fun="mean",geom="bar",color="cornflowerblue",fill="cornflowerblue")+
  #stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
              # geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=5)+
  theme_bw()+
  #coord_trans(y="sqrt")+
  
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  #scale_y_continuous("Proportion of Time \nExhibiting Food Seeking Behavior",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.5))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_continuous("Day of Trial")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"),
        axis.text.x = element_text(angle=45,hjust = 1,size=16),
        axis.text.y = element_text(size=16))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plotb

