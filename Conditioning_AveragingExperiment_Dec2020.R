rm(list=ls())

library(plyr)
library(readr)

setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Fall/DataSheets/2020_Fall_AvgExp/")
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

library(dplyr)
data <- data %>% mutate(field.type2 = ifelse(field.type=="ht-static"|field.type=="tc-static","conditioned",
                                             ifelse(field.type=="ht-inc"|field.type=="tc-inc","inc-toggle",
                                                    ifelse(field.type=="ht-int"|field.type=="tc-int","int-toggle","tot-toggle"))))


data_kg <- data_obs %>% filter(observer=="KG") %>% group_by(turtle.id,date,field,field.type,group,group.total) %>% summarise(mean.duration=mean(total.duration))

pink <- data %>% filter(group == "pink")
grey <- data %>% filter(group == "grey")

data_og<- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/CompactObserverData_AllYears/2020fall_data_observers_FINAL_12-2-21.csv",header=T)

data_og <- data_og %>% select(-c(X))

data_og <- data_og %>% group_by(turtle.id,date,field,field.type,group) %>% summarise(mean.duration=mean(total.duration)) %>% mutate(field.type2= ifelse(field.type=="conditioned","conditioned-og","control-og"))

data_og<- data_og %>% mutate(freq=mean.duration/1200)

data_og$date <- as.Date(data_og$date,format= "%m/%d/%Y")

data_comb <- rbind(data_og,data)


#data_og <- data_og %>% mutate(mean.duration=total.duration) %>% select(-c(total.duration)) %>% mutate(freq=mean.duration/1200)

#attach(data_kg)
#pairwise.wilcox.test(freq,field.type,data=data)

#pairwise.wilcox.test(freq,field.type,data=data_kg,paired=TRUE)
#detach()

attach(pink)
pairwise.wilcox.test(freq,field.type,data=pink,paired=TRUE)
detach()

attach(data)
pairwise.wilcox.test(freq,field.type2,data=data,paired=TRUE,p.adjust.method = "BH")
detach()

data2 <- data %>% filter(turtle.id!="L202")

#data_comb2 <- data_comb %>% filter(turtle.id!="L202")

attach(data_comb)
pairwise.wilcox.test(freq,field.type2,data=data_comb,paired=TRUE,p.adjust.method = "none")
detach()

#attach(data_comb2)
#pairwise.wilcox.test(freq,field.type2,data=data_comb,paired=TRUE,p.adjust.method = "BH")
#detach()

data_comb_condres<- data_comb %>% filter(field.type2!="control-og")
data_comb_condres <- data_comb_condres %>% mutate(count=rep(1)) 

detach(package:plyr)
data_comb_condres<- data_comb_condres %>% group_by(turtle.id) %>% mutate(timepoint=cumsum(count))



attach(data_comb_condres)
pairwise.wilcox.test(freq,timepoint,data=data_comb_condres,paired=TRUE,p.adjust.method = "BH")
detach()

library(datarium)
library(rstatix)

data_comb_condres$timepoint <- as.factor(data_comb_condres$timepoint)

modelAOV <- aov(freq~(field.type2*timepoint)+Error(factor(turtle.id)), data = data_comb_condres)
print(summary(modelAOV))

res.aov <- anova_test(
  data = data_comb_condres, dv = freq, wid = turtle.id,
  within = c(field.type2, timepoint)
)

one.way <- data_comb_condres %>%
  group_by(timepoint) %>%
  anova_test(dv = freq, wid = turtle.id) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

data %>% group_by(field.type2) %>% summarize(mean=mean(freq))

attach(grey)
pairwise.wilcox.test(freq,field.type,data=grey,paired=TRUE)
detach()

write.csv(data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Fall/DataSheets/2020AveragingExp_data_observers_FINAL_3-2-22.csv")


data2 <- data %>% filter(turtle.id!="L202")
L202 <- data %>% filter(turtle.id=="L202")
L202 <- L202 %>% filter(field.type!="ht-int")
data2 <- rbind(data2,L202)

library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)

plota <- ggplot(data=data,aes(x=date,y=freq))+ geom_point(aes(color=turtle.id),position = position_jitter())
plota

annotation_df1 <- data.frame(field.type=rep(c("conditioned","conditioned")),
                             y=c(0.15,0.17))

annotation_df2 <- data.frame(field.type=rep(c("conditioned","control")),
                             y=c(0.17,0.17))

annotation_df3 <- data.frame(field.type=rep(c("control","control")),
                             y=c(0.15,0.17))


data_comb$field.type2 <- factor(data_comb$field.type2,levels=c("conditioned-og","control-og","conditioned","inc-toggle","int-toggle","tot-toggle") ,labels = c("magnetic field\nwith food original","magnetic field\nwithout food original","magnetic field\nwith food","inclination\ntoggle","intensity\ntoggle","total field\ntoggle"))

plot<-ggplot(data_comb,aes(x=field.type2,y=freq))+
  stat_summary(fun="mean",geom="bar",color="turquoise4",fill="turquoise4")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=3)+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of time exhibiting \nfood seeking behavior",breaks = c(0,0.02,0.08,0.2,0.4,0.6,0.8),expand=c(0,0),limits=c(0,1))+
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
        panel.grid.minor = element_blank())+
  geom_segment(aes(x="magnetic field\nwith food original",xend="magnetic field\nwith food original"),y=.4,yend=.42)+
  geom_segment(aes(x="magnetic field\nwith food original",xend="magnetic field\nwithout food original"),y=0.42,yend=0.42)+
  geom_segment(aes(x="magnetic field\nwithout food original",xend="magnetic field\nwithout food original"),y=.42,yend=.4)+
  annotate("text",
           x = c(1.5),
           y = c(0.48),
           label = c("p = 0.003"),
           family = "Calibri", fontface = 3, size=4)+
  geom_segment(aes(x="magnetic field\nwith food",xend="magnetic field\nwith food"),y=.6,yend=.62)+
  geom_segment(aes(x="magnetic field\nwith food",xend="magnetic field\nwithout food original"),y=0.62,yend=0.62)+
  geom_segment(aes(x="magnetic field\nwithout food original",xend="magnetic field\nwithout food original"),y=.62,yend=.6)+
  annotate("text",
           x = c(2.5),
           y = c(0.69),
           label = c("p = 0.04"),
           family = "Calibri", fontface = 3, size=4)
  
plot

ggsave(plot, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/toggle_experiments_5-20-22.tiff",  bg = "transparent")


plot<-ggplot(data_og,aes(x=field.type,y=freq))+
  stat_summary(fun="mean",geom="bar",color="olivedrab3",fill="olivedrab3")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=3)+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of time exhibiting \nfood seeking behavior",breaks = c(0,0.02,0.08,0.2,0.4,0.6,0.8),expand=c(0,0),limits=c(0,1))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Magnetic Field Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=12,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=12,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=12,family = "Calibri"),
        axis.text.x = element_text(angle=45,hjust = 1,size=12),
        axis.text.y = element_text(size=12))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_segment(aes(x="conditioned",xend="conditioned"),y=.4,yend=.42)+
  geom_segment(aes(x="conditioned",xend="control"),y=0.42,yend=0.42)+
  geom_segment(aes(x="control",xend="control"),y=.42,yend=.4)+
  annotate("text",
           x = c(1.5),
           y = c(0.48),
           label = c("p = 0.003"),
           family = "Calibri", fontface = 3, size=4)

plot

data_roc <- data_comb %>% filter(field.type2=="conditioned")

data_roc2 <- data_comb %>% filter(field.type2=="tot-toggle")

data_roc3 <- rbind(data_roc,data_roc2)

data_roc3 <- data_roc3 %>% mutate(field.type3=ifelse(field.type2=="tot-toggle","fluctuating",as.character(field.type2)))

plot<-ggplot(data_roc3,aes(x=field.type3,y=freq))+
  stat_summary(fun="mean",geom="bar",color="darkolivegreen",fill="darkolivegreen")+
  stat_summary(fun=mean,fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)),fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom="errorbar",color="black")+
  geom_point(position=position_jitter(width=0.15),size=3)+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of time exhibiting \nfood seeking behavior",breaks = c(0,0.02,0.08,0.2,0.4,0.6,0.8),expand=c(0,0),limits=c(0,1))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Magnetic Field Treatment")+
  #labs(title="Canada Group") +
  theme(text=element_text(size=12,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=12,face = "plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=12,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=12,family = "Calibri"),
        axis.text.x = element_text(angle=45,hjust = 1,size=12),
        axis.text.y = element_text(size=12))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "transparent"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot

