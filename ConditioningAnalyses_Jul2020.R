#July Experiments - Same direction conditionig fields

rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/DataSheets/Jul_2mo/")
temp = list.files(pattern="*.csv")
data = lapply(temp, read.csv,header= T)

groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/DataSheets/TurtleGroupData_Spr2020.csv",header=T)



library(dplyr)
data = data[order(sapply(data,ncol),decreasing = F)]


data_15 <- data[c(0:50)]
data_16 <- data[c(51:62)]



detach(package:dplyr)
library(plyr)

columns_15<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

columns_16<- c("turtle.id","obs_date","descrip","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

#columns_14<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","modifiers","type","start","stop","duration","comment_start","comment_stop")

#columns_5 <- c("1","2","3","4","5")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data_15 = ldply(data_15,renameFunction,columns_15)

data_16 = ldply(data_16,renameFunction,columns_16)

#nov_data_5 = ldply(nov_data_5,renameFunction,columns_5)

detach(package:plyr)
library(dplyr)
data_15 <- data_15 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))

data_16 <- data_16 %>% select(-c("obs_date","descrip","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))
#nov_data_14 <- nov_data_14 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))


data <- rbind(data_15,data_16)



library(tidyr)
library(dplyr)

data <- data %>% separate(turtle.id, into = c('turtle.id','date'), sep = 4, convert=TRUE)
data <- data %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
data <- data %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
data <- data %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
data <- data %>% select(-c("delete","delete2"))

#nov_data <- nov_data %>% separate(observer, into = c('experiment','observer'), sep=2,convert=TRUE)
#nov_data <- nov_data %>% separate(observer, into = c('delete','observer'),sep=-2,convert=TRUE)
#nov_data <- nov_data %>% select(-c("delete"))




data <- data %>% mutate(minutes=start/60)

data<- data %>% mutate(minutesend=stop/60)

col_order <- c("turtle.id","date","observer","minutes","minutesend","start","stop","duration")

#js_data <- js_data[ , col_order]
data <- data[ , col_order]

#october_data <- bind_rows(oct_data,js_data)

#rf_data <- rf_data %>% filter(minutes != "NA")

data <- data %>% mutate(year=rep(2020))

data$date <- paste(data$date,data$year,sep = "")

data$date <- as.Date(data$date, format = "%b%d%Y")

#write.csv(js,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/js_october_data.csv")


#groupdata alterations

groupdata <- groupdata %>% separate(time.change, into = c("time.change.mins","time.change.sec"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.change.sec=time.change.sec/60) %>% mutate(video.change = time.change.mins+time.change.sec)

groupdata <- groupdata %>% mutate(end.trial=video.change+20)


groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","experiment","video.change","field.type","end.trial"))

groupdata <- groupdata %>% drop_na()

#combine data sheets

detach(package:plyr)
finaldata <- merge(data,groupdata,by=c("turtle.id","date"))

finaldata <- finaldata %>% group_by(turtle.id,date,observer,field,group,field.type) %>% filter(minutes >= video.change) %>% filter(minutes <= end.trial)

data_obs <- finaldata %>% group_by(turtle.id,date,observer,field,group,field.type) %>% summarise(total.duration=sum(duration))

data_obs <- data_obs %>% arrange(turtle.id,date)

write.csv(data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/DataSheets/Spring2020_observerdata_2-4-2021.csv")

finaldata <- data_obs %>% group_by(turtle.id,date,field,group,field.type) %>% summarise(mean.duration=mean(total.duration))

finaldata <- finaldata %>% arrange(turtle.id,date)

finaldata <- finaldata %>% mutate(freq=mean.duration/1200)

observer_difference <- data_obs %>% group_by(turtle.id,date) %>% summarize(difference=max(total.duration)-min(total.duration))

mean(observer_difference$difference) #18.03

wilcox.test(freq~field.type,data=finaldata)

t.test(freq~field.type,data=finaldata)

finaldata2 <- finaldata %>% filter(turtle.id != "L188") %>% filter(turtle.id !="L197")

obx <- finaldata %>% filter(group!="orange") %>% filter(group!="purple")

nb <- finaldata %>% filter(group!="grey") %>% filter(group!="teal")

nb2 <- nb %>% filter (turtle.id != "L192") %>% filter(turtle.id != "L188")

obx2 <- obx %>% filter(turtle.id != "L197")

wilcox.test(freq~field.type,data=nb,paired=TRUE)

wilcox.test(freq~field.type,data=obx)



t.test(freq~field.type,data=nb)

t.test(freq~field.type, data=obx)

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


jul_plot<-ggplot(finaldata,aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="grey",fill="grey")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16,0.32),expand=c(0,0),limits=c(0,0.32))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  ggtitle("") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  #theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="conditioned",xend="control",y=0.22,yend=0.22))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.25),
           label = c("p = 0.0004"),
           family = "Calibri", fontface = 3, size=5)+
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
jul_plot

ggsave(jul_plot, width = 5, height=7,units="in",filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2020_Spring/Figures/jul_plot.png",  bg = "transparent")


obx_plot_20 <-ggplot(obx,aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="aquamarine4",fill="aquamarine4")+
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
           label = c("p = 0.015"),
           family = "Calibri", fontface = 3, size=5)
obx_plot_20


nb_plot_20 <-ggplot(nb,aes(x=field.type,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="lightpink4",fill="lightpink4")+
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
           label = c("p = 0.039"),
           family = "Calibri", fontface = 3, size=5)
nb_plot_20





citation(package="emmeans")
