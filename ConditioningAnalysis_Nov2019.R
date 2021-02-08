##November 2 month tests

rm(list=ls())


setwd("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/November2019_2mo/")
temp = list.files(pattern="*.csv")
nov_data = lapply(temp, read.csv,header= T)

groupdata <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/TurtleGroups_Fa2019.csv",header=T)



library(dplyr)
nov_data = nov_data[order(sapply(nov_data,ncol),decreasing = F)]

nov_data_15 <- nov_data[c(0:60)]
nov_data_16 <- nov_data[c(61:67)]
#nov_data_14 <- nov_data[c(0:1)]
#nov_data_5 <- nov_data[c(0:1)]


detach(package:dplyr)
library(plyr)

columns_15<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

columns_16<- c("turtle.id","obs_date","description","media_file","length","fps","subject","behavior","delete","modifiers","type","start","stop","duration","comment_start","comment_stop")

#columns_14<- c("turtle.id","obs_date","media_file","length","fps","subject","behavior","modifiers","type","start","stop","duration","comment_start","comment_stop")

#columns_5 <- c("1","2","3","4","5")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

nov_data_15 = ldply(nov_data_15,renameFunction,columns_15)
nov_data_16 = ldply(nov_data_16,renameFunction,columns_16)



#nov_data_14 = ldply(nov_data_14,renameFunction,columns_14)

#nov_data_5 = ldply(nov_data_5,renameFunction,columns_5)

detach(package:plyr)
library(dplyr)
nov_data_15 <- nov_data_15 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","delete","modifiers","type","comment_start","comment_stop"))

nov_data_16 <- nov_data_16 %>% select(-c("delete","obs_date","description","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))

#nov_data_14 <- nov_data_14 %>% select(-c("obs_date","media_file","length","fps","subject","behavior","modifiers","type","comment_start","comment_stop"))


nov_data <- rbind(nov_data_15,nov_data_16)





library(tidyr)
library(dplyr)

nov_data <- nov_data %>% separate(turtle.id, into = c('turtle.id','date'), sep = 4, convert=TRUE)
nov_data <- nov_data %>% separate(date, into = c('delete','date'),sep=1,convert=TRUE)
nov_data <- nov_data %>% separate(date,into = c('date','observer'),sep=5,convert=TRUE)
nov_data <- nov_data %>% separate(observer,into = c('delete2','observer'),sep=1,convert = TRUE)
nov_data <- nov_data %>% select(-c("delete","delete2"))

#nov_data <- nov_data %>% separate(observer, into = c('experiment','observer'), sep=2,convert=TRUE)
#nov_data <- nov_data %>% separate(observer, into = c('delete','observer'),sep=-2,convert=TRUE)
#nov_data <- nov_data %>% select(-c("delete"))




nov_data <- nov_data %>% mutate(minutes=start/60)

col_order <- c("turtle.id","date","observer","minutes","start","stop","duration")

#js_data <- js_data[ , col_order]
nov_data <- nov_data[ , col_order]

#october_data <- bind_rows(oct_data,js_data)

#rf_data <- rf_data %>% filter(minutes != "NA")

nov_data <- nov_data %>% mutate(year=rep(2019))

nov_data$date <- paste(nov_data$date,nov_data$year,sep = "")

nov_data$date <- as.Date(nov_data$date, format = "%b%d%Y")

#write.csv(js,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/js_october_data.csv")


#groupdata alterations

groupdata <- groupdata %>% separate(time.in,into = c("time.in.hr","time.in.mins"),sep = ":",convert = TRUE) %>% separate(time.change, into = c("time.change.hr","time.change.mins"),sep=":",convert = TRUE)

groupdata <- groupdata %>% mutate(time.in.mins=time.in.mins/60) %>% mutate(time.change.mins=time.change.mins/60)

groupdata <- groupdata %>% mutate(time.in=time.in.hr+time.in.mins) %>% mutate(time.change=time.change.hr+time.change.mins) %>% mutate(video.change=time.change-time.in)

groupdata <- groupdata %>% mutate(video.change=video.change*60)

groupdata$date <- as.Date(groupdata$date,format= "%d-%b-%y")

groupdata <- groupdata %>% select(c("turtle.id","date","group","field","experiment","video.change"))


#combine data sheets

detach(package:plyr)
nov_data <- merge(nov_data,groupdata,by=c("turtle.id","date"))

nov_data <- nov_data %>% group_by(turtle.id,date,observer,field,group) %>% filter(minutes >= video.change)

nov_data_obs <- nov_data %>% group_by(turtle.id,date,observer,field,group) %>% summarise(total.duration=sum(duration))

nov_data_obs <- nov_data_obs %>% arrange(turtle.id,date)

nov_data <- nov_data_obs %>% group_by(turtle.id,date,field,group) %>% summarise(mean.duration=mean(total.duration))

nov_data <- nov_data %>% arrange(turtle.id,date)

nov_data <- nov_data %>% mutate(freq=mean.duration/1200)

#nov_data_alt <- nov_data %>% filter(turtle.id != "L192")# %>% filter(turtle.id!= "L200") 

write.csv(nov_data_obs,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/nov_data_observers_2-5-2021_updated.csv")

write.csv(nov_data,"C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/DataSheets/nov_data_11-9.csv")

#one_monthdata_alt <- one_monthdata %>% filter(id != "L190") #%>% filter(id != "L191")

observer_difference <- nov_data_obs %>% group_by(turtle.id,date) %>% summarize(difference=max(total.duration)-min(total.duration))

mean(observer_difference$difference) #15.6 seconds

library(car)
leveneTest(mean.duration~field,data=dfs_nov18_total) #passes

wilcox.test(mean.duration~field,nov_data)

t.test(freq~field,nov_data)


library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)



annotation_df1 <- data.frame(field.type=rep(c("FL","FL")),
                             y=c(0.086,0.09))

annotation_df2 <- data.frame(field.type=rep(c("FL","MA")),
                             y=c(0.09,0.09))

annotation_df3 <- data.frame(field.type=rep(c("MA","MA")),
                             y=c(0.09,0.086))


nov_plot<-ggplot(nov_data,aes(x=field,y=freq))+
  stat_summary(fun.y= mean,geom="bar",color="grey",fill="grey")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),
               geom="errorbar",color="black")+
  #geom_bar(data=purstats,aes(x=field.type,y=meanfreq),stat="identity")+
  geom_point(position=position_jitter(width=0.1))+
  theme_bw()+
  coord_trans(y="sqrt")+
  #scale_y_sqrt(breaks= c(0.01,0.02,0.04,0.08,0.16),limits=c(0,0.1),expand=c(0,0))+
  scale_y_continuous("Proportion of Time",breaks = c(0,0.01,0.02,0.04,0.08,0.16),expand=c(0,0),limits=c(0,0.16))+
  #coord_cartesian(ylim=c(0,0.1))+
  scale_x_discrete("Treatment")+
  ggtitle("") +
  theme(text=element_text(size=22,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5,family = "Calibri Light",size=18,face="plain"))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #theme(axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_line(data=annotation_df1,aes(x=field.type,y=y))+
  geom_segment(data=annotation_df2,aes(x="FL",xend="MA",y=0.09,yend=0.09))+
  geom_line(data=annotation_df3,aes(x=field.type,y=y))+
  annotate("text",
           x = c(1.5),
           y = c(0.1),
           label = c("p = 0.0028"),
           family = "Calibri", fontface = 3, size=5)
nov_plot

