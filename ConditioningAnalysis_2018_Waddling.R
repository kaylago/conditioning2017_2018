
#2018 Conditioning - Waddling Analysis


rm(list=ls())


setwd("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/Conditioning2018_Analysis/")
temp = list.files(pattern="*.csv")
data = lapply(temp, read.csv, skip=15, header= T)

groupdata<-read.csv("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/2018Groups.csv",header=T)

library(plyr)

columns<- c("Time","Media.file.path","Total.length","FPS","Subject","Behavior","Comment","Status","Start_Stop")

renameFunction<-function(x,columns){
  names(x) <- columns
  return(x)
}

data= ldply(data,renameFunction,columns)


detach(package:plyr)
library(dplyr)

data <- data %>% select(-c("Total.length","FPS","Subject","Comment","Status"))



library(tidyr)
data <- data %>% separate(Media.file.path, into = c('media.file.path', 'date'), sep = -9, convert = TRUE)

data <- data %>% separate(date, into = c('date','file.type'),sep=-4,convert=TRUE)

data <- data %>% separate(media.file.path, into = c('media.file.path','ID'),sep=-5,convert=TRUE)

data <- data %>% separate(ID, into=c('ID','delete'),sep=-1,convert=TRUE)

data <- data %>% select (-c(file.type,delete))

#dfs <- dfs %>% filter(Time != "Time")

names(data)<- c("time","observer.file","id","date","behavior","start_stop")

class(data$date)


data <- data %>% mutate (date = ifelse(date=="NOV28","28-Nov-18",ifelse(date=="NOV29","29-Nov-18",ifelse(date=="NOV30","30-Nov-18",ifelse(date=="DEC01","01-Dec-18","02-Dec-18")))))

data$date<-as.Date(data$date,format="%d-%b-%y")

groupdata$date<-as.Date(groupdata$date,format="%m/%d/%Y")
class(groupdata$date)


data_start<-subset(data,start_stop== "START")
names(data_start)<- c("start.time","observer.file","id","date","behavior","start_stop")


data_stop<-subset(data,start_stop== "STOP")
names(data_stop)<- c("stop.time","observer.file","id","date","behavior","start_stop")


dfs_behaviors<-cbind(data_start,data_stop)

names(dfs_behaviors)<- c("start","observer.file", "id","date","behavior","start_stop","stop","observer.file.2","id.2", "date.2","behavior.2","start_stop.2")

dfs_behaviors <-dfs_behaviors %>% select(-c("observer.file.2", "date.2","behavior.2","start_stop","start_stop.2","id.2"))

class(groupdata$id)
class(dfs_behaviors$id)
class(dfs_behaviors$date)

dfs_behaviors$id <- as.factor(dfs_behaviors$id)

dfs_behaviors<-merge(dfs_behaviors,groupdata,by=c("id","date"))

dfs_behaviors$start<- as.numeric(dfs_behaviors$start)
dfs_behaviors$stop<- as.numeric(dfs_behaviors$stop)

dfs_behaviors<- mutate(dfs_behaviors,time.el= stop-start)
dfs_behaviors<- dfs_behaviors %>% mutate(minutes= start/60)

timechange.file <- read.csv("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/TrialTimes2018.csv")

class(dfs_behaviors$id)
class(timechange.file$id)
class(dfs_behaviors$date)
class(timechange.file$date)

timechange.file$date <- as.Date(timechange.file$date,format= "%m/%d/%Y")



dfs_behaviors<- merge(dfs_behaviors,timechange.file, by = c("id","date"))

dfs_acclimation<- dfs_behaviors %>% filter(minutes< field.change)
dfs_test<-dfs_behaviors %>% filter(minutes>field.change)

dfs_test<-dfs_test %>% filter(minutes < (field.change+20))

dfs_acclim_summary <- dfs_acclimation %>% group_by(date,id,observer.file,group,field,field.type) %>% summarise(total.time.el=sum(time.el))
dfs_test_summary <- dfs_test %>% group_by(date,id,observer.file,group,field,field.type) %>% summarise(total.time.el=sum(time.el))

#write.csv(dfs_acclim_summary,file="C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/dfs_acclim_summary.csv")
write.csv(dfs_test_summary,file="C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/dfs_test_summary.csv")

dfs_acclim_mean <- dfs_acclim_summary %>% group_by(date,id,group,field,field.type) %>% summarise(mean.time=mean(total.time.el))
dfs_test_mean <- dfs_test_summary %>% group_by(date,id,group,field,field.type) %>% summarise(mean.time=mean(total.time.el))

dfs_test_mean <- dfs_test_mean %>% mutate(prop=mean.time/1200)

#dfsclean <- read.csv("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/dfs_cleaned.csv")

#fs_clean_mean <- dfsclean %>% group_by(date,id,group,field,field.type) %>% summarise(mean.time=mean(total.time.el))

#dfs_clean_mean <- dfs_clean_mean %>% filter(id != "L179" & id != "L184")

#dfs_test_mean <- dfs_test_mean  %>% filter(id != "L179" & id != "L184")

class(dfs_test_mean$mean.time)

library(car)
leveneTest(mean.time~field,data=dfs_test_mean) #passes
bartlett.test(mean.time~field.type,data=dfs_test_mean) #doesn't pass
shapiro.test(dfs_test_mean$mean.time) # not normal

#dfs_test_mean <- dfs_test_mean %>% mutate(trans= log10(mean.time))

summary(lm(mean.time~field*group+id,data=dfs_test_mean))
anova(lm(mean.time~field*group+id,data=dfs_test_mean))

library(nlme)
lme1<-lme(mean.time~field.type+id,random = ~1|group,data=dfs_test_mean)
summary(lme1)
anova(lme1)



wilcox.test(mean.time~field.type,dfs_test_mean)
t.test(mean.time~field.type,dfs_test_mean)


orange <- dfs_test_mean %>% filter(group=="orange")

#orange<- orange%>% filter(id !="L179" & id != "L184")
wilcox.test(mean.time~field.type,orange)
t.test(mean.time~field.type,orange)

blue <- dfs_test_mean %>% filter(group=="blue")
wilcox.test(mean.time~field.type,blue)
t.test(mean.time~field.type,blue)

write.csv(dfs_test_mean,"C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/2018/2018_testperiod_means.csv")

library(ggplot2)
library(ggpubr)
library(extrafont)

orangeplot<-ggplot(orange,aes(x=field.type,y=prop))+
  stat_summary(fun.y="mean",geom="bar",color="#FF9933",fill="#FF9933")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),geom="errorbar",color="black")+
  theme_bw()+
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15),expand = c(0,0),"Proportion of Time")+
  coord_cartesian(ylim=c(0,0.15))+
  scale_x_discrete("Treatment")+
  ggtitle("New England Group") +
  theme(text=element_text(size=18,family="calibri"))+
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5))+
  theme(plot.margin = unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))
orangeplot


blueplot<-ggplot(blue,aes(x=field.type,y=prop))+
  stat_summary(fun.y="mean",geom="bar",color="#99CCFF",fill="#99CCFF")+stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) + sd(x)/length(x),geom="errorbar",color="black")+
  theme_bw()+
  theme(text=element_text(size=20,family="calibri")) +
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15),expand=c(0,0),name="Proportion of Time")+
  coord_cartesian(ylim=c(0,0.15))+
  scale_x_discrete(name="Treatment")+
  ggtitle("Cuba Group")+ 
  theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0),hjust=0.5))+
  theme(plot.margin=unit(c(0.2,0.2,0.3,0.2),"cm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))
blueplot

library(ggpubr)
figure<- ggarrange(orangeplot,blueplot,labels= c("A","B"),ncol = 2,nrow = 1)
figure
#par(oma=c(2,2,2,2))
annotate_figure(figure,
                top = text_grob("Food-Seeking Behavior", color = "black", face = "bold", size = 28),
                bottom = text_grob("Treatment", color = "black",
                                   hjust = 0.5, face = "italic", size = 24),
                left = text_grob("Proportion of Time", color = "black", rot = 90,size=24,face="italic")
)


fig3<-ggplot(dfs_test_mean,aes(x=field.type,y=prop))+
  stat_summary(fun.y="mean",geom="bar",fill="grey48")+
  stat_summary(fun.y=mean,fun.ymin = function(x) mean(x)-sd(x)/length(x),fun.ymax = function(x) mean(x) +
                 sd(x)/length(x),geom="errorbar",color="black")+
  theme_bw()+
  scale_y_continuous(name="Proportion of Time",breaks=c(0.00,0.04,0.08,0.12,0.16),expand = c(0,0))+
  scale_x_discrete(name="Treatment")+
  theme(plot.margin=margin(20,20,20,20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20,family = "Calibri"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20,family = "Calibri"))+
  theme(text=element_text(size=20,family="Calibri"))+
  coord_cartesian(ylim=c(0,0.14))
fig3



#####Boxplots


my_comparisons<- list(c("conditioned","control"))

figure1<-ggplot(dfs_test_mean,aes(x=field.type,y=prop))+
  stat_boxplot(geom="errorbar",color="black")+
  geom_boxplot(outlier.shape=NA,fill="#CCCCCC",color="black")+
  geom_point(color="black",position="jitter")+
  theme_bw()+scale_y_continuous(name = "Proportion of Time",breaks=c(0.00,0.04,0.08,0.12,0.16,0.2,0.24,0.28,0.32,0.36,0.4))+
  theme(axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20))+
  theme(axis.title.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20))+
  scale_x_discrete(name="Treatment")+theme(text=element_text(size=18,family="Calibri"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_compare_means(comparison=my_comparisons,label="p.signif",label.y = 0.4,method="wilcox.test")
figure1




figure2<-ggplot(blue,aes(x=field.type,y=prop))+
  stat_boxplot(geom="errorbar",color="black")+
  geom_boxplot(outlier.shape=NA,fill="#99CCFF",color="black")+
  geom_point(color="black",position="jitter")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.00,0.04,0.08,0.12,0.16,0.2,0.24,0.28,0.32,0.36,0.4,0.44,0.48,0.52,0.56),limits=c(0,0.44))+
  theme(axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20))+
  theme(axis.title.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20))+
  theme(text=element_text(size=18,family="Calibri"),axis.title.x = element_blank(),axis.title.y=element_blank())+
  theme(plot.margin = margin(t=40,r=20,b=10,l=20))+
  ggtitle("Cuba Group")+
  theme(plot.title = element_text(size = 22,hjust=0.5,family = "Calibri",margin = margin(t=0,r=0,b=20,l=0)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_compare_means(comparison=my_comparisons,label="p.signif",label.y = 0.4,method="wilcox.test")
figure2


figure3<-ggplot(orange,aes(x=field.type,y=prop))+
  stat_boxplot(geom="errorbar",color="black")+
  geom_boxplot(outlier.shape=NA,fill="#FF9933",color="black")+
  geom_point(color="black",position="jitter")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.00,0.04,0.08,0.12,0.16,0.2,0.24,0.28,0.32,0.36,0.4,0.44,0.48,0.52,0.56),limits=c(0,0.44))+
  theme(axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size=20))+
  theme(axis.title.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=20))+
  theme(text=element_text(size=18,family="Calibri"),axis.title.x = element_blank(),axis.title.y=element_blank())+
  theme(plot.margin = margin(t=40,r=20,b=10,l=20))+
  ggtitle("New England Group")+
  theme(plot.title = element_text(size = 22,hjust=0.5,family = "Calibri",margin = margin(t=0,r=0,b=20,l=0)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_compare_means(comparison=my_comparisons,label="p.signif",label.y = 0.4,method="wilcox.test")
figure3




library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)
figure4a<-plot_grid(figure2,figure3,rel_widths = c(1,1))+draw_label("A",fontfamily = "Calibri", x = 0, y = 1, hjust = -0.5, vjust = 1.5)+draw_label("B",fontfamily = "Calibri",x=0.5,y=1,hjust=0.5,vjust=1.5)
figure4a
grid.arrange(arrangeGrob(figure4a,top=text_grob(""),
                         bottom = text_grob("Treatment", color = "black",
                                            hjust = 0.5, size = 20,family="Calibri"),
                         left = text_grob("Proportion of Time", color = "black", rot = 90,size=20,family="Calibri"),padding=unit(2,"line"
                         )))



#ggplot(dfs_clean_mean,x=field.type,y=mean.time)+geom_boxplot(aes(x=field.type,y=mean.time))+geom_point(aes(x=field.type,y=mean.time))







