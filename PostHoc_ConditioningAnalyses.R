
rm(list=ls())

library(dplyr)
#library(plyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
require(scales)

data2017 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Post-Hoc Analyses/data_all_byobs_2017.csv",header=T)

data2017 <- data2017 %>% dplyr::select(turtle.id,date,observer,minutes,start,stop,duration,group,field,field.type,video.change) %>% mutate(year=rep(2017))

data2018 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Post-Hoc Analyses/data_all_byobs_2018.csv",header=T)

data2018 <- data2018 %>% dplyr::select(turtle.id,date,observer,minutes,start,stop,duration,group,field,field.type,video.change)%>% mutate(year=rep(2018))

data2019 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Post-Hoc Analyses/data_all_byobs_2019.csv",header=T)

data2019 <- data2019 %>% mutate(field.type=ifelse(field=="MA","conditioned","control")) %>% dplyr::select(turtle.id,date,observer,minutes,start,stop,duration,group,field,field.type,video.change)%>% mutate(year=rep(2019))

data2020spr <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Post-Hoc Analyses/data_all_byobs_2020Spr.csv",header=T)

data2020spr <- data2020spr %>% dplyr::select(turtle.id,date,observer,minutes,start,stop,duration,group,field,field.type,video.change)%>% mutate(year=rep("2020spr"))

data2020fa <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Post-Hoc Analyses/data_all_byobs_2020Fall.csv",header=T)

data2020fa <- data2020fa %>% dplyr::select(turtle.id,date,observer,minutes,start,stop,duration,group,field,field.type,video.change)%>% mutate(year=rep("2020fa"))

allyears <- rbind(data2017,data2018,data2019,data2020spr,data2020fa)

allyears <- allyears %>% arrange(turtle.id,date,minutes,observer)

allyears <- allyears %>% mutate(trial.minutes=minutes-video.change)


allyears <- allyears %>% mutate(mins.round=round(minutes,digits=2))

allyears<- allyears %>% mutate(trialmins.ceiling=ceiling(trial.minutes))

allyears <- allyears %>% mutate(time.category=ifelse(trialmins.ceiling<=5,"1-5",
                                                         ifelse(trialmins.ceiling>5 & trialmins.ceiling<=10,"6-10",
                                                                ifelse(trialmins.ceiling>10 & trialmins.ceiling<=15,"11-15",
                                                                       "16-20"))))



#allyears3 <- allyears %>% filter(field.type=="conditioned") %>% group_by(turtle.id,date,minutes,start,stop,duration,group,field,field.type,year,trial.minutes,trialmins.ceiling) #%>% distinct(mins.round)

#allyears2 <- allyears %>% filter(duration>=5)

detach(package:plyr)
library(dplyr)
allyears<- allyears %>% arrange(turtle.id,date,observer,trial.minutes,trialmins.ceiling) %>% group_by(turtle.id,date,observer) %>% mutate(count = seq(n()))

allyears_cond <- allyears %>% filter(field.type=="conditioned")

allyears_cond <- allyears_cond %>% mutate(time.category=ifelse(trialmins.ceiling<=5,"1-5",
                                                                   ifelse(trialmins.ceiling>5 & trialmins.ceiling<=10,"6-10",
                                                                          ifelse(trialmins.ceiling>10 & trialmins.ceiling<=15,"11-15",
                                                                                 "16-20"))))
#allyears_cond <- allyears_cond %>% group_by(turtle.id,date,year,observer,time.category)%>% mutate(event.count=rep(1))

library(plyr)
allyears_cond1_alt <- allyears_cond %>% group_by(turtle.id,date,year,observer) %>% mutate(trialmins.round=round_any(trial.minutes,0.2,f=ceiling))
detach(package:plyr)

allyears_cond1_alt <- allyears_cond1_alt %>% group_by(turtle.id,date,year,observer,field,field.type,trialmins.round) %>% summarize(duration=sum(duration))

allyears_cond1_alt <- allyears_cond1_alt %>% mutate(trialmins.ceiling=ceiling(trialmins.round))

allyears_cond1_alt2 <- allyears_cond1_alt %>% group_by(turtle.id,date,year,field,field.type,trialmins.round) %>% summarize(duration=mean(duration))

allyears_cond1_alt2 <- allyears_cond1_alt2 %>% mutate(trialmins.ceiling=ceiling(trialmins.round))

allyears_cond1_alt2 <- allyears_cond1_alt2 %>% mutate(log.duration=log(duration))

allyears_cond1_alt2.1 <- allyears_cond1_alt2 %>% filter(duration >= 5)

allyears_cond1 <- allyears_cond %>% group_by(turtle.id,date,year,observer) %>% mutate(time.btwn.events=start-lag(stop),default=first(stop))



mean(allyears_cond1$time.btwn.events,na.rm=T)

allyears_cond1 <- allyears_cond1 %>% group_by(turtle.id,date,year,observer) %>% mutate(continuous.event=ifelse(time.btwn.events<=10,"continuous","non-continuous"))

allyears_cond1$continuous.event[is.na(allyears_cond1$continuous.event)] <- "first_event"


#allyears_cond <- allyears_cond %>% group_by(turtle.id,date,year,field,observer) %>% mutate(new.duration=lag(duration)+duration)

detach(package:plyr)
library(dplyr)

#allyears_cond_chk <- allyears_cond %>% dplyr::select(c(turtle.id,date,observer,start,minutes,trial.minutes,time.btwn.events,continuous.event,duration))

allyears_cond1 <- allyears_cond1 %>% mutate(start=ifelse(continuous.event=="continuous",lag(start),start))
#above line times 3

allyears_cond1 <- allyears_cond1 %>% mutate(minutes=start/60)


allyears_cond1 <- allyears_cond1 %>% mutate(stop=ifelse(continuous.event=="continuous",lag(stop),stop))
#above line times 3

allyears_cond1 <- allyears_cond1 %>% group_by(turtle.id,date,observer,start,minutes) %>% mutate(duration=sum(duration))

allyears_cond1 <- allyears_cond1 %>% dplyr::select(c(turtle.id,date,year,observer,start,stop,minutes,duration,group,field,field.type,video.change)) %>% distinct()

allyears_cond1 <- allyears_cond1 %>% mutate(trial.minutes=minutes-video.change)

allyears_cond1 <- allyears_cond1 %>% mutate(mins.round=round(minutes,digits=2))

allyears_cond1 <- allyears_cond1 %>% mutate(trialmins.ceiling=ceiling(trial.minutes))

allyears_cond1 <- allyears_cond1 %>% mutate(time.category=ifelse(trialmins.ceiling<=5,"1-5",
                                                               ifelse(trialmins.ceiling>5 & trialmins.ceiling<=10,"6-10",
                                                                      ifelse(trialmins.ceiling>10 & trialmins.ceiling<=15,"11-15",
                                                                             "16-20"))))



allyears_test<- allyears_cond %>% dplyr::select(c(turtle.id,date,observer)) %>% distinct() 

allyears_test <- allyears_test %>% group_by(turtle.id,date)%>% mutate(obs.count=seq(n()))


allyears_alt <- allyears_cond1 %>% filter(field.type=="conditioned") %>% group_by(turtle.id,date,year,field,field.type,observer,trialmins.ceiling,time.category)%>% summarize(duration=sum(duration))

allyears_alt <- merge(allyears_alt,allyears_test)

allyears_alt2 <- allyears_alt %>% group_by(turtle.id,date,year,field,field.type,trialmins.ceiling)%>% summarize(mean.duration=mean(duration))

allyears_alt2 <- allyears_alt2 %>% mutate(time.category=ifelse(trialmins.ceiling<=5,"1-5",
                                                               ifelse(trialmins.ceiling>5 & trialmins.ceiling<=10,"6-10",
                                                                      ifelse(trialmins.ceiling>10 & trialmins.ceiling<=15,"11-15",
                                                                             "16-20"))))
allyears_alt2 <- allyears_alt2 %>% mutate(duration.round=ceiling(mean.duration))

allyears_alt2 <- allyears_alt2 %>% mutate(log.duration=log(mean.duration))



allyears_cond2 <- allyears_cond1 %>% filter(duration>=5)
mean(allyears_cond2$duration)

allyears_cond2 <- allyears_cond2 %>% mutate(duration.round=ceiling(duration))

allyears_cond2 <- allyears_cond2 %>% mutate(log.duration=log(duration))

mean(allyears_cond2$duration)

allyears_cond3 <- allyears_cond2 %>% filter(duration>=10)

allyears_cond1 <- allyears_cond1 %>% mutate(duration.round=ceiling(duration)) %>% mutate(log.duration=log(duration))

allyears_cond1 <- merge(allyears_cond1,allyears_test,by=c("turtle.id","date","observer"))

library(plyr)
allyears_cond1 <- allyears_cond1 %>% mutate(trialmins.round=round_any(trial.minutes,0.2,f=ceiling))

detach(package:plyr)
allyears_cond1.2 <- allyears_cond1 %>% group_by(turtle.id,date,year,observer,group,field,field.type,trialmins.round) %>% summarize(duration=sum(duration))

allyears_cond1.2 <- allyears_cond1.2 %>% group_by(turtle.id,date,year,group,field,field.type,trialmins.round)%>% summarize(duration=mean(duration))

allyears_cond1.2 <- allyears_cond1.2 %>% mutate(trialmins.ceiling=ceiling(trialmins.round))
#%>% group_by(turtle.id,date,observer)%>% mutate(obs.count=seq(n()))
#allyears_cond_chk <- allyears_cond_chk %>% mutate(trial.minutes=ifelse(continuous.event=="continuous",lag(trial.minutes),trial))


#allyears_eventsum <- allyears_cond %>% group_by(turtle.id,date,year,observer,trialmins.ceiling) %>% summarize(event.count=sum(event.count))

#allyears_eventsum <- allyears_eventsum %>% group_by(turtle.id,date,year,trialmins.ceiling) %>% summarize(event.count=mean(event.count))

mean(allyears_cond$duration)

detach(package:plyr)
allyears4 <- allyears_cond %>% dplyr::filter(duration >= 5) #%>% (duration.round=ceiling(duration))
library(dplyr)

allyears4 <- allyears4 %>% mutate(duration.round=ceiling(duration))

mean(allyears4$duration)


#mean(allyears4$duration)

allyears5 <- allyears4 %>% group_by(turtle.id,date) %>% filter(count==min(count))

allyears6 <- allyears4 %>% group_by(turtle.id,date) %>% filter(duration==max(duration))

allyears6 <- allyears6 %>% mutate(color=rep("longest_duration"))

mean(allyears6$duration)

allyears5 <- allyears5 %>% mutate(color=rep("first_true_event"))

mean(allyears5$trial.minutes)

allyears7 <- rbind(allyears5,allyears6)

allyears7 <- allyears7 %>%  arrange(turtle.id,date,observer,trial.minutes,trialmins.ceiling)

allyears7 <- merge(allyears7,allyears_test,by=c("turtle.id","date","observer"))

allyears8 <- allyears_cond %>% group_by(turtle.id,date,observer) %>% filter(duration >= 10) %>% filter(count==min(count)) 

#allyears9 <- allyears %>% group_by(turtle.id,date,observer,trialmins.ceiling) %>% filter(duration >= 5)

allyears9 <- allyears_cond %>% arrange(turtle.id,date,observer,trialmins.ceiling) %>% filter(duration >=5)



#####


library(nlme)
library(MASS)
library(vcd)
library(pscl)
library(jtools)
library(lme4)
library(nlme)
#detach(package:lme4)



#allyears_cond <- allyears_cond %>% mutate(duration.round=ceiling(duration))

fit<-goodfit(allyears_cond$duration.round)
summary(fit)
rootogram(fit)

allyears7<- allyears7 %>% mutate(log.duration=log10(duration))

Ord_plot(allyears_cond2$duration.round)

linear <- glmer(log.duration ~ trialmins.ceiling + (1|turtle.id), data = allyears_alt2) #identity link, OLS
pois <- glm(dur.round ~ trialmins.ceiling, family = "poisson",data = allyears_alt2) #Poisson
negb <- glm.nb(dur.round ~ trialmins.ceiling, data = allyears_alt2) #negative binomial
#in the MASS package
#zinb <- zeroinfl(dur.round ~ trialmins.ceiling|trialmins.ceiling, data = allyears_alt2,
                 dist = "negbin")

summ(pois,exp=T)
summ(negb,exp=T)
summ(linear,exp=T)

qqnorm(residuals(lme2,type="normalized"))
qqline(residuals(lme2,type="normalized"))

plot(density(allyears_cond2$duration.round))
ggqqplot(allyears4$log.duration)



library(dplyr)
#allyears_alt2 <- allyears_alt2 %>% mutate(log.duration=log10(mean.duration))

allyears_alt2.1 <- allyears_alt2 %>% filter(year!="2020spr") 

allyears4 <- allyears4 %>% mutate(log.duration=log(duration))
plot(density(allyears4$log.duration))

#plot(density(allyears_cond2$log.duration))

library(emmeans)
lme1 <- lme(log(duration)~as.factor(trialmins.ceiling)*field,random=~1|observer/turtle.id,data=allyears_cond)
summary(lme1)
anova(lme1)
emmeans(lme1, list(pairwise ~ field), adjust = "tukey")

mean(allyears7$trial.minutes)

library(emmeans)
lme2 <- lme(log.duration~as.factor(trialmins.ceiling),random=list(turtle.id=~1,observer=~1),data=allyears4)
summary(lme2) ###minute 2 is significant factor, p= 0.0058
anova(lme2) ##anova p=0.04 with log.duration
emmeans(lme2,list(pairwise ~ as.factor(trialmins.ceiling)))

lme3 <- lme(log.duration~((trialmins.ceiling))+color,random=list(turtle.id=~1,observer=~1),data=allyears7)
summary(lme3)
anova(lme3)

library(nlme)
library(emmeans)
lme4 <- lme(trialmins.ceiling~color*log.duration,random=~1|turtle.id,data=allyears7)
summary(lme4)
anova(lme4)
emmeans(lme4, list(pairwise ~ color), adjust = "tukey")


lme5 <- lme(log.duration~as.factor(trialmins.ceiling),random = ~1|turtle.id,data=allyears_cond1_alt2.1)
summary(lme5)###min 2 is significant at 0.0076
anova(lme5) ##marginally sig at 0.08
emmeans(lme5,list(pairwise~trialmins.ceiling))


lme5.1 <- lme(log.duration~as.factor(trialmins.ceiling),random = ~1|turtle.id,data=subset(allyears_cond1_alt2,duration>=2))
summary(lme5.1) ##2 MINS SIG AT 0.0489
anova(lme5.1) ##SIG AT 0.02

lme5.2 <- lme(log.duration~as.factor(trialmins.ceiling),random = ~1|turtle.id,data=subset(allyears_cond1_alt2,duration>=1))
summary(lme5.2)
anova(lme5.2) #SIG AT 0.0016

allyears_alt2$fitted.lme <- predict(lme5,type="response")

library(lme4)
library(emmeans)
glm1 <- lmer(duration~as.factor(trialmins.ceiling)+(1|turtle.id),data=allyears_cond1_alt2)
summary(glm1)
anova(glm1)
#chisq.test(glm1)
emmeans(glm1,list(pairwise~time.category),adjust="tukey")

#glm <- glm(Y ~  X1 + X2 + X3, data = simdata, family = Gamma("inverse"))

glm1 <- glmer(duration.round~(trialmins.ceiling)*field+(1|turtle.id/observer),family="poisson",data=allyears_cond2)
summary(glm1)
#chisq.test(glm1)

qqnorm(residuals(glm2))
mean(allyears_alt2$dur.round)
var(allyears_alt2$dur.round)

allyears_cond1.1 <-allyears_cond1 %>% filter(obs.count==2)

mean(allyears_cond1_alt2$duration)

library(MASS)
glm.nb1 <- glm.nb(ceiling(duration)~(trialmins.ceiling),link=log,data=allyears_cond1)
summary(glm.nb1)

#allyears_alt2$fitted<- predict(glm1, type = "response")

glm2 <- glmer.nb(duration~as.factor(trialmins.ceiling)+(1|turtle.id),data=subset(allyears_cond1_alt2,duration>=2))
summary(glm2)## best fit

glm2 <- glmer.nb(duration~as.factor(trialmins.ceiling)+(1|turtle.id),data=subset(allyears_cond1_alt2,duration>=1))
summary(glm2)

glm2.1 <- glmer.nb(duration~as.factor(trialmins.ceiling)+(1|turtle.id),data=allyears_cond1_alt2.1)
summary(glm2.1)## best fit

#allyears_cond1_alt2.1 <- allyears_cond1_alt2 %>% filter(duration>=5)

glm2.2 <- glmer.nb(duration~as.factor(trialmins.ceiling)+(1|turtle.id),data=allyears_cond1_alt2)
summary(glm2.2)

glm2.3 <- glmer.nb(duration.round~(trialmins.ceiling)+(1|observer)+(1|turtle.id),data=allyears_cond2)
summary(glm2.3)

glm3 <- glmer.nb(trialmins.ceiling~color*field+(1|turtle.id),data=allyears7)
summary(glm3)##good fit

glm4 <- glmer.nb(event.count~as.factor(trialmins.ceiling)+(1|turtle.id),data=allyears_eventsum)
summary(glm4)

glm2 <- glmer.nb(ceiling(duration)~time.category+(1|turtle.id),data=allyears4)
summary(glm2)

#pchisq(2 * (logLik(nb_model) - logLik(p_model)), df = 1, lower.tail = FALSE)
#anova(glm1)

#allyears_min <- allyears %>% group_by(turtle.id,date,field,field.type) %>% filter(trial.minutes==min(trial.minutes)) %>% filter(field.type=="conditioned")
#allyears_max <- allyears %>% group_by(turtle.id,date,field,field.type) %>% filter(duration==max(duration)) %>% filter(field.type=="conditioned")

allyears_max <- allyears_max %>% group_by(turtle.id,date) %>% slice(1)


mean(allyears$min(trial.minutes))

allyears_alt3<- allyears_alt %>% group_by(turtle.id,date,observer) %>% filter(duration==max(duration))

mean(allyears_alt3$trialmins.ceiling)

mean(subset(allyears7,color=="first_true_event")$trial.minutes)
mean(subset(allyears7,color=="longest_duration")$trial.minutes)

mean(allyears_cond1_alt2$duration)
mean(allyears_cond1_alt$mean.duration)

allyears_alt2.2 <- allyears_alt2 %>% filter(mean.duration>=4.5)


allyears_cond4 <- allyears_cond2 %>% filter(year!="2020spr")

allyears_cond1_alt3 <- allyears_cond1_alt %>% group_by(turtle.id,year,date,)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(allyears_cond1_alt2$duration)

#####PLOTS
plot17<-ggplot(subset(allyears_cond1_alt2,duration>=1),aes(x=trialmins.ceiling,y=duration))+
  stat_summary(fun= mean,geom="bar")+
  geom_point(aes(size=duration,color=turtle.id),position=position_jitter(width=0.2))+
  #geom_point(data = allyears_alt2,aes(x=trialmins.ceiling,y=mean(mean.duration),color="red"),size=2)+
  scale_size_continuous(breaks=c(5,10,20,40,80))+
  #geom_smooth(method="loess",se=F)+
  theme_bw()+
  coord_trans(y="sqrt")+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot17

ggsave(plot17, width = 8, height=6,units="in",bg="white",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_project/Post-Hoc Analyses/Figures/SummedBetween0.2MinData_AveragedAcrossObservers_GreaterThan1Sec.png")


plot18<-ggplot(allyears_alt2.2,aes(x=trialmins.ceiling,y=mean.duration))+
  #stat_summary(fun= cumsum,geom="bar")+
  geom_point()+
  #scale_size_continuous(breaks=c(1,5,10,20,40,80))+
  theme_bw()+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot18

plot19<-ggplot(allyears_cond,aes(x=trialmins.ceiling,y=duration))+
  #stat_summary(fun= mean,geom="bar")+
 geom_point()+
  #geom_smooth(aes(y=fitted))+
  #geom_smooth(method='lm',formula=y~I(x^4)+I(x^3)+I(x^2))+
  #geom_line(aes(y=fitted,x=trialmins.ceiling))+
  #scale_size_continuous(breaks=c(1,5,10,20,40,80))+
  theme_bw()+
  coord_trans(y="sqrt")+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot19

plot20<-ggplot(allyears_alt2,aes(x=trialmins.ceiling,y=turtle.id))+
  #stat_summary(fun= mean,geom="bar")+
  geom_point(aes(size=mean.duration))+
  #geom_point(data=allyears6,aes(x=trialmins.ceiling,y=turtle.id),color="red")+
  #geom_smooth(aes(y=fitted))+
  #geom_smooth(method='lm',formula=y~I(x^4)+I(x^3)+I(x^2))+
  #geom_line(aes(y=fitted,x=trialmins.ceiling))+
  #scale_size_continuous(breaks=c(1,5,10,20,40,80))+
  theme_bw()+
  #coord_trans(y="sqrt")+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot20

ggsave(plot17, width = 8, height=6,units="in",bg="white",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_project/Post-Hoc Analyses/Figures/SummedBetweenShortEvents_AllEvents.png")


plot21<-ggplot(allyears7,aes(x=trialmins.ceiling,y=log(duration)))+
  #stat_summary(fun= mean,geom="bar")+
  geom_point(aes(size=duration,color=color))+
  #geom_point(data=allyears6,aes(x=trialmins.ceiling,y=turtle.id),color="red")+
  #geom_smooth(aes(y=fitted))+
  #geom_smooth(method='lm',formula=y~I(x^4)+I(x^3)+I(x^2))+
  #geom_line(aes(y=fitted,x=trialmins.ceiling))+
  #scale_size_continuous(breaks=c(1,5,10,20,40,80))+
  theme_bw()+
  #coord_trans(y="sqrt")+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot21

allyears_alt2 %>% group_by(turtle.id) %>% filter(mean.duration==max(mean.duration)) %>% group_by(trialmins.ceiling) %>% count()


allyears_alt3 <- allyears_alt2 %>% group_by(turtle.id,date,year,field,field.type) %>% mutate(time.category=ifelse(trialmins.ceiling<=5,"1-5",
                                                                                                                  ifelse(trialmins.ceiling>5 & trialmins.ceiling<=10,"6-10",
                                                                                                                         ifelse(trialmins.ceiling>10 & trialmins.ceiling<=15,"11-15",
                                                                                                                                "16-20"))))

allyears_alt3 <- allyears_alt3 %>% group_by(turtle.id,date,year,field,field.type,time.category) %>% summarize(summed.dur= sum(mean.duration))

allyears_alt3$time.category <- factor(allyears_alt3$time.category,levels=c("1-5","6-10","11-15","16-20"))

plot22<-ggplot(allyears_alt3,aes(x=time.category,y=summed.dur,group=time.category))+
  stat_summary(fun=sum,geom="bar")+
  #geom_smooth()+
  #geom_point()+
  theme_bw()+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot22

allyears_eventsum$time.category<-factor(allyears_eventsum$time.category,levels=c("1-5","6-10","11-15","16-20"))

plot23<-ggplot(allyears_eventsum,aes(x=time.category,y=event.count,group=time.category,color=turtle.id))+
  #stat_summary(fun=sum,geom="bar")+
  #geom_smooth()+
  geom_point()+
  theme_bw()+
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
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
plot23





