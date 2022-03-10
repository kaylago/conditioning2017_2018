rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps

library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)  # for web applications
library(ggmap)
library(googleway)
library(RgoogleMaps)
library(rnaturalearth)
library(ggrepel)
library(ggspatial)
library(rworldmap)
library(RColorBrewer)
library(viridis)
library(cowplot)

wmm_incl_data <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/WMM_WORLD_GRID_INCL.csv",skip=16,header=F)
names(wmm_incl_data) <- c("dec.date","lat","long","elev","incl","incl_del","na")

wmm_int_data <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/WMM_WORLD_GRID_INT.csv",skip=16,header=F)
names(wmm_int_data) <- c("dec.date","lat","long","elev","int","int_del","na")

land <- st_read("C:/Users/kkmgo/Dropbox/ne_10m_land/ne_10m_land.shp")

coastlines <- st_read("C:/Users/kkmgo/Dropbox/ne-coastlines-10m/ne_10m_coastline.shp")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

###PLOT CODE FOR INCLINATION

incl_wmm <- ggplot() +
  geom_raster(data=wmm_incl_data,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+#SETS COLOR GRADIENT TO INCLINATION
  geom_sf(data=world,fill="#B7B8BB",color="transparent")+#SETS LAND SHAPES AND FILL COLOR
  scale_fill_viridis_c(option = "turbo", direction=-1)+#SETS COLOR SCALE
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(10,-10,-20,20,30,-30,40,-40,50,-50,60,-60,70,-70,80,-80),color="black")+ #INCLINATION LINES, BREAKS SET MANUALLY BY 10 DEG
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl), breaks=0,color = "red")+ #LINE FOR MAGNETIC EQUATOR IN RED
  #geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-180, 180), ylim = c(-70, 70), expand = FALSE)+#SETS CONFINES OF PLOT - TO CHANGE THE PROJECTION, YOU SHOULD BE ABLE TO ALTER THE DETAILS OF THIS LINE
  labs(x="Longitude",y="Latitude",fill="Inclination")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+ #ANYTHING WITH THEME IS JUST ADJUST HOW THE PLOT LOOKS
  theme(panel.border = element_rect(color="black",fill="transparent"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#B5E0F8",color="black"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(labels = abs)+
  scale_y_continuous(labels=abs)
#annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
#geom_text(data=wmm_incl_data,aes(x=0,y=0,z=0),label="Magnetic Equator")
incl_wmm

###PLOT FOR INTENSITY OF WORLD
int_wmm <- ggplot() +
  geom_raster(data=wmm_int_data,aes(x=long,y=lat,group=int,fill=int),color = '#00000000',interpolate = T)+#ADDS COLOR GRADIENT
  geom_sf(data=world,fill="#B7B8BB",color="transparent")+ #SETS LAND SHAPES AND FILL COLOR
  scale_fill_viridis_c(option = "turbo",direction=-1)+#ADDS COLOR SCALE OF CHOICE -- YOU DON'T NEED VIRIDIS, BUT IT'S WHAT I USE
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  geom_contour(data=wmm_int_data,aes(x=long,y=lat,z=int),breaks=c(30000,35000,40000,45000,50000,55000,60000),color="black")+#MANUALLY SET INTENSITY BREAKS
  #geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-180, 180), ylim = c(-70, 70), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Intensity (nT)")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+ #ADD KEY FOR COLOR BUT NOTHING ELSE
  theme(panel.border = element_rect(color="black",fill="transparent"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#B5E0F8",color="black"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())+
  scale_x_continuous(labels = abs)+
  scale_y_continuous(labels=abs)
#annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
#geom_text(data=wmm_incl_data,aes(x=0,y=0,z=0),label="Magnetic Equator")
int_wmm

###SUBSET PLOT OF FLORIDA WITH GRADIENT BACKGROUND
incl_data_15 <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_data_2015.csv",skip=13,header=F) #INCL DATA FOR JUST ONE YEAR

incl15 <- ggplot() +
  geom_raster(data=incl_data_15,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  scale_fill_viridis_c(option = "turbo", trans = "sqrt",limits=c(49,68))+ #ADJUSTING COLOR SCALE TO ONLY SHOW THE INTENSITY VALUES WE CARE ABOUT
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  geom_contour(data=incl_data_15,aes(x=long,y=lat,z=incl),breaks = seq(35, 65, by = 0.5),color="black")+
  geom_sf(data=land,fill="grey42")+ #SETS LAND SHAPE WITH A FILL. REALLY YOU JUST NEED THE GEOM_SF WITH THE LAND OR THE COASTLINE SHAPES, BUT I'VE INCLUDED BOTH IN THE CODE
  geom_sf(data = coastlines, fill="grey16",color="black",size=1)+#HIGHER RESOLUTION COASTLINE/LAND OUTLINE
  coord_sf(xlim = c(-90, -76), ylim = c(22, 35), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination (deg)")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "lightskyblue1"),
        axis.line = element_line(color="black"),
        text = element_text(family="Arial",size = 12))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(labels=abs)+
  annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
incl15
#direct.label(incl, method="bottom.pieces")

ggsave(incl15,dpi=800, width = 2, height=2,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_2015_11-15-21.tiff",device='tiff')



