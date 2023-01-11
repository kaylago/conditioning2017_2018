rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(spData)

#library(spDataLarge)

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel","ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

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
#sites <- read.csv("C:/Users/kkmgo/OneDrive/Documents/Caretta caretta Conditioning 2017/For R_lat_long_field locations.csv")
#sites <- st_as_sf(sites, coords = c("latitude", "longitude"))

#data(land)

#st_crs(sites) <- st_crs(land)

#tm_shape(land)+
 # tm_shape(sites)

#projection(sites)
 

#ggmap::register_google(key = "AIzaSyA9zjHcg8lzRTl-tTxb-zO0bBsFPiKKzSE")
#ggmap(get_googlemap(center = c(lon = -118, lat = 45),
 #                   zoom = 8, scale = 2,
  #                  maptype ='terrain',color = 'color'))


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#ridley map - modeling

sites <- read.csv("C:/Users/kkmgo/Dropbox/Feasibility/ridley_release_points.csv",header=T)

zones <- read.csv("C:/Users/kkmgo/Dropbox/Feasibility/ridley_zone_labels.csv",header=T)

sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant")

sites$group <-as.factor(sites$group)

ggplot() +
  geom_sf(data=world,fill="chocolate4",color="black")+

  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_polygon(data=sites,aes(x=longitude,y=latitude,group=group,fill=group),alpha=0.7,color="black")+
  #geom_point(data=olive_beaches,aes(x=long,y=lat),size=3)+
  scale_fill_brewer(palette = "Pastel1")+
  #scale_fill_manual(values = c("Zone 1" ="red", "Zone 2"="orange","Zone 3"="yellow","Zone 4"="darkgreen","Zone 5"="green","Zone 6"="darkblue","Zone 7"="lightblue","Zone 8"="purple","Zone 9"="lightpurple"))+
  geom_text(data=zones,aes(x=longitude,y=latitude,label=zone),size=4,fontface="bold",color="white")+
  coord_sf(xlim = c(-115, -74), ylim = c(-5, 30), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,fill=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "darkcyan"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=14,family="Calibri"),axis.text = element_text(color="black",size=12,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)




###############Conditioning Maps

land <- st_read("C:/Users/kkmgo/OneDrive/Documents/ne_10m_land/ne_10m_land.shp")

coastlines <- st_read("C:/Users/kkmgo/OneDrive/Documents/ne-coastlines-10m/ne_10m_coastline.shp")


cond_sites <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/MagneticFieldInfo/ForR_LatLong.csv",header=T)

cond_sites$field  <- factor(cond_sites$field, levels=c("Mexico","New Hampshire","Delaware","Cuba","Maine","Florida","Virginia","Newfoundland","Turks & Caicos","Haiti","Acclimation"),
                               labels=c("Mexico","New Hampshire","Delaware","Cuba","Maine","Florida","Virginia","Newfoundland","Turks & Caicos","Haiti","Acclimation"))

#cond_sites_19 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/ForR_lat_long_2019.csv",header=T)

labels <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/MagneticFieldInfo/ForR_maplabels.csv",header=T)

labels$label <- factor(labels$label, levels=c("Mexico","New Hampshire","Delaware","Cuba","Maine","Florida","Virginia","Newfoundland","Turks & Caicos","Haiti","Acclimation"),
                       labels=c("Mexico","New Hampshire","Delaware","Cuba","Maine","Florida","Virginia","Newfoundland","Turks & Caicos","Haiti","Acclimation"))


labels2 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/MagneticFieldInfo/ForR_maplabels_smallplots.csv",header=T)

labels2$label <- factor(labels$label, levels=c("Mexico","New Hampshire","Delaware","Cuba","Maine","Florida","Virginia","Newfoundland","Turks & Caicos","Haiti","Acclimation"),
                        labels=c("Mexico","New Hampshire","Delaware","Cuba","Maine","Florida","Virginia","Newfoundland","Turks & Caicos","Haiti","Acclimation"))


#labels_19 <-  read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/ForR_location_labels_2019.csv",header=T)

#cond_sites <- rbind(cond_sites_17,cond_sites_19)

#labels <- rbind(labels,labels_19)

#cond_sites_17 <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                  #crs = 4326, agr = "constant")

#anno <- data.frame(label="Florida",
             #      lat=27.9,
              #     long=-78)
#anno


cond_map<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=cond_sites,aes(x=long,y=lat,color=field),size=7)+
  scale_color_manual(values = c("plum4","plum4","turquoise4","turquoise4","darkseagreen4","darkseagreen4","goldenrod3","goldenrod3","pink3","pink3","black"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)) # bg of the plot

cond_map



#ggsave(cond_map, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/map_of_sites_.png",  bg = "white")

ggsave(cond_map, dpi=300,width=8,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map_of_sites_9-6-2022.tiff",  bg = "white")

cond_map2<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2017"),aes(x=long,y=lat,color=location),size=8)+
  scale_color_manual(values = c("black", "plum4","plum4","turquoise4","turquoise4","darkseagreen4","darkseagreen4","goldenrod3","goldenrod3","pink3","pink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  coord_sf(xlim = c(-90, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)) # bg of the plot

cond_map2

ggsave(cond_map2, dpi=300,width=4,height=4,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map_of_sites_5-10-2022.tiff",  bg = "white")

cond_map17<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2017"),aes(x=long,y=lat,color=field),size=10)+
  scale_color_manual(values = c("orchid4","orchid4","turquoise4","turquoise4","darkseagreen4","darkseagreen4","goldenrod3","goldenrod3","pink3","pink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=subset(labels2, year=="2017"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Arial"),
        axis.text = element_text(color="black",size=12,family="Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map17

ggsave(cond_map17, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map2017_11-25-2022.tiff",  bg = "white")

cond_map18<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2018"),aes(x=long,y=lat,color=field),size=10)+
  scale_color_manual(values = c("turquoise4","turquoise4","darkseagreen4","darkseagreen4","goldenrod3","goldenrod3","pink3","pink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=subset(labels2,year=="2018"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map18
ggsave(cond_map18, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map2018_11-25-2022.tiff",  bg = "white")

cond_map19<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2019"),aes(x=long,y=lat,color=field),size=10)+
  scale_color_manual(values = c("seagreen","seagreen","goldenrod3","goldenrod3","pink3","pink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=subset(labels2,year=="2019"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map19
ggsave(cond_map19, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map2019_pres_11-30-2022.tiff",  bg = "white")

cond_map20sp<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2020spr"),aes(x=long,y=lat,color=field),size=10)+
  scale_color_manual(values = c("darkgoldenrod4","darkgoldenrod4","pink3","pink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=subset(labels2,year=="2020spr"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map20sp
ggsave(cond_map20sp,dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map2020spr_11-25-2022.tiff",  bg = "white")


cond_map20fa <- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2020fa"),aes(x=long,y=lat,color=field),size=10)+
  scale_color_manual(values = c("hotpink3","hotpink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=subset(labels2,year=="2020fa"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"))+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank())# bg of the plot

cond_map20fa

ggsave(cond_map20fa, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map2020fa_11-25-2022.tiff",  bg = "white")


#################Spring 2020 conditioning map

cond_sites20 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_lat_long_2020.csv",header=T)

obx_sites <- cond_sites20 %>% filter(location!="New Brunswick (NB)")%>% filter(location!="Prince Edward Island")%>% filter(location!="Acclimation")

obx_sites$location <- factor(obx_sites$location,levels=c("Northern Outer Banks (OBX)","Maryland","New Jersey ","New Hampshire (mid-point)","Nova Scotia"))

nb_sites <- cond_sites20 %>% filter(location!="Northern Outer Banks (OBX)")%>% filter(location!="Maryland")%>% filter(location!="Acclimation")

nb_sites$location <- factor(nb_sites$location,levels=c("New Brunswick (NB)","Prince Edward Island","Nova Scotia","New Hampshire (mid-point)","New Jersey "))

#cond_sites_19 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/ForR_lat_long_2019.csv",header=T)

labels20 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_location_labels_2020.csv",header=T)

obx_labs <-  read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_obx_labels_2020.csv",header=T)

nb_labs <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_nb_labels_2020.csv",header=T)

sensplot<- ggplot() +
  geom_sf(data=land,fill="antiquewhite")+
  geom_sf(data = coastlines, fill="transparent",color="grey16",size=.5)+
  coord_sf(xlim = c(-85, -38), ylim = c(27, 55), expand = FALSE)+
  geom_point(data=cond_sites20,aes(x=long,y=lat,color=location),size=8)+
  scale_color_manual(values = c("black", "black","maroon","black","black","sienna3","black","black"))+
  #geom_text(data=labels20,aes(x=long,y=lat,label=label),size=3,fontface="bold",color="black")+
  geom_label(data=labels20,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),alpha=0.8)+
  #coord_sf(xlim = c(-83, -40), ylim = c(25, 55), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "lightcyan2"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1))+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)
sensplot

ggsave(sensplot,dpi=600, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/sens_2020_map_5-10-22.tiff")

cond_map20fa_pres <- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=subset(cond_sites,year=="2020fa"),aes(x=long,y=lat,color=field),size=10)+
  scale_color_manual(values = c("violetred4","darkorange3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=subset(labels2,year=="2020fa"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"))+
  coord_sf(xlim = c(-100, -45), ylim = c(8, 53), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = alpha('slategray1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map20fa_pres

ggsave(cond_map20fa_pres, dpi=300,width=6,height=6,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/map2020fa_pres_12-2-2022.tiff",  bg = "white")

###########


obx<-ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=obx_sites,aes(x=long,y=lat,size=14,color=location))+
  scale_color_manual(values = c("sienna4","darkorange3","sienna2","darkorange1","coral1"))+
  geom_text(data=obx_labs,aes(x=long,y=lat,label=label),size=6,family="Calibri",color="black")+
  coord_sf(xlim = c(-80, -60), ylim = c(34, 47), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightsteelblue1"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=18,family="Calibri"),axis.text = element_text(color="black",size=16,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)
obx


nb<-ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=nb_sites,aes(x=long,y=lat,size=14,color=location))+
  scale_color_manual(values = c("violetred4","maroon4","deeppink3","violetred2","magenta"))+
  geom_text(data=nb_labs,aes(x=long,y=lat,label=label),size=6,family="Calibri",color="black")+
  coord_sf(xlim = c(-75, -55), ylim = c(37, 51), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightsteelblue1"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=18,family="Calibri"),axis.text = element_text(color="black",size=16,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)
nb

ggsave(obx,dpi=600, width = 6, height=6,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/obx_2020_map.png")

ggsave(nb,dpi=600, width = 6, height=6,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/nb_2020_map.png")
####################################


inc_sens<- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/MagneticFieldInfo/InclinationSensitivityDots.csv",header=T)


inc_sens_labs<- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/MagneticFieldInfo/InclinationSensitivityLabels.csv",header=T)

inc_sens$location <- factor(inc_sens$location,levels=c("Turks & Caicos","Haiti","TC+2","TC-2","TC+1","TC-1","HT+2","HT-2","HT+1","HT-1"))

inc_sens$color <- factor(inc_sens$color,levels=c("Turks & Caicos",
                                                 "Turks & Caicos - 1 degree difference",
                                                 "Turks & Caicos - 2 degree difference",
                                                 "Haiti",
                                                 "Haiti - 1 degree difference",
                                                 "Haiti - 2 degree difference"
                                                 ))

#cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")

cond_map_inc <- ggplot() +
  geom_sf(data=land,fill="antiquewhite")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=.5)+
  #geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=inc_sens,aes(x=long,y=lat,color=as.factor(color)),size=8)+
  scale_color_manual(values = c("violetred4","violetred2","pink1","darkorange4","darkorange2","orange1"))+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57),color="black")+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  geom_label(data=inc_sens_labs,aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"))+
  coord_sf(xlim = c(-85, -65), ylim = c(16, 28), expand = FALSE)+
  labs(x="Longitude",y="Latitude",color="Treatment")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE)+
  #theme(legend.position = "none"),
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = alpha('lightskyblue1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        legend.text = element_text(size=14,family="Helvetica"),
        legend.title = element_text(size=14,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map_inc

library(ggpubr)
leg <- get_legend(cond_map_inc)
leg<-as_ggplot(leg)
leg

cond_map_inc_plot <- cond_map_inc+theme(legend.position = "none")
cond_map_inc_plot
ggsave(cond_map_inc_plot,dpi=300, width = 8, height=5,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/inc_sens_12-1-22.tiff",bg = "white")


ggsave(leg,dpi=300, width = 4, height=2.5,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/inc_sens_legend_12-1-22.tiff",bg = "white")


cond_map_inc2 <- ggplot() +
  geom_raster(data=wmm_incl_data,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  scale_fill_viridis_c(option = "turbo", trans = "sqrt",limits=c(38,58))+
  geom_sf(data=land,fill="antiquewhite")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=.5)+
  #scale_color_manual(values = c("violetred4","darkorange3","violetred2","violetred2","violetred2","violetred2","darkorange1","darkorange1","darkorange1","darkorange1"))+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57),color="black")+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  #geom_label(data=inc_sens_labs,aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"))+
  coord_sf(xlim = c(-85, -65), ylim = c(16, 28), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination Angle")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = alpha('lightskyblue1', 0.5)),
        panel.border=element_rect(color="black",fill=NA,size=1),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  annotation_scale(location = "br", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map_inc2

ggsave(cond_map_inc2,dpi=300, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/inc_sens_distance_12-1-22.tiff",bg = "white")

cond_map_inc3 <- ggplot() +
  geom_sf(data=land,fill="antiquewhite")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=.5)+
  #geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  #geom_point(data=inc_sens,aes(x=long,y=lat,color=location),size=8)+
  scale_color_manual(values = c("violetred4","darkorange3","violetred2","violetred2","violetred2","violetred2","darkorange1","darkorange1","darkorange1","darkorange1"))+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57),color="black")+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=40,color="red")+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  #geom_label(data=inc_sens_labs,aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"))+
  coord_sf(xlim = c(-85, -65), ylim = c(16, 28), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = alpha('lightskyblue1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map_inc3

ggsave(cond_map_inc3,dpi=300, width = 8, height=5,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/inc_sens_justisolines_12-1-22.tiff",bg = "white")


avging<- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/MagneticFieldInfo/AveragingExperiment2021Dots.csv",header=T)

avging$location <- factor(avging$location,levels=c("Turks & Caicos","Haiti","TC+1","TC-1","HT+1","HT-1"))

cond_map_inc3 <- ggplot() +
  geom_sf(data=land,fill="antiquewhite")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=.5)+
  #geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=avging,aes(x=long,y=lat,color=location),size=10)+
  scale_color_manual(values = c("violetred4","chartreuse4","violetred2","violetred2","chartreuse3","chartreuse3"))+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57),color="black")+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=4,fontface="bold",family=c("Helvetica"),color="black")+
  #geom_label(data=subset(labels2,year=="2020fa"),aes(x=long,y=lat,label=label),size=6,fontface="bold",family=c("Helvetica"))+
  coord_sf(xlim = c(-85, -65), ylim = c(16, 28), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = alpha('lightskyblue1', 0.5)),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Helvetica"),
        axis.text = element_text(color="black",size=12,family="Helvetica"),
        plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1,size=10),
        axis.text.y = element_text(size=10))+
  #annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())# bg of the plot

cond_map_inc3

ggsave(cond_map_inc3,dpi=300, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/Updated_Figures/averaging2021.tiff",bg = "white")


###########Inclination maps

incl_data <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_data_2015.csv",skip=13,header=F)

names(incl_data) <- c("dec.date","lat","long","elev","incl","incl_del")

wmm_incl_data <- read.csv("C:/Users/kkmgo/OneDrive - University of North Carolina at Chapel Hill/Documents/JCP Magnetic Maps Review/WMM_WORLD_GRID_INCL.csv",skip=16,header=F)
names(wmm_incl_data) <- c("dec.date","lat","long","elev","incl","incl_del","na")

wmm_int_data <- read.csv("C:/Users/kkmgo/OneDrive - University of North Carolina at Chapel Hill/Documents/JCP Magnetic Maps Review/WMM_WORLD_GRID_INT.csv",skip=16,header=F)
names(wmm_int_data) <- c("dec.date","lat","long","elev","int","int_del","na")

detach(package:raster)
library(dplyr)
incl_data <- incl_data %>% select(-c("elev","incl_del"))

int_data <- read.csv("C:/Users/kkmgo/OneDrive - University of North Carolina at Chapel Hill/Documents/JCP Magnetic Maps Review/florida_intensity_2015.csv",skip=14,header=F)

names(int_data) <- c("dec.date","lat","long","elev","int","int_del")

int_data <- int_data %>% select(-c("elev","int_del"))

incl_data_15<- incl_data

library(plyr)
#incl_data_15$incl <- round_any(incl_data_15$incl,0.5,floor)

#wmm_incl_data$incl <- round_any(wmm_incl_data$incl,10)

incl_data_col$incl<-round_any(incl_data_col$incl,1)

incl_data2 <- incl_data %>% mutate(lat=round_any(lat,1)) %>% mutate(long=round_any(long,1))

incl_data2 <- incl_data2 %>% distinct()

#write.csv(incl_data2,"C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_polygons.csv")

incl_data2 <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_polygons.csv",header=T)

int_data <- int_data %>% mutate(int=int/1000)

int_data$int <- round_any(int_data$int,1)

incl_data3<- incl_data %>% arrange(incl)

class(incl_data$incl)

class(int_data$int)
class(incl_data$lat)
class(incl_data$long)

incl_data$lat<- as.numeric(incl_data$lat)
incl_data$long<- as.numeric(incl_data$long)

# download the data 
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
              destfile = 'coastlines.zip')


# unzip the file
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')

download.file("https://www.naturalearthdata.com/downloads/10m/physical/ne_10m_land.zip",destfile = 'land.zip')

#unzip(zipfile = "land.zip", 
#     exdir = 'ne_10m_land')

land <- st_read("C:/Users/kkmgo/OneDrive/Documents/ne_10m_land/ne_10m_land.shp")

coastlines <- st_read("C:/Users/kkmgo/OneDrive/Documents/ne-coastlines-10m/ne_10m_coastline.shp")

fl_points <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/points_florida_rookeries.csv")

fl_points_alt <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/points_florida_rookeries_alt.csv")

class(fl_points$group)

fl_points$group <- as.factor(as.character(fl_points$group))

cc1 <- fl_points %>% filter(group=="A")
cc2 <- fl_points %>% filter(group=="B")
cc3 <- fl_points %>% filter(group=="C")
cc4 <- fl_points %>% filter(group=="D")

fl_points$long <- as.numeric(fl_points$long)
fl_points$lat <- as.numeric(fl_points$lat)

fl_points_alt$long <- as.numeric(fl_points_alt$long)
fl_points_alt$lat <- as.numeric(fl_points_alt$lat)
library(raster)

incl_data_col2 <- incl_data_15 %>% filter(lat <= 35 & lat >= 23)

incl_data_col2 <- incl_data_col2 %>% filter(long >= -88 & long <= -76)

incl_data_18 <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_data_2018.csv",skip=13,header=F)

names(incl_data_18) <- c("dec.date","lat","long","elev","incl","incl_del")

incl_data_col3 <- incl_data_18 %>% filter(lat <= 35 & lat >= 23)

incl_data_col3 <- incl_data_col3 %>% filter(long >= -88 & long <= -76)


library(plyr)
incl_data_18b <- incl_data_18
incl_data_18b$incl <- round_any(incl_data_18b$incl,1,floor)

world <- ggplot() +
  geom_sf(data=land,fill="white")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-72, 30), ylim = c(-20, 60), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "grey88"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"))+
  #axis.text.x = element_blank(),
  #axis.ticks.x = element_blank())+
  #coord_equal()+
  scale_y_continuous(labels = abs)+
  scale_x_discrete(labels= abs)+
  annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
world
#library(directlabels)



#library(plyr)
#incl_data_col2$incl <- round_any(incl_data_col2$incl,5,floor)
library(scales)
incl <- ggplot() +
  #geom_raster(data=incl_data_col3,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  
  #scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  #geom_smooth(data=incl_data_15,aes(x=long,y=lat,group=incl),color="black",span=1,se=F)+
  #geom_contour(data=incl_data_18,aes(x=long,y=lat,z=incl),breaks = seq(50, 65, by = 1),color="black")+
  geom_sf(data=land,fill="white")+
  geom_sf(data = coastlines, fill="grey56",color="black",size=.5)+
  coord_sf(xlim = c(-88, -74), ylim = c(22, 35), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "grey86"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.text.y=element_blank())+
        #axis.ticks.x = element_blank())+
    
        #axis.ticks.x = element_blank())+
  #coord_equal()+
  scale_y_continuous(labels = abs)+
  scale_x_discrete(labels= abs)+
  annotation_scale(location = "tl", width_hint = 0.25,text_face = "bold",text_family = "Arial")

#library(directlabels)
incl

ggsave(incl,dpi=600, width = 4, height=4,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/florida_V2_11-15-21.tiff",device='tiff')

  
inc3 <- incl + 
  geom_point(data=cc1,aes(x=long,y=lat),color="navyblue",size=16,shape=15)+
  geom_point(data=cc2,aes(x=long,y=lat),color="maroon2",size=16,shape=16)+
  geom_point(data=cc3,aes(x=long,y=lat),color="springgreen4",size=16,shape=17)
  #geom_point(data=cc4,aes(x=long,y=lat),color="magenta3",size=9,shape=18)
inc3
#geom_scatterpie(aes(x=long,y=lat,group = location,r=1),data=fl_points, cols = c("A","B","C"), long_format = F)

fl <- ggplot() +
  #geom_raster(data=incl_data_col3,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  
  #scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  #geom_smooth(data=incl_data_15,aes(x=long,y=lat,group=incl),color="black",span=1,se=F)+
  #geom_contour(data=incl_data_18,aes(x=long,y=lat,z=incl),breaks = seq(50, 65, by = 1),color="black")+
  geom_sf(data=land,fill="white")+
  geom_sf(data = coastlines, fill="grey56",color="black",size=.5)+
  coord_sf(xlim = c(-88, -70), ylim = c(18, 35), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#B5E0F8"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank())+
  #axis.ticks.x = element_blank())+
  
  #axis.ticks.x = element_blank())+
  #coord_equal()+
  scale_y_continuous(labels = abs)+
  scale_x_discrete(labels= abs)+
  annotation_scale(location = "tl", width_hint = 0.25)

fl

ggsave(fl,dpi=600, width = 4, height=4,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/florida_V3_11-17-21.tiff",device='tiff')


library(scatterpie)
#detach(package:scatterpie)
detach(package:sp)
library(ggnewscale)
incl2 <- incl+
  new_scale_fill()+ 
  geom_scatterpie(aes(x=long,y=lat,group = location,r=.5),data=fl_points_alt, cols = c("CC.A1","CC.A2","Other"), long_format = F)+
  scale_fill_manual(values=c("darkblue","cyan4","skyblue2"),labels=c("CC-A1","CC-A2","Other"))+
  labs(fill="Haplotype")
#test
incl2
ggsave(inc3,dpi=800, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/genetics_fig_V2_11-15-21.tiff",device='tiff')
ggsave(incl2,dpi=600, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/genetics_fig2.png")

devtools::install_github("GuangchuangYu/scatterpie")

library(metR)


incl <- ggplot() +
  geom_raster(data=incl_data_15,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  scale_fill_viridis_c(option = "turbo", trans = "sqrt",limits=c(49,68))+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  #geom_smooth(data=incl_data_15,aes(x=long,y=lat,group=incl),color="black",span=1,se=F)+
  geom_contour(data=incl_data_15,aes(x=long,y=lat,z=incl),breaks = seq(35, 65, by = 0.5),color="black")+
  geom_sf(data=land,fill="grey42")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
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
  
#library(directlabels)
incl
#direct.label(incl, method="bottom.pieces")

ggsave(incl,dpi=800, width = 2, height=2,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_2015_11-15-21.tiff",device='tiff')


incl_wmm <- ggplot() +
  geom_raster(data=wmm_incl_data,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  geom_sf(data=world,fill="#B7B8BB",color="transparent")+
  #scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(10,-10,-20,20,30,-30,40,-40,50,-50,60,-60,70,-70,80,-80),color="black")+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl), breaks=0,color = "red")+
  #geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl), breaks=60,color = "red")+
  #geom_text_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl),breaks=c(0,-20,20,40,40,60,-60,80,-80),stroke=0.2,label.placement = label_placement_flattest())+
  #geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-180, 180), ylim = c(-70, 70), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
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

incl_wmm2 <- ggplot() +
  geom_raster(data=wmm_incl_data,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  geom_sf(data=world,fill="#B7B8BB",color="transparent")+
  scale_fill_viridis_c(option = "turbo",direction = -1)+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z=incl),breaks=c(0,10,-10,-20,20,30,-30,40,-40,50,-50,60,-60,70,-70,80,-80),color="black")+
  geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl), breaks=0,color = "red")+
  #geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl), breaks=60,color = "red")+
  #geom_text_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl),breaks=c(0,-20,20,40,40,60,-60,80,-80),stroke=0.2,label.placement = label_placement_flattest())+
  #geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-180, 180), ylim = c(-80, 80), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination (deg)")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.border = element_rect(color="black",fill="transparent"),
    panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#B5E0F8",color="black"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"))+
  scale_x_continuous(labels = abs)+
  scale_y_continuous(labels=abs)
#annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
#geom_text(data=wmm_incl_data,aes(x=0,y=0,z=0),label="Magnetic Equator")
incl_wmm2

ggsave(incl_wmm,dpi=800, width = 14, height=6,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_2021_wmm_V1_11-15-21.tiff",device='tiff')

ggsave(incl_wmm2,dpi=800, width = 14, height=6,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_2021_wmm_V2_11-15-21.tiff",device='tiff')


int_wmm <- ggplot() +
  #geom_raster(data=wmm_int_data,aes(x=long,y=lat,group=int,fill=int),color = '#00000000',interpolate = T)+
  geom_sf(data=world,fill="#B7B8BB",color="transparent")+
  #scale_fill_viridis_c(option = "turbo",direction=-1)+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  geom_contour(data=wmm_int_data,aes(x=long,y=lat,z=int),breaks=c(30000,35000,40000,45000,50000,55000,60000),color="black")+
  #geom_contour(data=wmm_int_data,aes(x=long,y=lat,z = int), breaks=0,color = "red")+
  #geom_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl), breaks=60,color = "red")+
  #geom_text_contour(data=wmm_incl_data,aes(x=long,y=lat,z = incl),breaks=c(0,-20,20,40,40,60,-60,80,-80),stroke=0.2,label.placement = label_placement_flattest())+
  #geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-180, 180), ylim = c(-70, 70), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Intensity (nT)")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
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

ggsave(int_wmm,dpi=800, width = 14, height=6,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/intensity_2021_wmm_V1_11-15-21.tiff",device='tiff')


###############Conditioning Map Fall 2020

cond_sites_fa20 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_lat_long_fa2020.csv",header=T)


#cond_sites_19 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/ForR_lat_long_2019.csv",header=T)

labels_fa20 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_location_labels_fa2020.csv",header=T)


ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=cond_sites_fa20,aes(x=long,y=lat,size=3,color=location))+
  scale_color_manual(values = c("black", "black","springgreen","goldenrod","red3","dodgerblue4"))+
  geom_text(data=labels_fa20,aes(x=long,y=lat,label=label),size=3,fontface="bold",color="black")+
  coord_sf(xlim = c(-85, -45), ylim = c(5, 40), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightskyblue1"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=11,family="Calibri"),axis.text = element_text(color="black",size=11),plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(hjust=1,angle=45))+
  annotation_scale(location = "br", width_hint = 0.25)

  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)


ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=cond_sites_19,aes(x=long,y=lat,size=10,color=location))+
  scale_color_manual(values = c("black","darkgoldenrod","springgreen4"))+
  geom_text(data=labels_19,aes(x=long,y=lat,label=label),size=6,fontface="bold",color="black")+
  coord_sf(xlim = c(-85, -58), ylim = c(15, 48), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightskyblue1"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=14,family="Calibri"),axis.text = element_text(color="black",size=12,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)



olive_beaches <- read.csv("C:/Users/kkmgo/Dropbox/Feasibility/olives_beaches_lat_long.csv",header=T)

ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=olive_beaches,aes(x=long,y=lat),size=3)+
  #scale_color_manual(values = c("black","black","black","black","black","black","black","black"))+
  #geom_text(data=labels_19,aes(x=long,y=lat,label=label),size=6,fontface="bold",color="black")+
  coord_sf(xlim = c(-100, -50), ylim = c(-4, 30), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightskyblue1"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=14,family="Calibri"),axis.text = element_text(color="black",size=12,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)

bhi<- read.csv("C:/Users/kkmgo/Dropbox/Biol395_Data/Isabel/BHI_point.csv")

sargassosea_map<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=bhi,aes(x=long,y=lat),size=5,color="red")+
  #scale_color_manual(values = c("black", "#990000","darkorchid","#FF9933","midnightblue","grey55","olivedrab","turquoise4","lightpink3"))+
  #geom_text(data=labels,aes(x=long,y=lat,label=label),size=3,fontface="bold",color="black")+
  #geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=3,fontface="bold",color="grey60")+
  coord_sf(xlim = c(-90, 8), ylim = c(0, 60), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightsteelblue1"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Calibri"),axis.text = element_text(color="black",size=11,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)) # bg of the plot

sargassosea_map

ggsave(sargassosea_map, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Biol395_Data/Isabel/sargassosea_map.png",  bg = "transparent")





