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

data(land)

st_crs(sites) <- st_crs(land)

tm_shape(land)+
  tm_shape(sites)

projection(sites)
 

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
  geom_sf(data=world,fill="chocolate4",color="black") +
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

cond_sites <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_lat_long_field_locations_2021.csv",header=T)

cond_sites$location  <- factor(cond_sites$location, levels=c("Acclimation","Nova Scotia","Bahamas","Long Island","Cuba","Florida","Massachusetts","Outer Banks","New Brunswick"))

#cond_sites_19 <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/ForR_lat_long_2019.csv",header=T)

labels <- read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/ForR_location_labels_2021_nofl.csv",header=T)

#labels_19 <-  read.csv("C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/2019/ForR_location_labels_2019.csv",header=T)

#cond_sites <- rbind(cond_sites_17,cond_sites_19)

#labels <- rbind(labels,labels_19)

#cond_sites_17 <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                  #crs = 4326, agr = "constant")

anno <- data.frame(label="Florida",
                   lat=27.9,
                   long=-78)
anno


cond_map<- ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=cond_sites,aes(x=long,y=lat,size=3,color=location))+
  scale_color_manual(values = c("black", "#990000","darkorchid","#FF9933","midnightblue","grey55","olivedrab","turquoise4","lightpink3"))+
  geom_text(data=labels,aes(x=long,y=lat,label=label),size=3,fontface="bold",color="black")+
  geom_text(data=anno,aes(x=long,y=lat,label="Florida"),size=3,fontface="bold",color="grey60")+
  coord_sf(xlim = c(-90, -45), ylim = c(8, 53), expand = FALSE)+
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

cond_map

ggsave(cond_map, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/map_of_sites4.png",  bg = "white")

ggsave(cond_map, dpi=300,width=10,height=8,units="in", filename = "C:/Users/kkmgo/Dropbox/Conditioning_MagFields_Project/Figures/map_of_sites5.png",  bg = "transparent")

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

ggplot(data = world) +
  geom_sf(fill="antiquewhite",color="black") +
  #geom_sf(data = sites, size = 1, shape = 16, fill = "black") +
  geom_point(data=cond_sites20,aes(x=long,y=lat,size=3,color=location))+
  scale_color_manual(values = c("black", "grey29","hotpink3","grey29","grey29","springgreen4","grey29","grey29"))+
  geom_text(data=labels20,aes(x=long,y=lat,label=label),size=3,fontface="bold",color="black")+
  coord_sf(xlim = c(-83, -40), ylim = c(25, 55), expand = FALSE)+
  labs(x="Longitude",y="Latitude")+
  #scale_fill_continuous(name="Zones")+
  guides(alpha=FALSE,size=FALSE,color=FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "lightcyan2"),axis.line = element_line(color="black"))+
  theme(axis.title=element_text(size=12,family="Calibri"),axis.text = element_text(color="black",size=11,family="Calibri"),plot.margin = unit(c(1,1,1,1),"cm"),axis.text.x = element_text(angle=45,hjust=1))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(.1, "in"), pad_y = unit(0.08, "in"),
                         style = north_arrow_fancy_orienteering)


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



###########Inclination maps

incl_data <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_data_2015.csv",skip=13,header=F)

names(incl_data) <- c("dec.date","lat","long","elev","incl","incl_del")

wmm_incl_data <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/WMM_WORLD_GRID_INCL.csv",skip=16,header=F)
names(wmm_incl_data) <- c("dec.date","lat","long","elev","incl","incl_del","na")

detach(package:raster)
library(dplyr)
incl_data <- incl_data %>% select(-c("elev","incl_del"))

int_data <- read.csv("C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/florida_intensity_2015.csv",skip=14,header=F)

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

land <- st_read("C:/Users/kkmgo/Documents/ne_50m_land/ne_50m_land.shp")

coastlines <- st_read("ne-coastlines-10m/ne_10m_coastline.shp")

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


#library(plyr)
#incl_data_col2$incl <- round_any(incl_data_col2$incl,5,floor)

incl <- ggplot() +
  #geom_raster(data=incl_data_col3,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  
  #scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  #geom_smooth(data=incl_data_15,aes(x=long,y=lat,group=incl),color="black",span=1,se=F)+
  #geom_contour(data=incl_data_18,aes(x=long,y=lat,z=incl),breaks = seq(50, 65, by = 1),color="black")+
  geom_sf(data=land,fill="white")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-88, -76), ylim = c(23, 35), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "grey88"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")

#library(directlabels)
incl
  
inc3 <- incl + 
  geom_point(data=cc1,aes(x=long,y=lat),color="navyblue",size=16,shape=15)+
  geom_point(data=cc2,aes(x=long,y=lat),color="maroon2",size=16,shape=16)+
  geom_point(data=cc3,aes(x=long,y=lat),color="springgreen4",size=16,shape=17)
  #geom_point(data=cc4,aes(x=long,y=lat),color="magenta3",size=9,shape=18)
inc3
#geom_scatterpie(aes(x=long,y=lat,group = location,r=1),data=fl_points, cols = c("A","B","C"), long_format = F)

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
ggsave(inc3,dpi=800, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/genetics_fig.png")
ggsave(incl2,dpi=600, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/genetics_fig2.png")

devtools::install_github("GuangchuangYu/scatterpie")

library(metR)


incl <- ggplot() +
  geom_raster(data=incl_data_col2,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
  
  scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
  #scale_fill_continuous(low = "blue", high = "red", na.value = NA)+
  #geom_smooth(data=incl_data_15,aes(x=long,y=lat,group=incl),color="black",span=1,se=F)+
  geom_contour(data=incl_data_15,aes(x=long,y=lat,z=incl),breaks = seq(50, 65, by = 0.5),color="black")+
  geom_sf(data=land,fill="grey42")+
  geom_sf(data = coastlines, fill="grey16",color="black",size=1)+
  coord_sf(xlim = c(-88, -76), ylim = c(23, 35), expand = FALSE)+
  labs(x="Longitude",y="Latitude",fill="Inclination")+
  guides(alpha=FALSE,size=FALSE,color=TRUE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "lightskyblue1"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
  
#library(directlabels)
incl
#direct.label(incl, method="bottom.pieces")

ggsave(incl,dpi=800, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_2015.png")


incl_wmm <- ggplot() +
  #geom_raster(data=incl_data_col2,aes(x=long,y=lat,group=incl,fill=incl),color = '#00000000',interpolate = T)+
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
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#B5E0F8",color="black"),
        axis.line = element_line(color="black"))+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black",size=11,family = "Arial"),
        plot.margin = unit(c(1,1,1,1),"cm"))
  #annotation_scale(location = "tl", width_hint = 0.5,text_face = "bold",text_family = "Arial")
  #geom_text(data=wmm_incl_data,aes(x=0,y=0,z=0),label="Magnetic Equator")
incl_wmm

ggsave(incl_wmm,dpi=800, width = 8, height=8,units="in",filename="C:/Users/kkmgo/Dropbox/JCP Magnetic Maps Review/inclination_2021_wmm_FINALCOLORS.png")


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

