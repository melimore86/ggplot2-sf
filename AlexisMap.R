# This map will be Alexis map with a subplot zoom of the observation sites
devtools::install_github("3wen/legendMap")
devtools::install_github("dgrtwo/gganimate")
library(sp)
library(sf)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maps)
library(maptools)
library(rworldmap)
library(rworldxtra)
library(devtools)
library(legendMap)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggmap)
library(plyr)
library(Rcpp)
library (leaflet)
library(cowplot)
library(ggthemes)
library(gridExtra)
library(OpenStreetMap)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
library(rJava)

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)



#### Need to upload the layers for the map

roadsPB<-readOGR(dsn="Z:/Groups/basille_lab/mapping/wood_storks_fieldsites/GIS_data", layer="roads_PBcounty")

PBwetlands<-readOGR(dsn="Z:/Groups/basille_lab/mapping/wood_storks_fieldsites/GIS_data", layer="wetlands_PBcounty")

roadsFL<-readOGR(dsn="Z:/Groups/ecored/GIS_Data_Library/Road/Roads",layer="local_streents")

protectedFL<-readOGR(dsn="Z:/Groups/basille_lab/Data/Florida/SE_protected_areas",layer="PAD_SE_all")

flinterstates<-readOGR(dsn="Z:/Groups/ecored/GIS_Data_Library/Road/Roads",layer="interstates")

lakeo<-readOGR(dsn="Z:/Groups/basille_lab/Data/Florida/Water_Bodies", layer="lake_okeechobee")

streams<-readOGR(dsn="Z:/Groups/basille_lab/Data/Florida/Water_Bodies", layer="rivers_streams")

flboundary<-readOGR(dsn="Z:/Groups/ecored/GIS_Data_Library/Boundary/FL_County", layer="FL")

#flnat<-readOGR(dsn="D:/Basille Lab/florida_natural",layer="florida_natural")

#flwater<-readOGR(dsn="D:/Basille Lab/florida_water",layer="florida_water")

#flstparks<-readOGR(dsn="D:/Basille Lab/stpark_dec15",layer="stpark_dec15")

#flmjrivers<-readOGR(dsn="D:/Basille Lab/mjrivp",layer="mjrivp")

#flmjwater<-readOGR(dsn="D:/Basille Lab/mjwaterbnd",layer="mjwaterbnd")

#flmjwater<-readOGR(dsn="D:/Basille Lab/mjwaterbnd",layer="mjwaterbnd")

#flparksandrec<-readOGR(dsn="D:/Basille Lab/gc_parksbnd_jul16",layer="gc_parksbnd_jul16")



#### Changing to WGS84 which is espg 4326, to match with the other layers in our map


#flstparks2<-spTransform(flstparks, CRS("+init=epsg:4326"))

#flmjrivers2<-spTransform(flmjrivers, CRS("+init=epsg:4326"))

#flmjwater2<-spTransform(flmjwater, CRS("+init=epsg:4326"))

#flparksandrec2<-spTransform(flparksandrec, CRS("+init=epsg:4326"))

roadsPB2<-spTransform (roadsPB, CRS("+init=epsg:4326"))

flinterstates2<-spTransform(flinterstates, CRS("+init=epsg:4326"))

roadsFL2<-spTransform(roadsFL, CRS("+init=epsg:4326"))

protectedFL2<-spTransform(protectedFL, CRS("+init=epsg:4326"))

lakeo2<-spTransform(lakeo, CRS("+init=epsg:4326"))

streams2<-spTransform(streams, CRS("+init=epsg:4326"))

PBwetlands2<-spTransform(PBwetlands, CRS("+init=epsg:4326"))


##### creating a sample data.frame with your lat/lon points
lon <- c(-80.144005,-80.109)
lat <- c(26.479005, 26.830)
df <- as.data.frame(cbind(lon,lat))


#####Getting the map data for the ggplot
worldmap <- getMap(resolution = "high")

map1<- sf::st_as_sf(worldmap )

##### Adding custom named cities for the map

FL<- map_data("state", region="florida")

flcities <- data.frame(State=rep("florida",7), 
                         City=c("Miami", "Fort Lauderdale", "Orlando", "Gainesville", "Tallahassee", "Tampa","Jacksonville"))
                         
flcities<-cbind(geocode(as.character(flcities$City)),flcities)


##### Creating our map
#map_plot<-
  
windows()
  ggplot()+
  
  geom_sf(data=map1, fill="antiquewhite1") +
  
  coord_sf(xlim = c(-87.35, -79.50), ylim = c(24.10, 30.80), expand=TRUE ) +
  
  geom_polygon(data=lakeo2,colour= "lightblue1",fill="lightblue1", aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_path(data=streams2,colour= "lightblue2", aes(x= long, y=lat, group=group),inherit.aes=TRUE) +

  geom_polygon(data=protectedFL2, fill= "darkseagreen", colour= "darkseagreen", aes(x= long, y=lat, group=group, alpha=0.5),inherit.aes=TRUE) +

  geom_path(data=roadsFL2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="gray54", size=.35) +
  
  geom_path(data=flinterstates2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="gray18", size=.60) +
  
  geom_polygon(data= flboundary, fill= NA, colour= "lemonchiffon4", aes(x=long, y=lat, group=group),inherit.aes=TRUE, size=.76) + 
  
 geom_text(aes(x=lon, y=lat, label = City), data = flcities, size = 3.9, inherit.aes = TRUE, nudge_y =.18, col = "black", check_overlap = TRUE,fontface = "bold") +

  #geom_point(data=df, aes(x = lon, y = lat, fill = "darkred"), size = 3.2, shape = 21, inherit.aes = TRUE) +
  
  #annotate(geom="text", x=-80.00, y=26.580, label= "Wakodahatchee", color="black",size=3.1, fontface = "bold", hjust = 0) +
 
  #annotate(geom="text", x=-80.00, y=26.931, label= "BallenIsles", color="black", size= 3.1,fontface = "bold",hjust = 0) +
 
  #ggtitle ("Wood Stork Observation Sites") + 
 
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
 
  xlab("Longitude")+ ylab("Latitude") +
    
  theme_classic() + 
  
  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5),panel.background = element_rect(fill = "aliceblue"), panel.border=element_rect(fill=NA)) +
  
  scale_bar(lon = -87.00, lat =24.50,distance_lon = 100, distance_lat = 15, distance_legend = 50, dist_unit = "km", arrow_length=50, arrow_distance=65, arrow_north_size =5, orientation= TRUE,legend_size=2.5) 

  ggsave("AlexisMainFLMap.png", plot = last_plot(), device = "png", path = NULL,
         scale = 1, width = 7, height = 7,
         dpi = 300)
  

windows()
plot(map_plot)


#### Need to create another map for the ggplot grob function

map_plot1<-
  
  ggplot()+
  
  geom_sf(data=map1, fill="antiquewhite1") +
  
  coord_sf(xlim = c(-87.35, -78.97), ylim = c(23.81, 31.00), expand=TRUE ) +
  
  #geom_polygon(data=lakeo2,colour= "lightblue1",fill="lightblue1", aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_path(data=streams2,colour= "lightblue2", aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  #geom_polygon(data=protectedFL2, fill= "darkseagreen", colour= "darkseagreen", aes(x= long, y=lat, group=group, alpha=0.5),inherit.aes=TRUE) +
  
  #geom_path(data=roadsFL2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="gray54", size=.01) +
  
  #geom_path(data=flinterstates2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="gray44", size=.18) +
  
  #geom_polygon(data= flboundary, fill= NA, colour= "lemonchiffon4", aes(x=long, y=lat, group=group),inherit.aes=TRUE, size=.76) + 
  
  geom_text(aes(x=lon, y=lat, label = City), data = flcities, size = 2.3, inherit.aes = TRUE, nudge_y =.18, col = "black", check_overlap = TRUE, fontface = "bold") +
  
  geom_point(data=df, aes(x = lon, y = lat, fill = "darkred"), size = 2, shape = 21, inherit.aes = TRUE) +
  
  #annotate(geom="text", x=-79.40, y=26.479, label= "Wakodahatchee Island", color="black",size=1.5) +
  
  #annotate(geom="text", x=-79.60, y=26.830, label= "Ballenisles", color="black", size= 1.5) +
  
  #ggtitle ("Wood Stork Observation Sites") + 
  
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  
  labs(x = NULL, y = NULL) +
  
  theme_classic() + 
  
  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5),panel.background = element_rect(fill = "aliceblue"), panel.border=element_rect(fill=NA),text=element_text(size=8)) +
  
  scale_bar(lon = -85.9, lat =25.2,distance_lon = 100, distance_lat = 25, distance_legend = 50, dist_unit = "km", arrow_length=50, arrow_distance=65, arrow_north_size =5, orientation= TRUE,legend_size=2) 

windows()
plot(map_plot1)


####Subplotting the observation sites for the map, this is the fist observation site

#site1<- 
  
  ggplot() +
  
  geom_sf(data=map1, fill="antiquewhite1") +
  
  coord_sf(xlim = c(-80.30, -80.00 ), ylim = c(26.63, 26.9), expand=TRUE ) +
  
  geom_polygon(data=PBwetlands2,colour= "darkseagreen",fill=NA, aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_path(data=streams2,colour= "lightblue2", aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_path(data=roadsPB2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="gray51",size=.40) +
  
  geom_polygon(data=protectedFL2, fill= "darkseagreen", colour= "darkseagreen", aes(x= long, y=lat, group=group,alpha=0.8),inherit.aes=TRUE) +
  
  geom_path(data=flinterstates2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="black", size=.60) +
  
  geom_point(data = df, aes(x = lon, y = lat, fill = "darkred"), size = 3.5, shape = 21) +
  
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  
  ggtitle ("BallenIsles") + 
 
   scale_x_continuous(expand=c(0,0)) +
  
  scale_y_continuous(expand = c(0,0)) +
  
  labs(x = NULL, y = NULL) +
  
  theme_map() +

  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5),panel.background = element_rect(fill = "aliceblue"), panel.border=element_rect(fill=NA), text=element_text(size=10, face="bold"))
  
  ggsave("AlexisBallenIsles.png", plot = last_plot(), device = "png", path = NULL,
         scale = 1, width = 7, height = 7,
         dpi = 300)

windows()
plot(site1)



#### This is the next subplot site for the map, observation site 2


#site2<- 
  
  ggplot() +
  
  geom_sf(data=map1, fill="antiquewhite1") +
  
  coord_sf(xlim = c(-80.30, -80.00 ), ylim = c(26.62, 26.35), expand=TRUE ) + 
  
  geom_polygon(data=PBwetlands2,colour= "darkseagreen",fill=NA, aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_path(data=streams2,colour= "lightblue2", aes(x= long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_path(data=roadsPB2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="gray51", size=.40) +
  
  geom_polygon(data=protectedFL2, fill= "darkseagreen", colour= "darkseagreen", aes(x= long, y=lat, group=group, alpha=0.8),inherit.aes=TRUE) + 
  
  geom_path(data=flinterstates2, aes(x=long, y=lat, group=group),inherit.aes=TRUE, col="black", size=.60) +
  
 geom_polygon(data= flboundary, fill= NA, colour= "lemonchiffon4", aes(x=long, y=lat, group=group),inherit.aes=TRUE, size=.76) +
  
  geom_point(data = df, aes(x = lon, y = lat, fill = "darkred"), size = 3.5, shape = 21) +
  
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  
  ggtitle ("Wakodahatchee") +
  
  scale_x_continuous(expand = c(0,0)) +
  
  scale_y_continuous(expand = c(0,0)) +
  
  labs(x = NULL, y = NULL) +
  
  theme_map() +
  
  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5),panel.background = element_rect(fill = "aliceblue"), panel.border=element_rect(fill=NA), text= element_text(size=10, face="bold"))

ggsave("AlexisWakodahatchee.png", plot = last_plot(), device = "png", path = NULL,
       scale = 1, width = 7, height = 7,
       dpi = 300)


windows()
plot(site2)



#### The ggplot+sf was not allowing me to plot the sub maps I previously created, used print function instead
# This will allow you to print the maps on the larger map_plot

vp <- viewport(width = 0.37, height = 0.32, x = 0.08, y =0.47, just = c("left","bottom"))  
    
vp1<- viewport(width = 0.37, height = 0.32, x = 0.08, y =0.17, just = c("left","bottom"))  

windows()
png("PrintMapping.png",width = 7, height = 7, units = "in", res = 300)
print(map_plot)
print(site1, vp=vp)
print(site2, vp=vp1)
dev.off()

# Normally we can save this with a ggsave function, but we don't have a last map, per se, we have one map with 2 plots printed on it

ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 7, height = 7,
       dpi = 300)

# Because we are not saving 1 plot, we are saving 3 plots with 2 printed on the first map, we need to save the file using 

png("PngMap.png", width = 1500, height = 1500); print(map_plot); print(site1, vp=vp); print(site2, vp=vp1); dev.off() 



#### Making side by side graphs with custom_annotation

list.plots=list(map_plot1,site1, site2)


#### Make sure to use list to save the "list.plots" in the correct object

align_three_plots <- function(list.plots, family = "")

  {
  
gg <- ggplot()+
 
   coord_equal(xlim = c(0, 80), ylim = c(0, 51), expand=TRUE) +
  
  annotation_custom(ggplotGrob(map_plot1), xmin = 0.0, xmax =57, ymin = 0.0, ymax = 50) +
  
  annotation_custom(ggplotGrob(site1),xmin = 58, xmax = 80, ymin = 25, ymax = 51) +
  
  annotation_custom(ggplotGrob(site2), xmin = 58, xmax = 80, ymin = 0.0, ymax = 29)# +
  
  #labs(x = NULL, y = NULL) +
 
  #theme_void()



#DF with the coordinates of the 2 arrows
df.arrows <- data.frame(id=1:2,
                        x=c(45.6,45.4),
                        y=c(21.7,19.8),
                        xend=c(58,58),
                        yend=c(35,12))


gg <- gg +
  geom_segment(data = df.arrows %>% filter(id==1),
             aes(x=x,y=y,xend=xend,yend=yend),
             arrow = arrow(type="closed",length = unit(0.25,"cm"))) +
  geom_segment(data = df.arrows %>% filter(id==2),
             aes(x=x,y=y,xend=xend,yend=yend),
             arrow = arrow(type="closed",length = unit(0.25,"cm")))

# add labes
#gg <- gg + annotate('text',label = labels,
#                    x=c(.5,12.5)+.5,
#                    y=c(29,27.5)+.1,
#                    size=labels.size,hjust=0, vjust=0, #family = family)

#return(gg)

}


# create a simple blank square plot, creating a clone, and then plotting the maps
p <- ggplot()+
  theme_map() +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA),aspect.ratio = 1)


# clone this plot three times and store as a list of six
plots <- mget(rep("p", 3))

three<- align_three_plots(plots)

windows()
plot(three)

save_plot("mymap.png",three)
save_plot("mymap_h7.png",three,base_height=7, base_width=7)

png(filename = "my_map_76.png", width = 7, height = 7, units = "in", res = 300)
plot(three)

dev.off()




memory.limit(size=50000)
