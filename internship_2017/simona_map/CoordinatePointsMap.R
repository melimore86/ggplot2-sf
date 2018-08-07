# In this code we will be plotting points on a FL map. 
# The points are very close together so we need to get a zoom in view
# We will be using ggplot2+ sf to creat the map

library(sp)
library (sf)
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(rworldmap)
library (rworldxtra)
library (legendMap)
library(tidyverse)

# creating a sample data.frame with your lat/lon points
lon <- c(-80.144,-80.109)
lat <- c(26.479, 26.830)
df <- as.data.frame(cbind(lon,lat))

# getting the map
fl_map <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 6, maptype = "satellite", scale = 2)

windows()
plot(fl_map)

# plotting the satellite map with some points on it
windows()
ggmap(fl_map) + 
  geom_point(data = df, aes(x = lon, y = lat, fill = "darkred"), size = 3.5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  xlab("Longitude")+ ylab("Latitude")+ theme_classic()+ ggtitle ("Wood Stork Observational Sites", subtitle= NULL)

# Going to make a map to compare between a satellite image and a non- satellite map
setwd("D:/Basille Lab")

points = read.table("flmajorcities.txt", header=T, sep="", quote="", dec=".") 


#Getting the map data for the ggplot
worldmap <- getMap(resolution = "high")
map1<- sf::st_as_sf(worldmap )

# Getting the states outlines using the map_data
states <- map_data("state")
head(states)

# Getting the county data for the map
counties <- map_data("county")
head(counties)
plot(counties)

windows()
ggplot()+
  geom_sf(data=map1, fill= "darkolivegreen3")+coord_sf(xlim = c(-89.15, -76.12), ylim = c(24.65, 33.97), expand=TRUE ) +
  
geom_polygon(data=counties, fill= NA, colour= "darkgreen", aes(x=long, y=lat, group=group),inherit.aes=TRUE) +
  
  geom_polygon(data=states, fill= NA, colour= "black", aes(x=long, y=lat, group=group),inherit.aes=TRUE, size=.76)+
  
  geom_point(data = df, aes(x = lon, y = lat, fill = "darkred", alpha = 0.9), size = 3.5, shape = 21) +
  
  ggtitle ("Wood Stork Observational Sites", subtitle = "2017") + 
  
  scale_bar(lon = -88.66, lat =25,distance_lon = 150, distance_lat = 25, distance_legend = 100, dist_unit = "km", arrow_length=75, arrow_distance=150, arrow_north_size =6, orientation= TRUE,legend_size=2) + 
  
  xlab("Longitude")+ ylab("Latitude") +
  
  theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue"), panel.border=element_rect(fill=NA))
