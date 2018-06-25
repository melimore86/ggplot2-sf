# Load all of the libraries listed, somce might not load because they aren't installed, use install.packages
library(rpostgis)
library(sp)
library(sf)
library(ggplot2)
library(scales)
library(devtools)
library(rgdal)
library(maps)
library(maptools)
library (ggmap)
library (mapdata)
library (ggsn)
library (GISTools)
library (prettymapr)
library(rworldmap)
library(rworldxtra)
library(tmap)
library (tmaptools)
library (grid)
library(legendMap)
library(raster)


# Connect to database
conn <- dbConnect("PostgreSQL", host = "basille-flrec.ad.ufl.edu",
                  dbname = "wood_stork_tracking", user = "mapper", password = "postgis")

# Get data from database (this can take a while)
traj <- pgGetGeom(conn, name = c("main","full_trajectories"))

# Plot the data
windows()
plot(traj, col = traj$animal_id)

head(traj)

#class       : SpatialLinesDataFrame
#features    : 6
#extent      : -86.958, -79.21517, 25.3225, 33.69933  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs
#variables   : 2
#names       : animal_id,         length_m
#min values  :    407680, 4368470.29749694
#max values  :    414250, 27561876.3860248

# Using st_as_sf we are converting the sp trajectories to sf

traj1<- st_as_sf (traj)
glimpse
#Observations: 133
#Variables: 3
#$ animal_id <int> 407680, 407700, 407710, 407800, 414240, 414250, 414260, 414270, 414...
#$ length_m  <dbl> 7823354.6, 27561876.4, 4368470.3, 12395973.3, 25587006.3, 8373455.6...
#$ geometry  <simple_feature> LINESTRING(-81.64483 30.404..., LINESTRING(-81.651 29.44...

# For ggplot2s to work with sf objects, all objects must be in sf format
worldmap <- getMap(resolution = "high")
map1<- sf::st_as_sf(worldmap )
windows()
plot(map1)

# Layers are added one at a time, so the order is important
windows()

ggplot() +
    geom_sf(data = map1) +
  
    geom_sf(data = traj1, aes(colour = factor(animal_id))) +
  
    ## scale_colour_grey(traj1, start = 0.2, end = 0.8, na.value = "lightgrey") +
  
    coord_sf(xlim = c(-100.95, -79.21), ylim = c(7.65, 33.97), expand = TRUE) +
  
    ggtitle("Wood Stork Movements", subtitle = NULL) +
  
    scale_bar(lon = -99.66, lat = 8, distance_lon = 500, distance_lat = 50, distance_legend = 100, dist_unit = "km", arrow_length = 150, arrow_distance = 300, arrow_north_size = 6, orientation = TRUE, legend_size = 2) +
  
    xlab("Longtitude") + ylab("Latitude") +
  
    theme_classic() +
  
    theme(legend.position = "none", panel.grid.major = element_line(colour = gray(.5), linetype = "dashed", size = 0.5))

geom_text(mapping=NULL, data=maptext, stat="identity", position="identity", size=3, inherit.aes=TRUE, label=label, x=-90, y=25, parse=FALSE)




