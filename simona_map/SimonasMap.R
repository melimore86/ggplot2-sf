# Going to creat a map for Simona, showing the two wood storks that traveled to MX, instead of staying in FL

install.packages("mapdata")
install.packages("adehabitatLT")
library(sf)
library(sp)
library(ggplot2)
library(rworldmap)
library(rworldxtra)
library(legendMap)
library(mapdata)
library(adehabitatLT)
library(scales)
library(munsell)
library(rpostgis)
library(lubridate)
library(ggthemes)
library(cowplot)
library(RPostgreSQL)
library(dplyr)
library(gridExtra)
library(rgdal)
library(rgeos)
library(lattice)


### Now this would be to seperate the other storks to plot them 
gps_data_animals <-dbReadTable(conMS, c("main","gps_data_animals"))
gps <- gps_data_animals
gps$tag <- factor(gps$tag_id)
gps$id <- factor(gps$animal_id)

gps <- gps[,-c(3,4,11,12,13)]
gps <- gps[,c(1,2,9,10,3,4,5,6,7,8)]
gps <- droplevels(gps[order(gps$id, gps$acquisition_time), ])

length(unique(gps$id)) # 133 individuals 

setwd("E:/Basille Lab/SimonaForMel")

# Info on individuals
ind <- read.csv("TelemetryShouldEndDates_ForMathieu.csv", col.names = c("id", "wost", "age", "year", "cycle", "colony", "type", "capture", "date_start", "date_end", "status"))
ind$type <- NULL
ind$id <- factor(ind$id)

# Rename status
levels(ind$status) <- c("alive", "no dispersal", "no fledge", "good", "no fledge", "disappeared", "no functional tag")

# Keep only good and alive individuals
ind <- droplevels(subset(ind, status %in% c("good", "alive")))

# Rename colonies
ind$colony[ind$id != "992250" & ind$colony == "ENP"] <- "TamW"
levels(ind$colony)[5] <- "Paurotis"
levels(ind$colony)[4] <- "Gilman"

# Convert to lower case
levels(ind$capture) <- tolower(levels(ind$capture))

# Correct age level names
levels(ind$age) <- c("adult", "juvenile", "juvenile", "subadult")

# Set monitoring dates to POSIXt format
ind$date_start <- dmy(as.character(ind$date_start), tz = "EST5EDT")

# Correct typos
levels(ind$date_end)[levels(ind$date_end) == "didn't fledge"] <- NA
ind$date_end <- dmy(as.character(ind$date_end), tz = "EST5EDT")
ind$date_end[ind$id == "475192"] <- ymd("2006-05-26", tz = "EST5EDT")

# Sort by starting date
ind <- ind[order(ind$date_start), ]

# Compute start and ending dates for age classes
ind$date_subadult <- ceiling_date(ind$date_start, "year") + months(2)
ind$date_subadult[ind$age == "adult"] <- NA
ind$date_subadult[ind$age == "subadult"] <- ind$date_start[ind$age == "subadult"]
ind$date_subadult[ind$date_subadult > ind$date_end] <- NA

ind$date_adult <- ceiling_date(ind$date_start, "year") + months(2) + years(3)
ind$date_adult[ind$age == "subadult"] <- ceiling_date(ind$date_start[ind$age == "subadult"], "year") + months(2) + years(2)
ind$date_adult[ind$age == "adult"] <- ind$date_start[ind$age == "adult"]
ind$date_adult[ind$date_adult > ind$date_end] <- NA

length(ind$id) # 115. Some individuals are missing here



#### Managing locations spatially ####

gps <- gps[gps$gps_validity_code==1,]

coordinates(gps) <- gps[, c("longitude", "latitude")] # In order to keep longitude/latitude in the object
## Beware that calls to gps$longitude/latitude refers to the coordinates of the SPDF; 
## if needed, use gps@data$longitude/latitude to extract the coordinate stored in decimal degrees
proj4string(gps) <- CRS("+proj=longlat +datum=WGS84")
summary(gps)

EPSG <- make_EPSG()
EPSG[grep("(WGS 84|NAD83).*(UTM zone 17)", EPSG$note), ]

gps <- spTransform(gps, CRS("+init=epsg:4326"))
summary(gps)

# gps@data <- merge(gps@data, ind, by = "id") 
# Instead of merging I will use a left join, because some individuals that are in gps (n=133) are not in ind (n=115)
gps@data <- left_join(gps@data, ind, by = "id") 

gps@data$date <- gps@data$acquisition_time

gps@data$acquisition_time <- NULL
gps@data <- gps@data[,c(1:6,21,7:20)]

# Age classes

# Juveniles
gpsjuv <- subset(gps, age == "juvenile" & date < ifelse(!is.na(date_subadult), 
                                                        date_subadult, date_end))
gpsjuv@data <- droplevels(gpsjuv@data)

# Subadults
gpssub <- subset(gps, (age == "subadult" & date < ifelse(!is.na(date_adult), 
                                                         date_adult, date_end)) | (age == "juvenile" & date >= ifelse(!is.na(date_subadult), 
                                                                                                                      date_subadult, date_end + 1) & date < ifelse(!is.na(date_adult), 
                                                                                                                                                                   date_adult, date_end)))
gpssub@data <- droplevels(gpssub@data)

# Adults
gpsad <- subset(gps, date >= ifelse(!is.na(date_adult), date_adult, 
                                    #date_end + 1)) # using this (+1), I noticed that ind 475201 ended up having some points classified as adult on the last day of monitoring.
                                    # This is because +1 only adds 1 second. Let's do +86400 instead, so that it adds a day
                                    date_end + 86400))
gpsad@data <- droplevels(gpsad@data)

#### Exploration mexican storks ####

# Individual information
mex.info <- ind[ind$id %in% c(572970,475201),]
mex.info

# GPS data
mex <- gps[gps@data$id %in% c(572970,475201),]
mex@data <- droplevels(mex@data)

# Subset based on age classes
mexjuv <- subset(gpsjuv, id %in% c(572970,475201)) # only 572970 was there as a juvenile
mexsub <- subset(gpssub, id %in% c(572970,475201)) # both were there as subadults
mexad <- subset(gpsad, id %in% c(572970,475201)) # only 572970 was there as an adult
mexjuv@data <- droplevels(mexjuv@data)
mexsub@data <- droplevels(mexsub@data)
mexad@data <- droplevels(mexad@data)


stork4s <- subset(mexsub, id==475201)
stork5j <- subset(mexjuv, id==572970)
stork5s <- subset(mexsub, id==572970) 
stork5a <- subset(mexad, id==572970)
stork4s@data <- droplevels(stork4s@data)
stork5j@data <- droplevels(stork5j@data)
stork5s@data <- droplevels(stork5s@data)
stork5a@data <- droplevels(stork5a@data)


#Making the stork subsets into the correst espg
stork4s2<-spTransform(stork4s, CRS("+init=epsg:4326"))
stork5j2 <-spTransform(stork5j, CRS("+init=epsg:4326"))
stork5s2<-spTransform(stork5s, CRS("+init=epsg:4326"))
stork5a2<-spTransform(stork5a, CRS("+init=epsg:4326"))

# Need to make the subsets in an sf object to be plotted by ggplot
stork4s3<-st_as_sf(stork4s2)
stork5j3<-st_as_sf(stork5j2)
stork5s3<-st_as_sf(stork5s2)
stork5a3<-st_as_sf(stork5a2)



# World map and names
world <- getMap(resolution = "low")
world <- sf::st_as_sf(world)


# Mississippi river shape file

missiriver<-readOGR(dsn="E:/Basille Lab/LayersShapefiles/ne_110m_rivers_lake_centerlines",layer="ne_110m_rivers_lake_centerlines")

# Lake Okechobee for Florida

lakeo<-readOGR(dsn="Z:/Groups/basille_lab/Data/Florida/Water_Bodies", layer="lake_okeechobee")

lakeo2<-spTransform(lakeo, CRS("+init=epsg:4326"))


# Now to get the state border and state names

states <- map_data("state")

#cnames <- aggregate(cbind(long, lat) ~ region, data=states, 
                   #FUN=function(x)mean(range(x)))


# Need to uppercase the names of the states, need the tools package to use toTitleCase
#cnames$region<-toTitleCase(cnames$region)


# Creating points to make a star starting point, of the colonies

#Noxubee National Wildlife Refuge: lat 33.270996, long -88.783969 
#Chew Mill Pond: lat 32.8336, long -82.0928
lon <- c(-88.783969)
lat <- c(33.270996)
df <- as.data.frame(cbind(lon,lat))

lon2<-c(-82.0928)
lat2<-c(32.8336)
df2<- as.data.frame(cbind(lon2,lat2))


lon3<-c(-88.783969,-82.0928)
lat3<-c(33.270996,32.8336)
df3<-as.data.frame(cbind(lon3,lat3))

# Color blind friendly pallette

cbPalette <- c("#E69F00","#009E73","#F0E442") 

colonies<- c("#000000", "#56B4E9")

# Map for the legends

LegendMap<- 
  
  ggplot() +
  
  geom_sf(data = world, fill = "seashell1", col="grey28") +
  
  #geom_path(data = traj, aes(x = long, y = lat, group=group), col= "darkseagreen4", alpha=0.25, size=0.16) +

  coord_sf(xlim = c(-104.15, -76.12), ylim = c(11.20, 35.12), expand = TRUE) +
  
  geom_polygon(data=states, fill= NA, col= "grey32", aes(x=long, y=lat, group=group),inherit.aes=TRUE, alpha=0.3) +
  
  geom_polygon(data=lakeo, fill= "#56B4E9", col= "#56B4E9", aes(x=long, y=lat, group=group),inherit.aes=TRUE, alpha=0.7) +
  
  geom_path(data=missiriver,colour= "#56B4E9", aes(x= long, y=lat, group=group),inherit.aes=TRUE, size= 1.3, alpha=0.7) +
  
  
  
  geom_path(data=stork4s3, aes(x= longitude, y=latitude,colour = "A"),size=1.8, inherit.aes=TRUE, pch=20,  alpha=0.65) +
  
  geom_path(data=stork5j3, aes(x= longitude, y=latitude, colour = "B"),size=1.5, inherit.aes=TRUE, alpha=0.6,pch=15) + 
  
  geom_path(data=stork5s3, aes(x= longitude, y=latitude, colour = "C"),size=1.5, inherit.aes=TRUE,alpha=0.8,pch=15) + 
  
  #geom_path(data=stork5a3, aes(x= longitude, y=latitude,colour = "red4"), size=1.5, inherit.aes=TRUE,alpha=0.9,pch=15) + 
  

  
  #xlab("LONGITUDE") + ylab("LATITUDE") +
  
  ggtitle("",subtitle="") +
  
  #geom_text(aes(LON, LAT, label = NAME), data = world, size = 2.3, inherit.aes = TRUE, nudge_x = 0.0, col = "grey22", check_overlap = FALSE, fontface="bold") +
  
  #geom_text(aes(long, lat, label = region), data = cnames, size = 2.5, inherit.aes = TRUE, col = "grey22", check_overlap = TRUE, fontface="bold", nudge_x=.19, nudge_y=.35) +
    
  geom_point(data=df, aes(x = lon, y = lat, fill= "Noxubee NWR"), color= "#D55E00", size = 2.5, inherit.aes = TRUE, pch=19) +
  
  geom_point(data=df2, aes(x = lon2, y = lat2, fill="Chew Mill Pond"),color= "#0072B2", size = 2.5,inherit.aes = FALSE, pch=19) + 
  
    
 
  annotate(geom="text", x=-91.00, y=25.00, label= "Gulf of Mexico", color="grey22",size=4.5) +
  
  annotate(geom="text", x=-81.50, y=28.00, label= "Florida", color="grey22",size=2.1, fontface="bold") +
  
  #annotate(geom="text", x=-84.00, y=28.00, label= "Florida", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-99.00, y=31.50, label= "Texas", color="grey22",size=2.7, fontface="bold") +
  
  annotate(geom="text", x=-97.00, y=35.50, label= "Oklahoma", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-92.50, y=35.50, label= "Arkansas", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-92.50, y=31.50, label= "Louisiana", color="grey22",size=2.3, fontface="bold") +
  
  annotate(geom="text", x=-87.50, y=36.00, label= "Tennessee", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-86.70, y=33.5, label= "Alabama", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-83.70, y=33.5, label= "Georgia", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-79.90, y=36.00, label= "North Carolina", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-80.5, y=34.00, label= "South Carolina", color="grey22",size=2.3, fontface="bold") +
  
  annotate(geom="text", x=-89.55, y=34.00, label= "Mississippi", color="grey22",size=2.2, fontface="bold") +

  annotate(geom="text", x=-91.10, y=34.75, label= "Mississippi River", color="grey22",size=2.1, fontface="bold", angle=60) +
  
  
  annotate(geom="text", x=-102.10, y=24.00, label= "Mexico", color="grey22",size=2.9, fontface="bold") +
  
  annotate(geom="text", x=-90.50, y=15.5, label= "Guatemala", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-88.60, y=17.00, label= "Belize", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-86.30, y=15.20, label= "Honduras", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-85.30, y=12.50, label= "Nicaragua", color="grey22",size=2.7, fontface="bold") +
  
  annotate(geom="text", x=-79.50, y=22.35, label= "Cuba", color="grey22",size=2.7, fontface="bold", angle=340) +


    scale_bar(lon = -102.50, lat = 11.20,distance_lon = 250, distance_lat = 25, distance_legend = 100, dist_unit = "km", arrow_length = 200, arrow_distance = 200, arrow_north_size = 6, orientation = TRUE, legend_size = 2.0)+
  
    guides(fill=guide_legend(title="Capture Colonies", show=TRUE, override.aes=list(colour=c("#0072B2","#D55E00")))) +
  
  guides(color=guide_legend(title="    Age Classes    ", show=TRUE)) +
  
  scale_color_manual(labels=c( "Juvenile", "Sub Adult", "Adult"),values=c(cbPalette)) +
    
  
  theme_map() +

  theme(legend.position="right",legend.background = element_rect(fill=NA,linetype="solid", colour ="black"),panel.grid.major = element_line(colour = "azure", linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "azure"), panel.border=element_rect(fill=NA))

plot(LegendMap)
  

# Map of Stork ID 572970

Stork572970<- 
  
ggplot() +
  
  geom_sf(data = world, fill = "seashell1", col="grey28") +
  
  #geom_path(data = traj, aes(x = long, y = lat, group=group), col= "darkseagreen4", alpha=0.25, size=0.16) +
  
  coord_sf(xlim = c(-104.15, -76.12), ylim = c(11.20, 35.12), expand = TRUE) +
  
  geom_polygon(data=states, fill= NA, col= "grey35", aes(x=long, y=lat, group=group),inherit.aes=TRUE,alpha=0.4) +
  
  geom_polygon(data=lakeo, fill= "#56B4E9", col= "#56B4E9", aes(x=long, y=lat, group=group),inherit.aes=TRUE, alpha=0.7) +
  
  geom_path(data=missiriver,colour= "#56B4E9", aes(x= long, y=lat, group=group),inherit.aes=TRUE, size= 1.3, alpha=0.7) +
  
  
  geom_path(data=stork4s3, aes(x= longitude, y=latitude), color="#009E73", size=1.7, inherit.aes=TRUE,alpha=0.9,pch=15) + 
  
  
  #xlab("LONGITUDE") + ylab("LATITUDE") +
  
  ggtitle("B",subtitle="") +
  
  
  geom_point(data=df, aes(x = lon, y = lat), color= "#D55E00", size = 4.3, inherit.aes = TRUE, pch=19) +

  
  annotate(geom="text", x=-91.50, y=25.00, label= "Gulf of Mexico", color="grey22",size=4.5) +
  
  annotate(geom="text", x=-81.50, y=28.00, label= "Florida", color="grey22",size=2.1, fontface="bold") +
  
  #annotate(geom="text", x=-84.00, y=28.00, label= "Florida", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-99.00, y=31.50, label= "Texas", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-97.00, y=35.50, label= "Oklahoma", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-92.50, y=35.50, label= "Arkansas", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-92.50, y=31.50, label= "Louisiana", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-87.50, y=36.00, label= "Tennessee", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-86.70, y=33.5, label= "Alabama", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-83.70, y=33.5, label= "Georgia", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-79.75, y=36.00, label= "North Carolina", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-80.5, y=34.00, label= "South Carolina", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-89.55, y=34.00, label= "Mississippi", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-91.10, y=34.75, label= "Mississippi River", color="grey22",size=2.1, fontface="bold", angle=60) +
  
  
  annotate(geom="text", x=-102.10, y=24.00, label= "Mexico", color="grey22",size=3.1, fontface="bold") +
  
  annotate(geom="text", x=-90.50, y=15.5, label= "Guatemala", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-88.60, y=17.00, label= "Belize", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-86.30, y=15.20, label= "Honduras", color="grey22",size=2.7, fontface="bold") +
  
  annotate(geom="text", x=-85.30, y=12.50, label= "Nicaragua", color="grey22",size=2.7, fontface="bold") +
  
  annotate(geom="text", x=-79.50, y=22.35, label= "Cuba", color="grey22",size=2.7, fontface="bold", angle=340) +
  
  
  scale_bar(lon = -102.50, lat = 11.20,distance_lon = 250, distance_lat = 25, distance_legend = 100, dist_unit = "km", arrow_length = 200, arrow_distance = 200, arrow_north_size = 6, orientation = TRUE, legend_size = 2.0)+
  
  guides(color=guide_legend(title="Stork Age Class",title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5)) +
  
  guides(fill=guide_legend(show=FALSE)) +
  
  #scale_color_manual(labels=c( "Juvenile", "Sub Adult", "Adult"),values=c(cbPalette)) +
  
  #guides(fill=guide_legend(title="Capture Colonies", show=TRUE, override.aes=list(colour=c("#56B4E9","#000000")))) +
  
  
  theme_map() +
  
  theme(panel.grid.major = element_line(colour = "azure", linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "azure"), panel.border=element_rect(fill=NA),plot.title = element_text(size=16, face="bold"))


# Map of stork ID 475201

Stork475201<-

ggplot() +
  
  geom_sf(data = world, fill = "seashell1", col="grey28") +
  
  #geom_path(data = traj, aes(x = long, y = lat, group=group), col= "darkseagreen4", alpha=0.25, size=0.16) +
  
  coord_sf(xlim = c(-104.15, -76.12), ylim = c(11.20, 35.12), expand = TRUE) +
  
  geom_polygon(data=states, fill= NA, col= "grey35", aes(x=long, y=lat, group=group),inherit.aes=TRUE,alpha=0.4) +
  
  geom_polygon(data=lakeo, fill= "#56B4E9", col= "#56B4E9", aes(x=long, y=lat, group=group),inherit.aes=TRUE, alpha=0.7) +
  
  geom_path(data=missiriver,colour= "#56B4E9", aes(x= long, y=lat, group=group),inherit.aes=TRUE, size= 1.3, alpha=0.7) +
  
  
  geom_path(data=stork5j3, aes(x= longitude, y=latitude),colour = "#E69F00",size=1.5, inherit.aes=TRUE, alpha=0.8,pch=15) + 
  
  geom_path(data=stork5s3, aes(x= longitude, y=latitude), colour = "#009E73",size=1.5, inherit.aes=TRUE,alpha=0.8,pch=15) + 
  
  geom_path(data=stork5a3, aes(x= longitude, y=latitude),colour =  "#F0E442", size=1.5, inherit.aes=TRUE,pch=15) + 
  
  
  #xlab("LONGITUDE") + ylab("LATITUDE") +
  
  ggtitle("A",subtitle="") +
  
  #geom_text(aes(LON, LAT, label = NAME), data = world, size = 2.3, inherit.aes = TRUE, nudge_x = 0.0, col = "grey22", check_overlap = FALSE, fontface="bold") +
  
  #geom_text(aes(long, lat, label = region), data = cnames, size = 2.5, inherit.aes = TRUE, col = "grey22", check_overlap = TRUE, fontface="bold", nudge_x=.19, nudge_y=.35) +

  
  geom_point(data=df2, aes(x = lon2, y = lat2),color= "#0072B2", size = 4.3,inherit.aes = FALSE, pch=19) + 
  
  
  annotate(geom="text", x=-91.50, y=25.00, label= "Gulf of Mexico", color="grey22",size=4.5) +
  
  annotate(geom="text", x=-81.50, y=28.00, label= "Florida", color="grey22",size=2.1, fontface="bold") +
  
  #annotate(geom="text", x=-84.00, y=28.00, label= "Florida", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-99.00, y=31.50, label= "Texas", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-97.00, y=35.50, label= "Oklahoma", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-92.50, y=35.50, label= "Arkansas", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-92.50, y=31.50, label= "Louisiana", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-87.50, y=36.00, label= "Tennessee", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-86.70, y=33.5, label= "Alabama", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-83.70, y=33.5, label= "Georgia", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-79.75, y=36.00, label= "North Carolina", color="grey22",size=2.6, fontface="bold") +
  
  annotate(geom="text", x=-80.5, y=34.00, label= "South Carolina", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-89.55, y=34.00, label= "Mississippi", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-91.10, y=34.75, label= "Mississippi River", color="grey22",size=2.1, fontface="bold", angle=60) +
  
  
  annotate(geom="text", x=-102.10, y=24.00, label= "Mexico", color="grey22",size=3.1, fontface="bold") +
  
  annotate(geom="text", x=-90.50, y=15.5, label= "Guatemala", color="grey22",size=2.5, fontface="bold") +
  
  annotate(geom="text", x=-88.60, y=17.00, label= "Belize", color="grey22",size=2.2, fontface="bold") +
  
  annotate(geom="text", x=-86.30, y=15.20, label= "Honduras", color="grey22",size=2.7, fontface="bold") +
  
  annotate(geom="text", x=-85.30, y=12.50, label= "Nicaragua", color="grey22",size=2.7, fontface="bold") +
  
  annotate(geom="text", x=-79.50, y=22.35, label= "Cuba", color="grey22",size=2.7, fontface="bold", angle=340) +
  
  
  scale_bar(lon = -102.50, lat = 11.20,distance_lon = 250, distance_lat = 25, distance_legend = 100, dist_unit = "km", arrow_length = 200, arrow_distance = 200, arrow_north_size = 6, orientation = TRUE, legend_size = 2.0)+
  
  #guides(color=guide_legend(title="Stork Age Class",title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5)) +
  
  #guides(color=guide_legend(title="Age Class", show=TRUE)) +
  
  #scale_color_manual(values=c(cbPalette)) +
  
  #guides(fill=guide_legend(title="Capture Colonies", show=TRUE, override.aes=list(colour=c("#56B4E9","#000000")))) +
  
  
  theme_map() +
  
  theme(panel.grid.major = element_line(colour = "azure", linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "azure"), panel.border=element_rect(fill=NA),plot.title = element_text(size=16, face="bold"))


# Going to make a small map to place inbetween the two maps


# Connect to database
conn <- dbConnect("PostgreSQL", host = "basille-flrec.ad.ufl.edu",
                  dbname = "wood_stork_tracking", user = "mapper", password = "postgis")

# Get data from database (this can take a while)
traj <- pgGetGeom(conn, name = c("main","full_trajectories"))


traj1<- st_as_sf (traj)

# For ggplot2s to work with sf objects, all objects must be in sf format
worldmap <- getMap(resolution = "high")
map1<- sf::st_as_sf(worldmap )


allstorks<-
  ggplot() +
  
  geom_sf(data = map1,fill = "seashell1", col="grey28") +
  
  geom_path(data = traj, aes(x = long, y = lat, group=group), col= "darkseagreen4", alpha=0.25, size=0.01) +
  
  coord_sf(xlim = c(-88.00, -79.10), ylim = c(24.00, 33.50), expand = TRUE) +
    
    geom_polygon(data=lakeo, fill= "#56B4E9", col= "#56B4E9", aes(x=long, y=lat, group=group),inherit.aes=TRUE, alpha=0.7) +
    
    
  geom_polygon(data=states, fill= NA, col= "grey35", aes(x=long, y=lat, group=group),inherit.aes=TRUE,alpha=0.4) +
    
  
  ggtitle("C") +
  
  #scale_bar(lon = -99.66, lat = 8, distance_lon = 500, distance_lat = 50, distance_legend = 100, dist_unit = "km", arrow_length = 150, arrow_distance = 300, arrow_north_size = 6, orientation = TRUE, legend_size = 2) +
  
  #xlab("Longtitude") + ylab("Latitude") +
  
  theme_map() +
  
  theme(panel.grid.major = element_line(colour = "azure", linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "azure"), panel.border=element_rect(fill=NA),plot.title = element_text(size=16, face="bold"))





# Now grabbing the legends from LegendMap map and plotting them in the same window, the x11 is the size of the window I want, the NULL in the plot_grid is to make an empty space where I can later draw the legend with draw_grob

vp<- viewport(width = 0.37, height = 0.32, x = 0.50, y =0.15, just = c("bottom"))  

legend <- get_legend(LegendMap+ theme(legend.position="left") )


prow <- plot_grid( Stork475201 + theme(legend.position="none"), 
                   NULL, Stork572970 + theme(legend.position="none"),
                   align = 'vh',
                   #labels = c("A","", "B" ),
                   hjust = -1,
                   nrow = 1,
                   rel_widths = c(1, .30, 1)
)
x11(width=12, height=6, pointsize=10)

#To save run the whole code below at the same time

png("SimonaMap.png",width = 12, height = 6.5, units = "in", res = 300)
prow + draw_grob(legend, 2/4,1.3/2.63, 0.003) 
print(allstorks, vp=vp)
dev.off()










