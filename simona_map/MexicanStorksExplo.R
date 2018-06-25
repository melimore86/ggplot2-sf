#### Install packages ####

install.packages("adehabitatLT")
library(RPostgreSQL)
library(lubridate)
library(adehabitatLT)
library(sp)
library(rgdal)
library(rworldmap)
library(rworldxtra)
library(dplyr)
library(hab)

#### Import data ####

# GPS data
drv<-dbDriver("PostgreSQL")
conMS<-dbConnect("PostgreSQL",dbname = "wood_stork_tracking", host =  "basille-flrec.ad.ufl.edu", port='5432', user = "mapper", password = "postgis")
# Instert password before running

gps_data_animals <-dbReadTable(conMS, c("main","gps_data_animals"))
gps <- gps_data_animals
gps$tag <- factor(gps$tag_id)
gps$id <- factor(gps$animal_id)

gps <- gps[,-c(3,4,11,12,13)]
gps <- gps[,c(1,2,9,10,3,4,5,6,7,8)]
gps <- droplevels(gps[order(gps$id, gps$acquisition_time), ])

length(unique(gps$id)) # 133 individuals 

# Info on individuals
ind <- read.csv("/home/simona/Documents/PhD/Research/Wood Stork/Data - Partial/TelemetryShouldEndDates_ForMathieu.csv", col.names = c("id", "wost", "age", "year", "cycle", "colony", "type", "capture", "date_start", "date_end", "status"))
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

#### Explore general data #### 

# Histogram of data across years
par(mar = c(5, 5, 1, 1) + .1)
hist(gps$acquisition_time, breaks = "weeks", freq = TRUE, xlab = "Date", 
     col = grey(0.4), eborder = grey(0.4), axes = FALSE, main = NULL)
axis(1, at = ymd(paste0(2004:2011, "-07-01")), labels = 2004:2011, 
     tick = FALSE, cex.axis = 1.2) # For some reason this code for the axis doesn't work
axis(2)
abline(v = ymd(paste0(2004:2011, "-01-01")), lwd = 3, col = "white", 
       lty = 3) # For some reason this code for the abline doesn't work

# Histogram of data according to time of the day
barplot(table(hour(gps$acquisition_time)), space = 4, col = grey(0.3), border = grey(0.3), 
        xlab = "Hour of the day (Eastern time with DST)", ylab = "Frequency")

#### Managing locations spatially ####

gps <- gps[gps$gps_validity_code==1,]
  
coordinates(gps) <- gps[, c("longitude", "latitude")] # In order to keep longitude/latitude in the object
## Beware that calls to gps$longitude/latitude refers to the coordinates of the SPDF; 
## if needed, use gps@data$longitude/latitude to extract the coordinate stored in decimal degrees
proj4string(gps) <- CRS("+proj=longlat +datum=WGS84")
summary(gps)

EPSG <- make_EPSG()
EPSG[grep("(WGS 84|NAD83).*(UTM zone 17)", EPSG$note), ]

gps <- spTransform(gps, CRS("+init=epsg:32617"))
summary(gps)

# Global map
par(mar = rep(.1, 4))
plot(getMap(resolution = "high"), col = "cornsilk", bg = "azure2", 
     border = grey(0.5), xlim = range(gps@data$longitude), ylim = range(gps@data$latitude))
box()
points(gps@data$longitude, gps@data$latitude, pch = 3, cex = .5, col = gps$id)

#### Create trajectories ####

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

# Map for both ind, with individuals in different colors and color getting darker with age
par(mar = rep(.1, 4))
plot(getMap(resolution = "high"), col = "cornsilk", bg = "azure2", 
     border = grey(0.5), xlim = range(mex@data$longitude), ylim = range(mex@data$latitude))
box()
points(mexjuv@data$longitude, mexjuv@data$latitude, pch = 20, cex = .5, col = "dodgerblue")
palette(c("red3","dodgerblue3"))
points(mexsub@data$longitude, mexsub@data$latitude, pch = 20, cex = .5, col = mexsub@data$id)
points(mexad@data$longitude, mexad@data$latitude, pch = 20, cex = .5, col = "dodgerblue4")
legend("bottomright", c("Ind1 - juvenile","Ind1 - subadult", "Ind1 - adult", "Ind2 - subadult"), 
       fill= c("dodgerblue","dodgerblue3","dodgerblue4","red3"))

# Subset each individual in the different age classes
stork4s <- subset(mexsub, id==475201)
stork5j <- subset(mexjuv, id==572970)
stork5s <- subset(mexsub, id==572970) 
stork5a <- subset(mexad, id==572970)
stork4s@data <- droplevels(stork4s@data)
stork5j@data <- droplevels(stork5j@data)
stork5s@data <- droplevels(stork5s@data)
stork5a@data <- droplevels(stork5a@data)

# Plot for stork 475201
plot(getMap(resolution = "high"), col = "cornsilk", bg = "azure2", 
     border = grey(0.5), xlim = range(mex@data$longitude), ylim = range(mex@data$latitude))
box()
points(stork4s@data$longitude, stork4s@data$latitude, pch = 20, cex = .5, col = "red")

# Plot for stork 572970
plot(getMap(resolution = "high"), col = "cornsilk", bg = "azure2", 
     border = grey(0.5), xlim = range(mex@data$longitude), ylim = range(mex@data$latitude))
box()
points(stork5j@data$longitude, stork5j@data$latitude, pch = 20, cex = .5, col = "orange")
points(stork5s@data$longitude, stork5s@data$latitude, pch = 20, cex = .5, col = "red")
points(stork5a@data$longitude, stork5a@data$latitude, pch = 20, cex = .5, col = "black")

#### Create Ltraj ####

mex.traj <- as.ltraj(cbind(mex@data$longitude,mex@data$latitude),mex@data$date,mex@data$id)
mex.traj

# Stork 4 was captured in Noxubee as a subadult on Aug 18th 2006, and was tracked until Nov 27th 2006. 
# Stork 5 was captured in Chew Mill as a juvenile on Jun 22nd 2005, and was tracked as a subadult and adult until Oct 27th 2009.

# One separate trajectory for each age class x individual
s4s.traj <- as.ltraj(cbind(stork4s@data$longitude,stork4s@data$latitude),stork4s@data$date,stork4s@data$id) # 3 months
s5j.traj <- as.ltraj(cbind(stork5j@data$longitude,stork5j@data$latitude),stork5j@data$date,stork5j@data$id) # 8 months
s5s.traj <- as.ltraj(cbind(stork5s@data$longitude,stork5s@data$latitude),stork5s@data$date,stork5s@data$id) # 3 years
s5a.traj <- as.ltraj(cbind(stork5a@data$longitude,stork5a@data$latitude),stork5a@data$date,stork5a@data$id) # 8 months
# Tot for stork 5: 4 years and ~4 months

trajdyn(mex.traj)
# doesn't work

# Plot months with different colors just to get an idea of when they were where.
plot(getMap(resolution = "high"), col = "cornsilk", bg = "azure2", 
     border = grey(0.5), xlim = range(mex@data$longitude), ylim = range(mex@data$latitude))
box()
points(stork4s@data$longitude, stork4s@data$latitude, pch = 20, cex = .5, col = month(stork4s@data$date))
#
plot(getMap(resolution = "high"), col = "cornsilk", bg = "azure2", 
     border = grey(0.5), xlim = range(mex@data$longitude), ylim = range(mex@data$latitude))
box()
points(stork5j@data$longitude, stork5j@data$latitude, pch = 20, cex = .5, col = month(stork5j@data$date))
points(stork5s@data$longitude, stork5s@data$latitude, pch = 20, cex = .5, col = month(stork5s@data$date))
points(stork5a@data$longitude, stork5a@data$latitude, pch = 20, cex = .5, col = month(stork5a@data$date))

