install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("county.fips")
library(county.fips)

# because the projection is rectangular, these are not true areas on the globe.
m = map("state", fill = TRUE, plot = FALSE)
area.map(m)
area.map(m, ".*dakota")
area.map(m, c("North Dakota", "South Dakota"))
if(require(mapproj)) {
  # true areas on the globe
  m = map("state", proj="bonne", param=45, fill=TRUE, plot=FALSE)
  # North Dakota is listed as 70,704 square miles
  area.map(m, "North Dakota")
}


# Mapping counties
map('county', 'iowa', fill= TRUE, col= palette())

windows()
map('county', 'florida', fill= TRUE, col= palette())


data(countyMapEnv)
data (county.fips)

windows()
map('france', fill=TRUE, col= 1:10)


# Identifying what areas of the map were clicked on
windows()
identify(map("state", fill= TRUE, col=0))
if(required (mapproj))
  identify (map("world", proj= "larange", fill=TRUE, col=0))

sov.expand("France")

# Map of Spain has no Canary islands, loading them in the picture
iso.expand("ES")
windows()
map(regions= sov.expand ("Spain"))

# Making a map with iso codes as labels
windows()
wm<- map("world", fill= TRUE, col=0, xlim=c(-10,40), ylim=c(30,60))
# if you take out the isalnd, ut you lookw the UK and NZ
nam<-grep(":", wm$names, inv= TRUE, val=TRUE)
#adding ISO labels
map.text(wm,regions=nam,label=iso.alpha(nam),col=2,exact=TRUE)

map('broward','florida', fill=FALSE)
data("county.fips")
county.fips
data("countyMapEnv")
countyMapEnv
data("stateMapEnv")
data("state.vbm.center")
data("usaMapEnv")
data("worldMapEnv")
data ("world.cities")

# Mao this borders are clear, US map
windows()
map('state.vbm', fill=TRUE, col=palette())

# Color pallete of the US map
windows()
map('state', fill=TRUE, col=palette())

# Just a black outline of the US
windows()
map('usa')

# A color palette of the world map
windows()
map('world', fill=TRUE, col= 1:10)


#Old world map, from 1990, like an outline but I'm not sure if it's from the 1990s
windows()
map("legacy_world")

# Pacific CEntric Low resolution world map
data(world2MapEnv)
windows()
map("world", projection="rectangular", parameter=0, orientation= c(90,0,180), wrap= TRUE)

windows()
map('world2', xlim=c(100,300))
map.axes()

windows()
map('county', 'florida', fill= TRUE, col= palette())
map.axes()




