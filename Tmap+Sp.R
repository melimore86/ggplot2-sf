# Trying to make a map with tmap package
library(tmap)
library (tmaptools)
windows()
qtm(worldmap)

data(land)
data(rivers)
data(metro)
windows()
tm_shape(worldmap) +tm_polygons(col="grey", border.col="black")+ tm_shape(land)+tm_raster("trees", breaks=seq(0,100, by=20), legend.show=FALSE) +tm_shape(rivers) +tm_lines(lwd="strokelwd", scale=5, legend.lwd.show=FALSE)+ tm_style_natural()

tm_shape(traj)+ tm_lines(lwd=5, scale=5, legend.lwd.show= FALSE, col= "red")

