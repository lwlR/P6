#install.packages("httpgd")

library(lidR)
library(terra)
#biomass algorithm for group 1

#local repo

path = "D:/biomass"


#read the shapes
points  = terra::vect(file.path(path, "field_measuremnts.shp"))
shp_east = terra::vect(file.path(path, "ext_plot_east.shp"))

#read the ortho
dop_east = rast(file.path(path, "20230707_EDNX_ortho_ext_plot_east.tif"))

window()
plotRGB(dop_east)

#test for steffi 