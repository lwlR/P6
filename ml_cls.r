library(caret)
library(terra)

maindir <- "/data/teaching/MasterP6/data_ednx"
ortho <- rast(file.path(maindir, "20230707_EDNX-ortho_COG.tif"))

b1 <- rast(file.path(maindir, "band_1_orthophoto.tif"))

multis <- c(crop(rast(file.path(maindir, "band_1_orthophoto.tif")), ext(b1)), crop(rast(file.path(maindir, "band_2_orthophoto.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_3_orthophoto.tif")), ext(b1)), crop(rast(file.path(maindir, "band_4_orthophoto.tif")), ext(b1)),
            crop(rast(file.path(maindir, "band_5_orthophoto.tif")), ext(b1)))


