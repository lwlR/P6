library(terra)

maindir <- "/data/projects/Students/Master/eisenschink/p6"
# 1. Terra
# - spielen Multispektral
# - shapefile aus den plots erstellen
# Read orthophoto in R
orth <- rast(file.path(maindir, "20230707_EDNX-ortho.tif"))
# Get summary of file
orth
# Create a first plot
plotRGB(orth)
# Vector data

# Create shapefile from field measurements
# Read Trimble and StoneX
trmbl <- vect(file.path(maindir, "coordinaten_prakt_2.shp"))
stx <- vect(file.path(maindir, "coordinaten_prakt_1.shp"))
# Plot field data above orthophoto
plotRGB(orth)
plot(trmbl, add = TRUE, col = "lightgray")
plot(stx, add = TRUE, col = "lightgray")

# Merge both GPS devices
names(stx) <- c("Name", "Easting", "Northing", "Elevation")
gps_points <- vect(c(trmbl,stx))
# Plot Result
plotRGB(orth)
plot(gps_points, add = TRUE, col = "lightgray")

# Join table with field measurements/observations
# Load table with field measurements
fd <- read.csv(file.path(maindir, "fielddata.csv"))#
fd_shp <- merge(x = gps_points, y = fd, by = "Name", all.x = T, all.y = T)


#fd_shp <- vect(fd, geom=c("lat", "lon"), crs=crs(orth), keepgeom=FALSE) # Needs to be updated with the correct columns names and crs
# Plot field data above orthophoto
plotRGB(orth)
for (i in 1:nrow(fd_shp))
{
    if (is.finite(fd_shp$dbh[i]))
       points(fd_shp[i,], col = "lightgray", cex = 1.3*fd_shp$dbh[i])
}

# Save shapefile to disc
writeVector(fd_shp, file.path(maindir, "20230707_EDNX_fieldData.shp"))