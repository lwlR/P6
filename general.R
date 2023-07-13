library(terra)
library(lidR)


#maindir <- "/home/lehnert/Nextcloud/Lehre/2022_SoSe_Pr_Datenanalyse/data"


#----------------------------------------------------------------------
# course outline
#----------------------------------------------------------------------

# all on small extent


# TERRA part
# 1. slides on terra
# 2. read multispectral data and the rgb orthophoto
# 3. do some plots
# 4. create shapefiles of the respective west and east plots from the coordinates in the google docs
# (5. learn how to cut rasters with shape files)

# LidR part
# 1. slides on LidR
# 2. import data, check structure and see how .las / .laz are composed
# 3. plot data
# 4. ITD (segmentation), DTM, CHM
# 5. export to raster 



#----------------------------------------------------------------------
# morning part
#----------------------------------------------------------------------
# ------> fuer terra siehe Datei terra.R



maindir = 'C:/Users/paule/Desktop/p6/'

# 2. lidR
las04 = readLAS(file.path(maindir, "20230422_EDNX_viehtraenke.laz"))
las07 = readLAS(file.path(maindir, "20230707_EDNX_viehtraenke.laz"))
#print header
print(las04)

#check data for crs, point validity, etc.
las_check(las04)
hist(las04$Z) # Check for points above trees


#plot whole area 
plot(las07, axis = TRUE, bg = 'white')


# - CHM rechnen
chm04 = rasterite_canopy(las04, res = 0.1, algorithm = p2r(0.1))
plot(chm04)
writeRaster(chm04, 'chm04.tif') #load chm into QGIS: Check offset in comp. to ortho
# - DTM rechnen 
# - export als raster
## -> QGIS
## Visualisierung CMD, DTM, Ortho
## Fehler suchen
# -> R 
# Fehler beheben:
# Height normalization
#TEST


#----------------------------------------------------------------------
# afternoon part
#----------------------------------------------------------------------
