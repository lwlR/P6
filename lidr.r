library(terra)
library(lidR)
library(ggplot2)
library(sf)
library(dbscan)


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


maindir = 'W:/daten_p6'
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
# Load table with field measurements
fd <- read.csv("fielddata.csv")#
# Create shapefile from field measurements
fd_shp <- vect(fd, geom=c("lon", "lat"), crs="", keepgeom=FALSE) # Needs to be updated with the correct columns names and crs
# Plot field data above orthophoto
plotRGB(orth)
plot(fd_shp, add = TRUE, col = "darkred")
# Save shapefile to disc
writeVector(fd_shp, file.path(maindir, "20230707_EDNX_fieldData.shp")



# 2. lidR
las07 = readLAS(file.path(maindir, "20230707_EDNX_viehtraenke.laz"))
#print header
print(las07)

#check data for crs, point validity, etc.
las_check(las07)
hist(las07$Z) # Check for points above trees


#plot whole area 
plot(las07, axis = TRUE, bg = 'white')
plot(las07)

# - CHM rechnen
chm07 = rasterize_canopy(las07, res = 0.1, algorithm = p2r(0.1))
plot(chm07)
writeRaster(chm07, file.path(maindir, 'chm07.tif')) #load chm into QGIS: Check offset in comp. to ortho

#correct offset
las07@data$X = las07@data$X - 2.6 #für 07
las07@data$Y = las07@data$Y - 0.3 #für 07

chm07 = rasterite_canopy(las07, res = 0.1, algorithm = p2r(0.1))
writeRaster(chm07, file.path(maindir, 'chm07_corr1.tif'))  #load chm into QGIS: Check offset in comp. to ortho

#Height normalization
las07@data$Classification = 0
las07 = classify_ground(las07, algorithm = csf())
dtm07 = rasterize_terrain(las07, res = 0.5, algorithm = tin())

plot(dtm07)
writeRaster(dtm07, file.path(maindir, 'dtm07.tif'))  #load chm into QGIS: Was fällt auf?

las07 = las07 - dtm07

plot(las07)
hist(las07$Z)

#filter birds
las07 <- filter_poi(las07, Z < 30)

#ITS
chm07 = rasterize_canopy(las07, res = 0.1, algorithm = p2r(0.1))
las07 = segment_trees(las07, lidR::watershed(chm = chm07, th_tree = 3, tol = 0.9, ext = 6))

max(las07$treeID, na.rm = TRUE)

#plot ITS
plot(las07, color = 'treeID', bg = 'white')
plot(filter_poi(las07, treeID == 448))

#crown metrics
metrics07 = crown_metrics(las07, .stdmetrics, geom = 'convex')
plot(metrics07['area'])

#export to QGIS
st_write(metrics07, file.path(maindir, "20230707_viehtranke_metrics.shp"), append = FALSE)

#crown metrics und plote bäume über identify -> Stamm nicht da!!!!


#----------------------------------------------------------------------
# afternoon part
#----------------------------------------------------------------------

# 2. lidR

#dbh alog erklären

#explain step by step: 
dbh_algo_prakt <- function(las, dbh_z1, dbh_z2, outl_det_th, max_dbh){ #dbh_z1 = 1, dbh_z2 = 3, outl_det_th = 2.5, max_dbh= 1.5
  df_diam = data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df_diam) <- c('h_mid', "x_center","y_center", "diam_08", "diam_09", "diam_095", "diam_099", 'n_points', 'id_tree', 'id_cluster', 'id_cluster_fein')
  for (baum in 1:max(las$treeID, na.rm = TRUE)){
    #select tree
    stem_ng <- filter_poi(las, treeID == baum)
    stem_n <- filter_poi(stem_ng, Z < max(stem_ng$Z)*0.3 & Z > 0.5) #max(stem_ng$Z)*0.3
    stem_n <- segment_shapes(stem_n, shp_vline(th1 = 2, th2 = 0.5, k = 50), "vertical_tree_level")
    stem_n <- filter_poi(stem_n, vertical_tree_level == TRUE)
    print(baum)
    #hdbsacn
    if (length(stem_n$X) < 50){
      #print(baum)
      #print('not enough points in stem sector, skipping tree')
      df_diam[nrow(df_diam) + 1,] <- c(NA, mean(stem_ng$X), mean(stem_ng$Y), NA, NA, NA, NA, length(stem_n$X), baum, NA, NA)
    } else {
      min_p_cluster <- 50 #What ever you pick here highly influences the number of trees recognized
      stem_n_hdbscan <- hdbscan(cbind(stem_n$X, stem_n$Y), minPts = min_p_cluster)
      stem_n <- add_attribute(stem_n, stem_n_hdbscan$cluster, 'cluster')
      while (max(stem_n_hdbscan$cluster) == 0){
        min_p_cluster <- min_p_cluster - 5
        stem_n_hdbscan <- hdbscan(cbind(stem_n$X, stem_n$Y), minPts = min_p_cluster)
        stem_n <- add_attribute(stem_n, stem_n_hdbscan$cluster, 'cluster')
      }
      for (clus in 1:max(stem_n_hdbscan$cluster)){
        stem_n_c_n <- filter_poi(stem_n, cluster == clus)
        if (length(stem_n_c_n$X) < 15){
          #print('not enough points in cluster')
          df_diam[nrow(df_diam) + 1,] <- c(NA, mean(stem_n_c_n$X), mean(stem_n_c_n$Y), NA, NA, NA, NA, length(stem_n$X), baum, clus, NA)
        } else {
          if (max(stem_n_c_n$Z)>=4 & sum((hist(stem_n_c_n$Z, breaks = c(0:round(max(stem_n_c_n$Z)+1, 0)))$counts) == 0)/(length(c(0:round(max(stem_n_c_n$Z), 0)))-1) <= 0.5  & (hist(stem_n_c_n$Z, breaks = c(0:round(max(stem_n_c_n$Z)+2, 0)))$counts[2]) > 3 | (hist(stem_n_c_n$Z, breaks = c(0:round(max(stem_n_c_n$Z)+2, 0)))$counts[3]) > 3){
            clus_h1 <- filter_poi(stem_n_c_n, Z < dbh_z2 & Z > dbh_z1)
            clus_h1_pm <- point_metrics(clus_h1, ~mean(Z), k = 2, xyz = TRUE)
            clus_h1_X <- clus_h1_pm$X - round(clus_h1_pm$X[1],0) + 1
            clus_h1_Y <- clus_h1_pm$Y - round(clus_h1_pm$Y[1],0) + 1
            distances <- sqrt((clus_h1_X - mean(clus_h1_X))^2 + (clus_h1_Y - mean(clus_h1_Y))^2)
            threshold <- outl_det_th * median(distances)
            outliers <- which(distances > threshold)
            if (length(outliers) == 0){
              if (quantile(distances, 0.925) > max_dbh){ 
                df_diam[nrow(df_diam) + 1,] <- c(NA, mean(stem_n_c_n$X), mean(stem_n_c_n$Y), NA, NA, NA, NA, length(stem_n$X), baum, clus, NA)
              } else {
                xo <- clus_h1_X
                yo <- clus_h1_Y
                df_o <- data.frame(xo, yo)
                disto <- dist(df_o[,])
                quantile(disto, 0.9)
                quantile(disto, 0.95)
                quantile(disto, 0.99)
                df_diam[nrow(df_diam) + 1,] <- c(1.5, mean(clus_h1$X), mean(clus_h1$Y), quantile(disto, 0.8), quantile(disto, 0.9), quantile(disto, 0.95), quantile(disto, 0.99), length(stem_n$X), baum, clus, NA)
              }
            } else {
              if (quantile(distances[-outliers], 0.925) > 1.5){
              } else {
                xo <- clus_h1_X[-outliers]
                yo <- clus_h1_Y[-outliers]
                df_o <- data.frame(xo, yo)
                disto <- dist(df_o[,])
                quantile(disto[-outliers], 0.9)
                quantile(disto[-outliers], 0.95)
                quantile(disto[-outliers], 0.99)
                df_diam[nrow(df_diam) + 1,] <- c(1.5, mean(clus_h1$X), mean(clus_h1$Y), quantile(disto, 0.8), quantile(disto[-outliers], 0.9), quantile(disto[-outliers], 0.95), quantile(disto[-outliers], 0.99), length(stem_n$X), baum, clus, NA)
              }
            }
          }
        }
      }
    }
  }
  xy <- df_diam[, c(2,3)]
  sdf_diam <- st_as_sf(x = df_diam, coords = c("x_center", "y_center"), crs = '+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs')
  n_sdf_diam <- sdf_diam[!is.na(sdf_diam$diam_09),]
  return(n_sdf_diam)
}

#Gebiete aus plot ost:
las04_plot_ost = readLAS(file.path(maindir, "20230422_EDNX_plot_ost.laz"))
las04_plot_ost@data$X = las04_plot_ost@data$X - 3.95
las04_plot_ost@data$Y = las04_plot_ost@data$Y - 2.75
plot(las04_plot_ost, bg = 'white')

las07_plot_ost = readLAS(file.path(maindir, "20230707_EDNX_plot_ost.laz"))
#las07_plot_ost@data$X = las07_plot_ost@data$X - 3.9
#las07_plot_ost@data$Y = las07_plot_ost@data$Y - 2.8
plot(las07_plot_ost, bg = 'white')

#chm
chm_ost04 = rasterize_canopy(las04_plot_ost, res = 0.1, algorithm = p2r(0.1))
plot(chm_ost04)
chm_ost07 = rasterize_canopy(las07_plot_ost, res = 0.1, algorithm = p2r(0.1))
plot(chm_ost07)
writeRaster(chm_ost07, file.path(maindir, "test.tif"), overwrite = TRUE)

#its
las04_plot_ost = segment_trees(las04_plot_ost, lidR::watershed(chm = chm_ost04, th_tree = 3, tol = 0.9, ext = 6))
max(las04_plot_ost$treeID, na.rm = TRUE)
#plot(filter_poi(las04_plot_ost, treeID == 16), size = 4)

las07_plot_ost = segment_trees(las07_plot_ost, lidR::watershed(chm = chm_ost07, th_tree = 3, tol = 0.9, ext = 6))
max(las07_plot_ost$treeID, na.rm = TRUE)

#metrics
metrics_ost04 = crown_metrics(las04_plot_ost, .stdmetrics, geom = 'convex')
st_write(metrics_ost04, file.path(maindir, "metrics_ost04.shp"), append = FALSE)

metrics_ost07 = crown_metrics(las07_plot_ost, .stdmetrics, geom = 'convex')
st_write(metrics_ost07, file.path(maindir, "metrics_ost071.shp"), append = FALSE)


#DBH Algo: Welche Punktwolke würdet ihr nutzen?

dbh_plot_ost_04 = dbh_algo_prakt(las04_plot_ost, 1,3,2.5,1.5)
dbh_plot_ost_041 = dbh_plot_ost_04
plot(dbh_plot_ost_04$diam_08)
plot(dbh_plot_ost_04$geometry)

#export to QGIS
st_write(dbh_plot_ost_04, file.path(maindir, 'dbh_plot_ost_04.shp'))

#leider noch immer versatz: Evenutell stone X vs TRimble 
dbh_plot_ost_04$geometry = dbh_plot_ost_04$geometry + c(0.8,1.5)
st_write(dbh_plot_ost_04, file.path(maindir, 'dbh_plot_ost_04_cor.shp'), append = FALSE)


plot(filter_poi(las07_plot_ost, treeID == 9), bg = 'white', size = 5)
