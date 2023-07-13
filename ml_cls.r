library(caret)
library(terra)

maindir <- "/data/teaching/MasterP6/data_ednx"
ortho <- rast(file.path(maindir, "20230707_EDNX-ortho_resampled_1m.tif"))

b1 <- rast(file.path(maindir, "band_1_orthophoto.tif"))

multis <- c(crop(rast(file.path(maindir, "band_1_orthophoto.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_2_orthophoto.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_3_orthophoto.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_4_orthophoto.tif")), ext(b1)),
            crop(rast(file.path(maindir, "band_5_orthophoto.tif")), ext(b1)))

lid_m <- vect(file.path(maindir, "20230707_viehtranke_metrics.shp"))
px_dat <- extract(ortho,lid_m)

lid_m$b1 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, mean, na.rm = TRUE)
lid_m$b2 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, mean, na.rm = TRUE)
lid_m$b3 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, mean, na.rm = TRUE)


lid_m$b1_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, sd, na.rm = TRUE)
lid_m$b2_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, sd, na.rm = TRUE)
lid_m$b3_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, sd, na.rm = TRUE)

tr_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                        savePredictions = "final")

mod_trained <- train(tree_species ~ ., data = as.data.frame(lid_m[,-1]), trControl = tr_ctrl, method = "rf")

estimates <- tapply(mod_trained$pred$pred[order(mod_trained$pred$rowIndex)], 
                    INDEX = mod_trained$pred$rowIndex[order(mod_trained$pred$rowIndex)], mean)

