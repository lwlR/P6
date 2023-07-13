library(caret)
library(terra)

maindir <- "/data/teaching/MasterP6/data_ednx"
ortho <- rast(file.path(maindir, "20230707_EDNX-ortho_resampled_1m.tif"))

b1 <- rast(file.path(maindir, "band_1_orthophoto_cor.tif"))

multis <- c(crop(rast(file.path(maindir, "band_1_orthophoto_cor.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_2_orthophoto_cor.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_3_orthophoto_cor.tif")), ext(b1)), 
            crop(rast(file.path(maindir, "band_4_orthophoto_cor.tif")), ext(b1)),
            crop(rast(file.path(maindir, "band_5_orthophoto_cor.tif")), ext(b1)))

lid_m <- vect(file.path(maindir, "20230707_viehtranke_metrics.shp"))
px_dat <- extract(ortho,lid_m)
px_dat_ms <- extract(multis,lid_m)

lid_m$b1 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, mean, na.rm = TRUE)
lid_m$b2 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, mean, na.rm = TRUE)
lid_m$b3 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, mean, na.rm = TRUE)

lid_m$b1_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, sd, na.rm = TRUE)
lid_m$b2_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, sd, na.rm = TRUE)
lid_m$b3_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, sd, na.rm = TRUE)

lid_m$b_ms1 <- tapply(px_dat_ms$"20230707_EDNX-ortho_resampled_1m_1", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m$b_ms2 <- tapply(px_dat_ms$"20230707_EDNX-ortho_resampled_1m_2", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m$b_ms3 <- tapply(px_dat_ms$"20230707_EDNX-ortho_resampled_1m_3", px_dat_ms$ID, mean, na.rm = TRUE)

lid_m$b1_ms_sd <- tapply(px_dat_ms$"20230707_EDNX-ortho_resampled_1m_1", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m$b2_ms_sd <- tapply(px_dat_ms$"20230707_EDNX-ortho_resampled_1m_2", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m$b3_ms_sd <- tapply(px_dat_ms$"20230707_EDNX-ortho_resampled_1m_3", px_dat_ms$ID, sd, na.rm = TRUE)


tr_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                        savePredictions = "final")

mod_trained <- train(tree_species ~ ., data = as.data.frame(lid_m[,-1]), trControl = tr_ctrl, method = "rf")

mod_trained