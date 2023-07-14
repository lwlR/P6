library(caret)
library(terra)

maindir <- "/data/teaching/MasterP6/data_ednx"
ortho <- rast(file.path(maindir, "20230707_EDNX-ortho_resampled_1m.tif"))

multis <- rast(file.path(maindir, "multispectral_crop.tif"))

lid_m <- vect(file.path(maindir, "20230707_viehtranke_metrics.shp"))
f_data <- vect(file.path(maindir, "field_measuremnts.shp"))

r <- relate(f_data, lid_m, "within", pairs = TRUE)

lid_m$species <- NA
lid_m$species[r[,2]] <- f_data$Baumart[r[,1]]

lid_m_t <- lid_m[!is.na(lid_m$species),]
plot(lid_m_t)
px_dat <- extract(ortho,lid_m_t)
px_dat_ms <- extract(multis,lid_m_t)

lid_m_t$b1 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, mean, na.rm = TRUE)
lid_m_t$b2 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, mean, na.rm = TRUE)
lid_m_t$b3 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, mean, na.rm = TRUE)

lid_m_t$b1_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, sd, na.rm = TRUE)
lid_m_t$b2_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, sd, na.rm = TRUE)
lid_m_t$b3_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, sd, na.rm = TRUE)

lid_m_t$b_ms1 <- tapply(px_dat_ms$"multispectral_crop_1", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m_t$b_ms2 <- tapply(px_dat_ms$"multispectral_crop_2", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m_t$b_ms3 <- tapply(px_dat_ms$"multispectral_crop_3", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m_t$b_ms4 <- tapply(px_dat_ms$"multispectral_crop_4", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m_t$b_ms5 <- tapply(px_dat_ms$"multispectral_crop_5", px_dat_ms$ID, mean, na.rm = TRUE)

lid_m_t$b1_ms_sd <- tapply(px_dat_ms$"multispectral_crop_1", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m_t$b2_ms_sd <- tapply(px_dat_ms$"multispectral_crop_2", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m_t$b3_ms_sd <- tapply(px_dat_ms$"multispectral_crop_3", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m_t$b4_ms_sd <- tapply(px_dat_ms$"multispectral_crop_4", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m_t$b5_ms_sd <- tapply(px_dat_ms$"multispectral_crop_5", px_dat_ms$ID, sd, na.rm = TRUE)


lid_m_d <- as.data.frame(lid_m_t)
for (i in 1:ncol(lid_m_d))
{
    if (any(is.na(lid_m_d[,i])))
    {
        print(c(i, names(lid_m_d[i])))
    }
}
lid_m_d <- lid_m_d[!is.na(lid_m_d$species),]
lid_m_d$decidious <- NA
lid_m_d$decidious[lid_m_d$species %in% c("Ahorn", "Linde", "Eiche")] <- 1
lid_m_d$decidious[lid_m_d$species %in% c("Kiefer", "Fichte")] <- 0

lid_m_d$species <- as.factor(lid_m_d$species)
lid_m_d$decidious <- as.factor(lid_m_d$decidious)

tr_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                        savePredictions = "final")

mod_trained <- train(decidious ~ ., data = lid_m_d[,c(-1,-7, -58)], trControl = tr_ctrl, method = "rf")
mod_trained_s <- train(species ~ ., data = lid_m_d[,c(-1,-7, -75)], trControl = tr_ctrl, method = "rf")


mod_trained
mod_trained_s


plot(varImp(mod_trained))

plot(lid_m_d$zmax ~ lid_m_d$decidious)
plot(lid_m_d$b_ms4 ~ lid_m_d$decidious)


writeVector(lid_m, file.path(maindir,"training_data.shp"))


px_dat <- extract(ortho,lid_m)
px_dat_ms <- extract(multis,lid_m)

lid_m$b1 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, mean, na.rm = TRUE)
lid_m$b2 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, mean, na.rm = TRUE)
lid_m$b3 <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, mean, na.rm = TRUE)

lid_m$b1_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_1", px_dat$ID, sd, na.rm = TRUE)
lid_m$b2_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_2", px_dat$ID, sd, na.rm = TRUE)
lid_m$b3_sd <- tapply(px_dat$"20230707_EDNX-ortho_resampled_1m_3", px_dat$ID, sd, na.rm = TRUE)

lid_m$b_ms1 <- tapply(px_dat_ms$"multispectral_crop_1", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m$b_ms2 <- tapply(px_dat_ms$"multispectral_crop_2", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m$b_ms3 <- tapply(px_dat_ms$"multispectral_crop_3", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m$b_ms4 <- tapply(px_dat_ms$"multispectral_crop_4", px_dat_ms$ID, mean, na.rm = TRUE)
lid_m$b_ms5 <- tapply(px_dat_ms$"multispectral_crop_5", px_dat_ms$ID, mean, na.rm = TRUE)

lid_m$b1_ms_sd <- tapply(px_dat_ms$"multispectral_crop_1", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m$b2_ms_sd <- tapply(px_dat_ms$"multispectral_crop_2", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m$b3_ms_sd <- tapply(px_dat_ms$"multispectral_crop_3", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m$b4_ms_sd <- tapply(px_dat_ms$"multispectral_crop_4", px_dat_ms$ID, sd, na.rm = TRUE)
lid_m$b5_ms_sd <- tapply(px_dat_ms$"multispectral_crop_5", px_dat_ms$ID, sd, na.rm = TRUE)

decidous <- predict(mod_trained, lid_m)