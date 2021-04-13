rm(list=ls())
cat("\014")
options(warn=-1)
library(rgdal)
library(raster)
library(gstat)
library(ggplot2)
library(sf)
source('climate_functions.R')

fileStudy <-'SPATIAL/ShapeFiles/StudyArea.shp'
dir2Read = dirName('PROCESS', 'Spatial_data')


## Annual plot ---------------------
ok.result <- get_ordinary_kriging(model = 'ENSEMBLE', dir2Read, fileStudy,
                                  scale = 'Annual', stat = 'Mean', 
                                  attribute_name='X2032', emm_scene = 'RCP45')
ok <- ok.result$krig
# Plotting map in ggplot
ok.dataFrame = as.data.frame(ok$var1.pred, xy = T)
strtitle.main = 'Average Tempearure for the year 2032'
strtitle.sub = 'Ensembles of GCMs under RCP4.5 emmission scenario'
ggplot(ok.dataFrame) + 
  geom_raster(aes(x,y, fill= var1.pred)) + 
  scale_fill_viridis_c(option  = "plasma") + 
  theme_minimal() + 
  labs(title = strtitle.main,
       subtitle = strtitle.sub, 
       x = "Longitude", 
       y = "Latitude", 
       fill = "Tempearure") +
  theme_bw()+
  coord_fixed(1.1)
dir2Save = dirName('Figures', 'SpatialAnalysis')
file_name = 'Year_2032_Ensemble_RCP45'
fileImgPng <- paste(dir2Save, '/', file_name, '.png', sep = '') 
ggsave(fileImgPng, plot = last_plot(), width = 8, height = 11)
dir2Save = dirName('ToGIS', 'SpatialAnalysis')
fileImgtiff <- paste(dir2Save, '/',file_name, '.tiff', sep = '') 
bf <- writeRaster(ok, filename=fileImgtiff, 
                  options="INTERLEAVE=BAND", overwrite=TRUE)


## Month plot ---------------------
ok.result <- get_ordinary_kriging(model = 'ENSEMBLE', dir2Read, fileStudy,
                                  scale = 'Month', stat = 'Mean', 
                                  attribute_name='Aug.2032', emm_scene = 'RCP45')
ok <- ok.result$krig
# Plotting map in ggplot
ok.dataFrame = as.data.frame(ok$var1.pred, xy = T)
strtitle.main = 'Average Tempearure for August 2032'
strtitle.sub = 'Ensembles of GCMs under RCP4.5 emmission scenario'
ggplot(ok.dataFrame) + 
  geom_raster(aes(x,y, fill= var1.pred)) + 
  scale_fill_viridis_c(option  = "plasma") + 
  theme_minimal() + 
  labs(title = strtitle.main,
       subtitle = strtitle.sub, 
       x = "Longitude", 
       y = "Latitude", 
       fill = "Tempearure") +
  theme_bw()+
  coord_fixed(1.1)
dir2Save = dirName('Figures', 'SpatialAnalysis')
file_name = 'Month_Aug_2032_Ensemble_RCP45'
fileImgPng <- paste(dir2Save, '/', file_name, '.png', sep = '') 
ggsave(fileImgPng, plot = last_plot(), width = 8, height = 11)
dir2Save = dirName('ToGIS', 'SpatialAnalysis')
fileImgtiff <- paste(dir2Save, '/',file_name, '.tiff', sep = '') 
bf <- writeRaster(ok, filename=fileImgtiff, 
                  options="INTERLEAVE=BAND", overwrite=TRUE)




## Daily plot ---------------------
ok.result <- get_ordinary_kriging(model = 'ENSEMBLE', dir2Read, fileStudy,
                                  scale = 'Daily', stat = 'Mean', 
                                  attribute_name="X2032.08.30", emm_scene = 'RCP85')
ok <- ok.result$krig
# Plotting map in ggplot
ok.dataFrame = as.data.frame(ok$var1.pred, xy = T)
strtitle.main = 'Average Tempearure for 30 August 2032'
strtitle.sub = 'Ensembles of GCMs under RCP8.5 emmission scenario'
ggplot(ok.dataFrame) + 
  geom_raster(aes(x,y, fill= var1.pred)) + 
  scale_fill_viridis_c(option  = "plasma") + 
  theme_minimal() + 
  labs(title = strtitle.main,
       subtitle = strtitle.sub, 
       x = "Longitude", 
       y = "Latitude", 
       fill = "Tempearure") +
  theme_bw()+
  coord_fixed(1.1)
dir2Save = dirName('Figures', 'SpatialAnalysis')
file_name = 'Day_2032.08.30_Ensemble_RCP85'
fileImgPng <- paste(dir2Save, '/', file_name, '.png', sep = '') 
ggsave(fileImgPng, plot = last_plot(), width = 8, height = 11)
dir2Save = dirName('ToGIS', 'SpatialAnalysis')
fileImgtiff <- paste(dir2Save, '/',file_name, '.tiff', sep = '') 
bf <- writeRaster(ok, filename=fileImgtiff, 
                  options="INTERLEAVE=BAND", overwrite=TRUE)

