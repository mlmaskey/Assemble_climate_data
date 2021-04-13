rm(list=ls())
cat("\014")
library(reshape2)
library(lubridate)
library(matrixStats)
library(lubridate)
library(zoo)
source('climate_functions.R')

modelList = c('ACCESS1',	'CANESM21',	'CAESM1BGC',
              'CCSM4',	'CMCC_CMS',	'CNRCCM5',
              'GFDL_CM3',	'HADGECC',	'HADGEES',	'MICRO5')
listFrame = read.csv('ListLocations.csv')

# # Processing the downloaded data----------
# process_data_V1(listFrame, modelList, dir_save='PROCESS', id_scene = 1)
# process_data_V1(listFrame, modelList, dir_save='PROCESS', id_scene = 2)
# # 
# # Aggregating the data in different scale and statistics------------------
# aggregate_climate_data(listFrame, modelList[1], dir_save='PROCESS', id_scene = 1)
# aggregate_climate_data(listFrame, modelList[1], dir_save='PROCESS', id_scene = 2)
# 
# #
# # Export for spatial Analysis ------------------
models = c(NA, modelList)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Annual', stat='min', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Month', stat='min', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Annual', stat='min', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Month', stat='min', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Annual', stat='mean', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Month', stat='mean', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Annual', stat='mean', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Month', stat='mean', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Annual', stat='max', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Month', stat='max', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Annual', stat='max', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Month', stat='max', modelList = models)

# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Daily', stat='min', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Daily', stat='mean', modelList = models)
# export_data(listFrame, dir_read ='PROCESS', id_scene=2, scale='Daily', stat='max', modelList = models)

export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Daily', stat='min', modelList = models)
export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Daily', stat='mean', modelList = models)
export_data(listFrame, dir_read ='PROCESS', id_scene=1, scale='Daily', stat='max', modelList = models)




