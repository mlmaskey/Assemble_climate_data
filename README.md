# Assembeling Climate data in time scale and perform spatial interpolation using ordinary kriging

## Steps
- Gathering the data
- Assembeling the data
- Converting data into spatial format
- Interpolate them spatially in different scale
- Visualize and save into GIS readable raster file

## Required files

- A file with list of cordinates
- Shapefile of study area

## Study area: Central Valley of California along Highway 99

## Required packages
- For primary data analytics:: reshape2, lubridate, matrixStats, and zoo 
- For spatial interpolation:: rgdal, raster, gstat, sf and ggplot2

## Custumized function
 - `process_data_V1(listFrame, modelList, dir_save', id_scene)`
 - `aggregate_climate_data(listFrame, modelList[1], dir_save, id_scene)`
 - `export_data(listFrame, dir_read, id_scene, scale, stat='min’, modelList)`
 - `get_ordinary_kriging(model, dir2Read, fileStudy,  scale, stat,  attribute_name, emm_scene)`



 
    
