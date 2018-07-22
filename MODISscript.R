
#### Script to plot MODIS data GEOTIFF and raster calculations
#### Jerome Maleski
#### July 22 2018

rm(list = ls()) # clear the workspace


# data processing
library(foreign) # for reading dbfs
library(dplyr)
library(magrittr)
library(tidyr) 
library(ggplot2)
library(gridExtra) # to arrange grid plots

# spatial
library(raster)
library(rasterVis)
library(rgdal)
library(dismo) #map raster on Google Map


getwd() ## to check what your working directory is.

#land use classifications IGBP 
#https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mcd12_user_guide_v6.pdf
class_file <- "DataIn/MCD12Q1.006_LC_1_2010.tif"

#one modis NDVI 500m image
modistsp_file <- "DataIn/MOD13A1.006__500m_16_days_NDVI_doy2008001_aid0001.tif"

#read rasters into R
my_class <- raster(class_file)
my_raster <- raster(modistsp_file)

#plot Rasters
plot(my_class, axes=FALSE)
plot(my_raster, axes=FALSE)

#check clasification values, 12 is cropland
vals<- unique(values(my_class))
vals

##extract cropland NDVI values
#get where my_class == 12
D<-calc(my_class, fun = function(x) { x[x!=12] <- NA; return(x)})
plot(D, axes=FALSE)


#get cropland NDVI
cropland_NDVI<-overlay(my_raster,D, fun=function(x,y)(x*y))

plot(cropland_NDVI, axes=FALSE)

#get mean NDVI
mean_NDVI<-cellStats(cropland_NDVI, 'mean',na.rm=TRUE)

####create loop over all files
#get files list
files<-list.files(path=".", pattern='NDVI', full.names = TRUE, recursive=TRUE)

#creat vector to store results from loop
result <- vector("numeric", length(files))



for (i in 1:length(files)) {
  #read in each raster in turn
  my_raster<-raster(files[i])
  #get cropland NDVI
  cropland_NDVI<-overlay(my_raster,D, fun=function(x,y)(x*y))
  #get mean NDVI
  mean_NDVI<-cellStats(cropland_NDVI, 'mean',na.rm=TRUE)
  #store in vec
  result[i]<-mean_NDVI
}  


#create time series

NDVI <- ts(result/10000, start=c(2008, 1), end=c(2010, 12), frequency = 16)

plot(NDVI)


