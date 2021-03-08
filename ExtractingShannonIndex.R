##############################################################################################################
# This script allows to extract the Shannon index for given sites among different buffer size
# It returns the table given as an argument with one column for the VIIRS value at the points (VIIRS) and as 
# many columns as the number of buffers given (VIIRS_bufferXXXX with X buffer size in meters)
#
##############################################################################################################
#
# Land-use layers used available here: http://osr-cesbio.ups-tlse.fr/echangeswww/TheiaOSO/OCS_2018_CESBIO.tif
#
##############################################################################################################
#
### Arguments ###
#
# tableSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS82
# -> a column named "Longitude" with the site longitude in WGS82
#
# vect_buffer_size : vector with all the buffers sizes (in meters) for which a Shannon index should be 
# calculated (e.g. c(50,100,1000,5000))
#
# Hab_path = directory of raster location (only one) 
#
##############################################################################################################

index_shannon_buffer <- function(tableSites,bufwidth,Hab_path){
  
  ####Preliminary####
  
  #Install and open required packages
  
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("landscapemetrics","sf","raster")
  load(packages)
  
  #creation of a site layer
  sites_SHP <- st_as_sf(tableSites, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  #Open habitat raster layer
  Hab <- raster(Hab_path)
  
  for (buffer_size in bufwidth) {
    tableSites[dim(tableSites)[2]+1]<- rep(NA,dim(tableSites)[1])
    colnames(tableSites)[dim(tableSites)[2]] <- paste0("Shannon_",buffer_size)

    for (index in c(1:dim(tableSites)[1])){
      shp_buffer <- st_buffer(sites_SHP[index,],buffer_size)
      raster_buffer <- mask(crop(Hab,shp_buffer),shp_buffer)
      tableSites[index,which(colnames(tableSites)==paste0("Shannon_",buffer_size))] <- as.numeric(lsm_l_shdi(raster_buffer)[6])
    
      cat(paste0(round(index/dim(tableSites)[1]*100,digits=1),"%\r")) #print the percentage of files treated
    }
    
    print(paste0("Buffer ",buffer_size,"m : OK"))
  }
  
  return(tableSites)
}