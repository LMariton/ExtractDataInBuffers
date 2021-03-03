##############################################################################################################
# This script allows to extract the radiance from the VIIRS data for given sites and the mean radiance in 
# buffers around sites
# It returns the table given as an argument with one column for the VIIRS value at the points and as many 
# columns as the number of buffers given
##############################################################################################################

### Arguments ###

# tableSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS82
# -> a column named "Longitude" with the site longitude in WGS82
# -> a column named "Site_ID" with the sites unique IDs
#
# vect_buffer_size : vector with all the buffers sizes (in meters) for which a mean radiance should be 
# calculated (e.g. c(50,100,1000,5000))
#
# path_VIIRS93 : the path to the VIIRS raster layer. This layer has to be download at this link : 
# https://eogdata.mines.edu/download_dnb_composites.html
# name : SVDNB_npp_20160101-20161231_75N060W_vcm-orm-ntl_v10_c201807311200.avg_rade9
# /!\ it has to be reprojected in Lambert93 (thanks to QGis or ArcGis for example)

##############################################################################################################

extract_VIIRS <- function(tableSites){
  
  rm(list=ls())
  tableSites <- read.csv("C:/Users/Lea_Mariton/Desktop/Tab_Pt_Fixe90.csv")
  vect_buffer_size <- c(50,100)
  path_VIIRS <- "C:/Users/Lea_Mariton/Documents/These/Data/Data_SIG/VIIRS/VIIRS_Europe/VIIRS93.tif"
  
  
  #load packages
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("raster","sf")
  load(packages)
  
  #raster opening
  VIIRS_data <- raster(path_VIIRS)
  
  #creation of a site layer
  sites_SHP <- st_as_sf(tableSites, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  #Sunrise and sunset time
  calc_sunrise_set <- function(x,y,z){
    tabsun <- sunrise.set(x,y,z,num.days = 2)
    tabsun[1,1] <- tabsun[2,1]
    tabsun <- tabsun[-2,]
    return(as.vector(tabsun))
  }
  
  tableNightsSites$sunrise <- rep(NA,dim(tableNightsSites)[1])
  tableNightsSites$sunset <- rep(NA,dim(tableNightsSites)[1])
  
  tableNightsSites[,c("sunrise","sunset")] <- as.data.frame(t(mapply(calc_sunrise_set,tableNightsSites$Latitude,tableNightsSites$Longitude,tableNightsSites$Date)))
  tableNightsSites$sunset <- as_datetime(as.numeric(tableNightsSites$sunset),tz="Europe/Paris")
  tableNightsSites$sunrise <- as_datetime(as.numeric(tableNightsSites$sunrise),tz="Europe/Paris")
  
  return(tableNightsSites)
  
}