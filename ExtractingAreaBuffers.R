##############################################################################################################
# This script allows to extract the total area of polygon features 
# (ex : lakes) among buffers around points.
# It returns a data table containing the point IDs and the values obtained
# for the area (in km²) among buffers.
##############################################################################################################

### Arguments ###

# tableSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS82
# -> a column named "Longitude" with the site longitude in WGS82
# -> a column with the sites unique IDs

# Col_ID : column name of the table containing the site IDs (ex : "CODE_SITE")

# Vect_bufferSize : size of the buffers in meters (ex : 50, 500 and 1000m)

# poly_shp_or_path : either the path to open the polygon file (ex : "C:/Users/Prenom Nom/Documents/Poly.shp")
# or the polygon file already opened in your script

##############################################################################################################

Calc_area_in_buffers <- function (tableSites,Col_ID,Vect_bufferSize,poly_shp_or_path){
  
  ##################
  # rm(list=ls())
  # Nights90 <- read.csv("C:/Users/Lea_Mariton/Documents/These/Data/Vigie_Chiro/Pour_CK/Tab_Pt_Fixe90.csv")
  # site_Nights90 <- distinct(Nights90[,which(colnames(Nights90) %in% c("site_point","Longitude","Latitude"))])
  # tableSites <- site_Nights90
  # Col_ID <- "site_point"
  # Vect_bufferSize <- c(1000)
  # poly_shp_or_path <- "D:/Data/Data_SIG/BD_Carthage_2017/BD_Carthage_2017_Fr/PlanEau_FXX.shp/PlanEau_FXX.shp"
  
  #################
  
  #required package
  if (!("sf" %in% installed.packages()[, "Package"])){
    install.packages("sf", dependencies = TRUE)
  }
  library(sf)
  
  #creation of a site layer
  shp_points <- st_as_sf(tableSites, coords = c("Longitude","Latitude"), crs = 4326)
  shp_points <- st_transform(shp_points,crs=2154)
  
  #open polygons data
  if (all(class(poly_shp_or_path)=="character")){
    shp_poly <- st_read(poly_shp_or_path) 
  } else if (all(class(poly_shp_or_path)==c("sf","data.frame"))){
    shp_poly <- poly_shp_or_path
  } else {
    cat("poly_shp_or_path has to be either the path to the polygon file (i.e. a character)\nor a shp file created with the sf package (i.e. an object whose class is c(sf,data.frame)")
  }
  shp_poly <- st_transform(shp_poly,crs=2154) #projection in Lambert 93 if not
  
  #table to fill
  polyBuf <- data.frame(data.frame(shp_points[,Col_ID])[,Col_ID])
  colnames(polyBuf) <- "Code_site"
  
  for (buffer_size in Vect_bufferSize){

    #creation of a new column for the buffer size 
    polyBuf[,dim(polyBuf)[2]+1] <- rep(0,dim(polyBuf)[1])
    colnames(polyBuf)[dim(polyBuf)[2]] <- paste0("poly_buffer_",buffer_size)
    
    #creation of buffers around points
    shp_buffer <- st_buffer(shp_points[,c(Col_ID,"geometry")],buffer_size)
    
    #intersection between polygons elements and buffers
    shp_intersec <- st_intersection(shp_poly,shp_buffer)
    shp_intersec$area <- st_area(shp_intersec) #calculation of the area of each element
    
    #sum by buffers
    sum_by_buffers <- by(shp_intersec$area,shp_intersec[,which(colnames(shp_intersec)==Col_ID)][[1]], FUN = sum, na.rm = T)
    polyBuf[match(names(sum_by_buffers), polyBuf$Code_site),dim(polyBuf)[2]]  <- sum_by_buffers/10^6

    print(buffer_size)
    
  }
  
  return(polyBuf)
  
}
