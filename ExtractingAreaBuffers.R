##############################################################################################################
# This script allows to extract the total area of polygon features 
# (ex : lakes) among buffers around points.
# It returns a data table containing the point IDs and the values obtained
# for the area (in km²) among buffers.
##############################################################################################################

### Arguments ###

# points_shp_or_path : either the path to open the point file (ex : "C:/Users/Prenom Nom/Documents/Points.shp")
# or the point file already opened in your script

# Col_ID : column name of the point layer containing the site IDs (ex : "CODE_SITE")

# Vect_bufferSize : size of the buffers in meters (ex : 50, 500 and 1000m)

# poly_shp_or_path : either the path to open the polygon file (ex : "C:/Users/Prenom Nom/Documents/Poly.shp")
# or the polygon file already opened in your script

### How to call the function (example): ###

# path_sites <- "C:/Users/Prenom Nom/Documents/Points.shp"
# sites_ID <- "CODE_SITE"
# buffers_in_meters <- c(50,500,1000)
# path_lakes <- "C:/Users/Isabelle Le Viol/Documents/Poly.shp"

# Tab_lakes <- Calc_area_in_buffers (path_sites,sites_ID,buffers_in_meters,path_lakes)

##############################################################################################################

Calc_area_in_buffers <- function (points_shp_or_path,Col_ID,Vect_bufferSize,poly_shp_or_path){
  
  #required package
  if (!("sf" %in% installed.packages()[, "Package"])){
    install.packages("sf", dependencies = TRUE)
  }
  library(sf)
  
  #open points data
  if (all(class(points_shp_or_path)=="character")){
    shp_points <- st_read(points_shp_or_path)  
  } else if (all(class(points_shp_or_path)==c("sf","data.frame"))){
    shp_points <- points_shp_or_path
  } else {
    cat("points_shp_or_path has to be either the path to the point file (i.e. a character)\nor a shp file created with the sf package (i.e. an object whose class is c(sf,data.frame)")
  }
  shp_points <- st_transform(shp_points,crs=2154) #projection in Lambert 93 if not
  
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
