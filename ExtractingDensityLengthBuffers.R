##############################################################################################################
# This script allows to extract the total length and the density of linear features 
# (ex : roads or rivers) among buffers around points.
# It returns a data table containing the point IDs and the values obtained
# for the total length (in km) and the density (in km/km²) among buffers.
##############################################################################################################

### Arguments ###

# points_shp_or_path : either the path to open the point file (ex : "C:/Users/Prenom Nom/Documents/Points.shp")
# or the point file already opened in your script

# Col_ID : column name of the point layer containing the site IDs (ex : "CODE_SITE")

# Vect_bufferSize : size of the buffers in meters (ex : 50, 500 and 1000m)

# lines_shp_or_path : either the path to open the line file (ex : "C:/Users/Prenom Nom/Documents/Lines.shp")
# or the line file already opened in your script

### How to call the function (example): ###

# path_sites <- "C:/Users/Prenom Nom/Documents/Points.shp"
# sites_ID <- "CODE_SITE"
# buffers_in_meters <- c(50,500,1000)
# path_roads <- "C:/Users/Isabelle Le Viol/Documents/Lines.shp"

# Tab_roads <- Calc_linear_density_in_buffers (path_sites,sites_ID,buffers_in_meters,path_roads)

##############################################################################################################

Calc_linear_density_in_buffers <- function (points_shp_or_path,Col_ID,Vect_bufferSize,lines_shp_or_path){
    
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
  
  #open lines data
  if (all(class(lines_shp_or_path)=="character")){
    shp_lines <- st_read(lines_shp_or_path) 
  } else if (all(class(lines_shp_or_path)==c("sf","data.frame"))){
    shp_lines <- lines_shp_or_path
  } else {
    cat("lines_shp_or_path has to be either the path to the polygon file (i.e. a character)\nor a shp file created with the sf package (i.e. an object whose class is c(sf,data.frame)")
  }
  shp_lines <- st_transform(shp_lines,crs=2154) #projection in Lambert 93 if not
  
  #table to fill
  linearBuf <- data.frame(data.frame(shp_points[,Col_ID])[,Col_ID])
  colnames(linearBuf) <- "Code_site"
  
  for (buffer_size in Vect_bufferSize){

    #creation of two new columns for the buffer size 
    linearBuf[,dim(linearBuf)[2]+1] <- rep(0,dim(linearBuf)[1])
    colnames(linearBuf)[dim(linearBuf)[2]] <- paste0("linear_buffer_length_",buffer_size)
    linearBuf[,dim(linearBuf)[2]+1] <- rep(0,dim(linearBuf)[1])
    colnames(linearBuf)[dim(linearBuf)[2]] <- paste0("linear_buffer_density_",buffer_size)
    
    #creation of buffers around points
    shp_buffer <- st_buffer(shp_points[,c(Col_ID,"geometry")],buffer_size)
    
    #intersection between polygons elements and buffers
    shp_intersec <- st_intersection(shp_lines,shp_buffer)
    shp_intersec$length <- st_length(shp_intersec) #calculation of the length of each linear element 
    
    #sum by buffers
    sum_by_buffers <- by(shp_intersec$length,shp_intersec[,which(colnames(shp_intersec)==Col_ID)][[1]], FUN = sum, na.rm = T)
    linearBuf[match(names(sum_by_buffers), linearBuf$Code_site),dim(linearBuf)[2]-1]  <- sum_by_buffers/1000
    linearBuf[match(names(sum_by_buffers), linearBuf$Code_site),dim(linearBuf)[2]]  <- as.numeric(sum_by_buffers/1000) / (st_area(shp_buffer[1,])/10^6)
  
    print(buffer_size)
    
    }
  
  return(linearBuf)

}
