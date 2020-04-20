##############################################################################################################
# This script allows to extract the total area and polygone features 
# (ex : lakes) among buffers around points.
# It returns a data table containing the point IDs and the values obtained
# for the area (in km²) among buffers.
##############################################################################################################

# path_points : path to open the point file (ex : "C:/Users/Isabelle Le Viol/Documents/Points.shp")
# Col_ID : column name of the point layer containing the site IDs (ex : "CODE_SITE")
# bufferSize : size of the buffer in meters (ex : 2000)
# path_poly : path to open the polygone file (ex : "C:/Users/Isabelle Le Viol/Documents/Poly.shp")

# how to call the function (ex):
# path_sites <- "C:/Users/Isabelle Le Viol/Documents/Points.shp"
# sites_ID <- "CODE_SITE"
# buffer_in_meters <- 2000
# path_lakes <- "C:/Users/Isabelle Le Viol/Documents/Poly.shp"

# Tab_lakes <- Calc_linear_density_in_buffers (path_sites,sites_ID,buffer_in_meters,path_lakes)

Calc_area_in_buffers <- function (path_points,Col_ID,bufferSize,path_poly){
  
  #required package
  if (!("sf" %in% installed.packages()[, "Package"])){
    install.packages("sf", dependencies = TRUE)
  }
  library(sf)
  #open points data
  shp_points <- st_read(path_points)
  shp_points <- st_transform(shp_points,crs=2154) #projection in Lambert 93 if not
  
  #creation of buffers around points
  shp_buffer <- st_buffer(shp_points[,c(Col_ID,"geometry")],bufferSize)
  
  #open polygons data
  shp_poly <- st_read(path_poly)
  shp_poly <- st_transform(shp_poly,crs=2154) #projection in Lambert 93 if not
  
  #intersection between linear elements and buffers
  shp_intersec <- st_intersection(shp_poly,shp_buffer)
  shp_intersec$area <- st_area(shp_intersec) #calculation of the area of each element

  #total by buffers
  sum_by_buffers <- by(shp_intersec$area,shp_intersec[,which(colnames(shp_intersec)==Col_ID)][[1]], FUN = sum, na.rm = T)
  polyBuf <- data.frame(names(sum_by_buffers))
  colnames(polyBuf) <- "Code_site"
  polyBuf$area_km2 <- as.numeric(sum_by_buffers)/10^6
  polyBuf$area_km2[is.na(polyBuf$area_km2)]=0
  
  return(polyBuf)
  
}
