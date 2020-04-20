##############################################################################################################
# This script allows to extract the total length and the density of linear features 
# (ex : roads or rivers) among buffers around points.
# It returns a data table containing the point IDs and the values obtained
# for the total length (in km) and the density (in km/km²) among buffers.
##############################################################################################################

# path_points : path to open the point file (ex : "C:/Users/Isabelle Le Viol/Documents/Points.shp")
# Col_ID : column name of the point layer containing the site IDs (ex : "CODE_SITE")
# bufferSize : size of the buffer in meters (ex : 2000)
# path_lines : path to open the line file (ex : "C:/Users/Isabelle Le Viol/Documents/Lines.shp")

# how to call the function (ex):
# path_sites <- "C:/Users/Isabelle Le Viol/Documents/Points.shp"
# sites_ID <- "CODE_SITE"
# buffer_in_meters <- 2000
# path_roads <- "C:/Users/Isabelle Le Viol/Documents/Lines.shp"

# Tab_roads <- Calc_linear_density_in_buffers (path_sites,sites_ID,buffer_in_meters,path_roads)

Calc_linear_density_in_buffers <- function (path_points,Col_ID,bufferSize,path_lines){
  
  #required package
    if (!("sf" %in% installed.packages()[, "Package"])){
    install.packages("sf", dependencies = TRUE)
  }
  library(sf)

  #open point layer
  shp_points <- st_read(path_points)
  shp_points <- st_transform(shp_points,crs=2154) #projection in Lambert 93 if not
  
  #creation of buffers around points
  shp_buffer <- st_buffer(shp_points[,c(Col_ID,"geometry")],bufferSize)

  #open line layer
  shp_lines <- st_read(path_lines)
  shp_lines <- st_transform(shp_lines,crs=2154) #projection in Lambert 93 if not
  
  #intersection between lines and buffers
  shp_intersec <- st_intersection(shp_lines,shp_buffer)
  shp_intersec$length <- st_length(shp_intersec) #calculation of the length of each linear element 
  
  #total by buffers
  sum_by_buffers <- by(shp_intersec$length,shp_intersec[,which(colnames(shp_intersec)==Col_ID)][[1]], FUN = sum, na.rm = T)
  linearBuf <- data.frame(names(sum_by_buffers))
  colnames(linearBuf) <- "Code_site"
  linearBuf$len_km <- as.numeric(sum_by_buffers)/1000
  linearBuf$density <- linearBuf$len_km / as.numeric(st_area(shp_buffer[1,])/10^6)
  
  return(linearBuf)

}
