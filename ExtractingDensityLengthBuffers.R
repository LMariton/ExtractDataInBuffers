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
  
  # ##########################
  # rm(list=ls())
  # 
  # #Participations x nights selected
  # Nights50 <- read.csv2("C:/Users/Lea_Mariton/Documents/These/Article_a_lancer/Vigie_chiro_opening/CSV_First_treatment/Nights50_all_filters.csv")
  # str(Nights50)
  # options(digits.secs = 3)
  # Nights50$Nuit <- parse_date_time(Nights50$Nuit,"%Y-%m-%d", tz="Europe/Paris")
  # Nights50$sunrise <- parse_date_time(Nights50$sunrise,"%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  # Nights50$sunset <- parse_date_time(Nights50$sunset,"%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  # Nights50$premier_contact <- parse_date_time(Nights50$premier_contact,"%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  # Nights50$dernier_contact <- parse_date_time(Nights50$premier_contact,"%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  # Nights50$date_deb_obs <- parse_date_time(Nights50$date_deb_obs,"%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  # Nights50$date_fin_obs <- parse_date_time(Nights50$date_fin_obs,"%Y-%m-%d %H:%M:%S", tz="Europe/Paris")
  # 
  # #Participation and sites selected
  # part_select <- read.csv2("C:/Users/Lea_Mariton/Documents/These/Article_a_lancer/Vigie_chiro_opening/CSV_Filtering/Participations_selection.csv")
  # part_select$site_point <- paste0(part_select$idsite,"_",part_select$point)
  # part_select <- part_select[which(part_select$participation %in% Nights50$participation),]
  # 
  # site_select <- read.csv2("C:/Users/Lea_Mariton/Documents/These/Article_a_lancer/Vigie_chiro_opening/CSV_Filtering/Sites_selection.csv")
  # site_select$site_point <- paste0(site_select$id_site,"_",site_select$nom)
  # site_select <- site_select[which(site_select$site_point %in% part_select$site_point),]
  # 
  # #SHP Sites
  # SHP_site_select <- st_as_sf(site_select, coords = c("longitude","latitude"), crs = 4326)
  # SHP_site_select <- st_transform(SHP_site_select,crs=2154)
  # points_shp_or_path <- SHP_site_select
  # 
  # Col_ID <- "site_point"
  # Vect_bufferSize <- c(50,500,1000)
  # lines_shp_or_path <- "C:/Users/Lea_Mariton/Documents/These/Data/Data_SIG/BD_Carthage_2017/BD_Carthage_2017_Fr/CoursEau_FXX.shp"

  ##########################
  
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
