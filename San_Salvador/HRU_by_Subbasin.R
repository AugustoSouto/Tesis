#IDENTIFICACIÓN DE LAS SUBCUENCAS Y SUS HRU#

rm(list=ls())
library(tidyve)


subbasin_map <-
rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/Results/subs.shp"
               #,
               #layer ="sub" 
               )

hrus_map <-
  rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/Results/hrus.shp"
                 #,
                 #layer ="sub" 
  )

list.files("C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Watershed")

hrus1_map <-
  rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Watershed/Shapes/hrus1.shp"
                 #,
                 #layer ="hrus1" 
  )

hrus1_map <-
  rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Watershed/Shapes/hrus1.shp" )

hrus2_map <-
  rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Watershed/Shapes/hrus2.shp")


library(raster)

raster::crs(subbasin_map) <- 
  CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

raster::crs(hrus_map) <- 
  CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

raster::crs(hrus1_map) <- 
  CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

raster::crs(hrus2_map) <- 
  CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")



plot(subbasin_map)

plot(hrus_map)

plot(hrus1_map)

plot(hrus2_map)

hrus1_map@data %>% head
hrus2_map@data %>% View

hru_list <-
hrus2_map@data$HRUS %>% str_split(patter=",")

hru_data <-
matrix(ncol=5, nrow=hru_list %>% length)

for (i in 1:dim(hru_data)[1]) {
  for (j in 1:dim(hru_data)[2]) {

    hru_data[i,j]<-hru_list[[i]][j]    
  }

}

hru_data<-
hru_data %>% as.data.frame()
colnames(hru_data)<- c("hru1", "hru2", "hru3", "hru4", "hru5")

hrus2_map@data <- cbind(hrus2_map@data, hru_data) 

sub_hru <- rbind(
  hrus2_map@data %>% dplyr::select(Subbasin, hru1) %>% rename(hru=hru1),
  hrus2_map@data %>% dplyr::select(Subbasin, hru2) %>% rename(hru=hru2),
  hrus2_map@data %>% dplyr::select(Subbasin, hru3) %>% rename(hru=hru3),
  hrus2_map@data %>% dplyr::select(Subbasin, hru4) %>% rename(hru=hru4)
  )

sub_hru <-
sub_hru %>% mutate(Subbasin=as.numeric(Subbasin),
                   hru=as.numeric(hru)) %>%
  arrange(hru) %>% filter(!is.na(hru))

setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador")
saveRDS(sub_hru, "sub_hru.RDS")

hrus2_map@data %>% group_by(Subbasin) %>% dplyr::select(hru1)



