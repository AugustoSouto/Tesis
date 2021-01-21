rm(list=ls())
library(sp)
library(maptools)

hru_info <-
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/hru_info.RDS")

subs <- 
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/subs.RDS")
# Generalidades

## Uso del suelo y rotaciones
#Se consideran 11 categorías de uso de suelo y 7 rotaciones.


luse = aggregate(area~landuse+rot,data = hru_info, FUN=sum)
luse$area = round(luse$area/sum(luse$area)*100,2)
luse$rot[luse$rot==luse$landuse] = ""

#Se trabaja con 7 tipos de rotaciones. La siguiente imagen muestra la distribución espacial de cada rotación por subcuenca así como el uso del suelo para aquellas zonas sin rotación de cultivos.

library(maptools)
library(sp)
library(mapplots)

hru_info <-
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/hru_info.RDS")

subs <- 
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/subs.RDS")

xy = coordinates(subs)
hru_info$lat = NA
hru_info$lon = NA
hru_info_sb = aggregate(area~rot+subbasin, data=hru_info, FUN=sum)
hru_info_sb$lon = NA
hru_info_sb$lat = NA
for(i in 1:nrow(hru_info_sb)){
  hru_info_sb$lat[i] = xy[hru_info_sb$subbasin[i],2]
  hru_info_sb$lon[i] = xy[hru_info_sb$subbasin[i],1]
}
xyz <- make.xyz(hru_info_sb$lon,hru_info_sb$lat,hru_info_sb$area,hru_info_sb$rot)

#cc = rainbow((ncol(xyz$z)))
cc = c("orange","deeppink","red","darkmagenta","yellow",
       "gold","darkgreen","burlywood",
       "darkolivegreen","black","cyan")

plot(subs, axes=F)
plot(subs)
draw.pie(xyz$x, xyz$y, xyz$z, radius = 5000, col=cc)

legend.pie(460000,6290000,labels=colnames(xyz$z), radius=7000, bty="n", col=cc,
           cex=0.8, label.dist=1.3)

legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE)),0)

legend.bubble(390000,6250000,z=c(3500,1500,500),round=1,maxradius=5000,bty="n",txt.cex=0.6)

text(390000,6258000,"área (ha)",cex=0.8) 


saveRDS(myhru,"C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/hru_info.RDS")

