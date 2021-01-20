rm(list = ls())

library(foreign)
hru = read.dbf("C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Watershed/Shapes/hrus2.dbf")

hru = hru[,c("Subbasin", "HRUS")]
con = strsplit(paste(hru$HRUS),split = ", ")

myhru = matrix(ncol=2,nrow = 0)
for(i in 1:length(con)){
  for(j in 1:length(con[[i]])){
    myhru = rbind(myhru,
                  as.numeric(c(hru$Subbasin[i],con[[i]][j]))) 
  }
}

myhru = myhru[order(myhru[,2]),]
myhru = myhru[-1,]

colnames(myhru) = c("subbasin","hru")
myhru = data.frame(myhru)

ar = read.fwf("C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/hru.con",widths = c(37,14), n=5204, colClasses=c("NULL", "numeric"), skip = 2)
myhru$area = ar$V2

head(myhru)

rot = read.fwf("C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/hru-data.hru",skip = 2,widths = c(81,18),colClasses=c("NULL", "character"))
rot = rot$V2
rot = gsub("\\s", "", rot)

myhru$landuse = rot


head(myhru)
#################################################
library("RSQLite")

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/SSalvador_LU2018_v0.sqlite")



## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

tables[134]
landuse_tab = dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[134]], "'", sep=""))[,c("id","name","mgt_id")]

tables[139]
mgt_tab = dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[139]], "'", sep=""))


landuse_tab

myhru$mgt_id = landuse_tab$mgt_id[match(myhru$landuse,landuse_tab$name)]
myhru$rot = mgt_tab$name[myhru$mgt_id]
myhru$rot = ifelse(is.na(myhru$rot),myhru$landuse, myhru$rot)


summary(myhru)

unique(myhru$rot)

saveRDS(myhru,"C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/hru_info.RDS")
