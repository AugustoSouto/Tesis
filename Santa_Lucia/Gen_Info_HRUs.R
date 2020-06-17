#CREACION DE LOS DATOS NECESARIOS DE LAS HRU
rm(list = ls())

setwd("C:/SWAT/Rafael")

hru_files <- list.files("C:/SWAT/Rafael/SL_20200401/",
                        pattern = ".hru")
head(hru_files)

length(hru_files)

#info de la hru
readLines(paste0("C:/SWAT/Rafael/SL_20200401/",hru_files[1]),n=1)

#iniciar los vectores de las variables que necesito#
HRU_ID1 <- c()
HRU_ID2 <- c()
Subbasin <- c()
SLOPE_BAND <- c()

LANDUSE <- c()
SOIL_TYPE <- c()

setwd("C:/SWAT/Rafael")

for(i in 1:865){
  SLOPE_BAND = c(SLOPE_BAND,
                 strsplit(strsplit(readLines(paste0("./SL_20200401/",
                                                    hru_files[i]),n=1),
                                   split = ":")[[1]][7],split=" ")[[1]][2])
  LANDUSE = c(LANDUSE,
              strsplit(strsplit(readLines(paste0("./SL_20200401/",
                                                 hru_files[i]),n=1),
                                split = ":")[[1]][5],split=" ")[[1]][1])
  Subbasin = c(Subbasin,
               strsplit(strsplit(readLines(paste0("./SL_20200401/",
                                                  hru_files[i]),n=1),
                                 split = ":")[[1]][3],split=" ")[[1]][1])
  HRU_ID1 = c(HRU_ID1,
              strsplit(strsplit(readLines(paste0("./SL_20200401/",
                                                 hru_files[i]),n=1),
                                split = ":")[[1]][2],split=" ")[[1]][1])
  HRU_ID2 = c(HRU_ID2,
              strsplit(strsplit(readLines(paste0("./SL_20200401/",
                                                 hru_files[i]),n=1),
                                split = ":")[[1]][4],split=" ")[[1]][1])
  SOIL_TYPE = c(SOIL_TYPE,
                strsplit(strsplit(readLines(paste0("./SL_20200401/",
                                                   hru_files[i]),n=1),
                                  split = ":")[[1]][6],split=" ")[[1]][2])
}

Subbasin <- as.numeric(Subbasin)
HRU_ID1 <- as.numeric(HRU_ID1)
HRU_ID2 <- as.numeric(HRU_ID2)

unique(SOIL_TYPE)

unique(SLOPE_BAND)

unique(LANDUSE)
range(Subbasin)
range(HRU_ID1)
range(HRU_ID2)

HRUGIS <- substr(hru_files,1,9)

library(SWATplusR)
library(stringr)

t1 <- Sys.time()
run <- run_swat2012(project_path = "./SL_20200401",
                   output = list(area = define_output(file = "hru",
                                                      variable = "AREA",
                                                      unit = 1:865),
                                 LULC = define_output(file = "hru",
                                                      variable = "LULC",
                                                      unit = 1:865)),
                   start_date = as.Date("2006-01-01"),
                   end_date = as.Date("2015-12-31"),
                   years_skip = 4,
                   output_interval = "m"
)
t2 <- Sys.time()
t2-t1

cc <- substr(names(run),1,4)%in%c("date","LULC")
LULC <- run[cc]

head(names(LULC))
unique(LULC$LULC_800)
unique(LULC$LULC_400)

cc <- substr(names(run),1,4)%in%c("area")
hru_area <- run[cc]

head(names(hru_area))

HRU_AREA_km2 <- unlist(lapply(hru_area, function(x){as.numeric(unique(x))}))

sum(HRU_AREA_km2)

hru_info <- data.frame(HRU_ID1,
                       HRU_ID2,
                       Subbasin,
                       SLOPE_BAND,
                       LANDUSE,
                       SOIL_TYPE,
                       HRUGIS,
                       HRU_AREA_km2)

"C:/SWAT/Analysis.RData"
saveRDS(hru_info,"hru_info.RDS")

saveRDS(LULC,"LULC_2006_2015.RDS")
