
rm(list=ls())

# Packages 
require(tidyverse)
require(lubridate)
require(hydromad)
library(foreign)
library(ggplot2)
library(data.table)
library(pander)


#Cambiar carpeta de trabajo POR QU? YA NO ME LO RECONOCE?? SI YA SE LA DI EN ROOT.DIR PREGUNTAR!!!
setwd("C:/SWAT/Florencia/CCa3")
#setwd("C:/Users/fhastings/Documents/BIBLIOGRAFIA/SWAT/SWAT+/robit_demo_1.2.0/Robit_demo_v2")

#Importo la tabla de resultados por HRU
c_HRUnames <- read.table("Scenarios/Default/TxtInOut/hru_pw_day.txt", skip = 1, nrows=1, colClasses = "character")
outputHRU <- read.table("Scenarios/Default/TxtInOut/hru_pw_day.txt", skip = 3, col.names= c_HRUnames) 

#Importo tablas con informaci?n de los HRu para asignar el Landuse y el area de cada HRU
HRU_colnames <- read.table("Scenarios/Default/TxtInOut/hru.con", skip = 1, nrows=1, colClasses = "character") 
HRU_con <- read.table("Scenarios/Default/TxtInOut/hru.con", skip = 2, col.names= HRU_colnames) 

HRU_datanames <- read.table("Scenarios/Default/TxtInOut/hru-data.hru", skip = 1, nrows=1, colClasses = "character") 
HRU_data <- read.table("Scenarios/Default/TxtInOut/hru-data.hru", skip = 2, col.names= HRU_datanames) 
HRU_data$lu_mgt %>% unique

yi= min(outputHRU$yr) #ano inicio simulaci?n (sin contar warm up)
yf= max(outputHRU$yr) #ano fin simulaci?n
HRUs= max(outputHRU$gis_id); HRUs

#Importo la salida mgt_out para relacionar el HRU con cultivo en un momento dado. Filtro los datos desde el a?o yi
mgtnames <- read.table("Scenarios/Default/TxtInOut/mgt_out.txt", skip = 1, nrows=1, colClasses = "character")
mgt_out <- read.table("Scenarios/Default/TxtInOut/mgt_out.txt", skip = 3, fill=T, col.names= mgtnames)  #fill=T porque hay columnas que no estan completas en todas las filas, se llenan con NA


# y agrego una columna date
outputHRU <- merge(outputHRU, select(HRU_data, name, soil, lu_mgt), by="name", all=T) %>%
  mutate(date = as.Date(paste(yr, jday), format = "%Y %j")) %>%
  merge(HRU_con[,3:4], by="gis_id", all.x=T)


#Selecciono solo algunas variables
crop_hru <- outputHRU %>%
  select(date, yr, hru=gis_id, hruname=name, area, lu_mgt, lai, bioms, yield, residue, strsw, strsa, strstmp, strsn, strsp)  %>%
  mutate(date_hru= paste(date, hru, sep="_"))

#Voy a filtrar el ultimo cultivo que se plant? en cada hru el ultimo a?o de warm up
#Agregue la categoria Sin Plant (podria ser GRAS, EUCA, MONT)
crop_wup <- mgt_out  %>% filter (year == (yi-1)) %>% filter (operation == "PLANT") %>% group_by(hru) %>% summarise(crop=as.character(last(crop.fert.pest))) %>%
  complete(hru = seq(1, HRUs))  %>% 
  merge(HRU_data[,c(1,6)], by.x="hru", by.y="id", all.x=T) %>%
  mutate(crop= ifelse(is.na(crop), as.character(lu_mgt),crop))

#En la tabla mgt_out filtro las operaciones plant y harvest
mgt_out <- mgt_out %>% filter(operation != "FERT") %>% 
  mutate(date = as.Date(paste(year, mon, day, sep="-")), date_hru = paste(date, hru, sep="_")) %>%
  filter(year>= yi)


#Voy a unir las columnas "crop.fert.pest" donde aparece el nombre del cultivo que se planta y/o cosecha
crop_hru <- merge(crop_hru, (mgt_out %>% select(date_hru, crop=crop.fert.pest, operation)), by= "date_hru", all.x=T)

#voy a darle el cultivo inicial a cada hru
crop_hru <- crop_hru[order(crop_hru$date, crop_hru$hru),]
crop_hru$crop <- as.character(crop_hru$crop)
crop_hru$crop[1:HRUs] = crop_wup$crop

crop_hru <- crop_hru[order(crop_hru$hru, crop_hru$date),] %>%
  fill(`crop`) %>% mutate(crop= ifelse((bioms==0 & yield==0 & is.na(operation)),NA ,as.character(crop)))

crop_hru %>%
  group_by(date,lu_mgt) %>%
  summarise(BiomHRUm=mean(bioms), BiomHRUwm=weighted.mean(bioms,area), crop=first(crop)) %>%
  #filter(lu_mgt=="agr1_lum") %>%
  filter( crop=="corn"  ) %>%
  ggplot(aes(date, BiomHRUwm, colour=crop)) + geom_line()+
  facet_wrap(~crop, scales ="free")


crop_hru %>%
  group_by(date,lu_mgt) %>%
  summarise(BiomHRUwm=weighted.mean(bioms,area), crop=first(crop)) %>%
  # filter(lu_mgt=="agr6_lum"& yr>=2007 & yr<2010) %>%
  filter( crop=="corn" & date>="2006-09-21" & date<"2007-02-28" ) %>%
  ggplot(aes(date, BiomHRUwm, colour=crop)) + geom_line()


crop_hru %>%
  group_by(date,lu_mgt) %>%
  summarise(LaiHRUm=mean(lai), LaiHRUwm=weighted.mean(lai,area), crop=first(crop)) %>%
  #filter(lu_mgt=="agr5_lum") %>%
  filter( crop=="corn") %>%
  ggplot(aes(date, LaiHRUwm, colour=crop)) + geom_line()+
  facet_wrap(~crop, scales ="free")

crop_hru %>%
  group_by(date,lu_mgt) %>%
  summarise(LaiHRUm=mean(lai), LaiHRUwm=weighted.mean(lai,area), crop=first(crop)) %>%
  #filter(lu_mgt=="agr5_lum") %>%
  filter( crop=="corn" & date>="2006-09-21" & date<"2007-02-28" ) %>%
  ggplot(aes(date, LaiHRUwm, colour=crop)) + geom_line()


Yld <- crop_hru %>%
  mutate(op_crop=paste(operation,crop,sep="_")) %>%
  group_by(yr, op_crop) %>%
  filter(yield>0) %>%
  summarise(YldHRUwm=weighted.mean(yield,area), 
            crop=first(crop)) %>%
  filter(crop=="corn") 

Yld %>%
  ggplot(aes(yr, YldHRUwm, fill=crop)) + geom_col(position = "dodge")#+
facet_wrap(~crop, scale= "free")

#saco los estadisticos
pander(crop_hru %>% mutate(op_crop=paste(operation,crop,sep="_")) %>% 
         group_by(yr,op_crop) %>% filter(yield>0)  %>% filter(crop=="corn") %>% 
         summarise(YldHRUwm=weighted.mean(yield,area), crop=first(crop))) 


#Genero las columnas strs_value y strs con el valor m?ximo de strs y su causa

crop_hru %>%
  mutate(strs= colnames(crop_hru[,12:16])[max.col(crop_hru[,12:16],ties.method="first")], strs_val= pmax(strsw, strsa, strstmp, strsn, strsp)) %>%
  filter(!is.na(crop)) %>%
  filter(crop=="corn") %>%
  group_by(strs) %>%
  ggplot()+ geom_histogram(aes(strs_val, fill= strs), position = "dodge", bins = 4)+  facet_wrap(~strs)

crop_hru %>%
  mutate(strs= colnames(crop_hru[,12:16])[max.col(crop_hru[,12:16],ties.method="first")], strs_val= pmax(strsw, strsa, strstmp, strsn, strsp)) %>%
  filter(crop=="corn") %>%
  ggplot()+ geom_histogram(aes(strs_val, fill= strs), position = "dodge", bins = 4)

crop_hru %>%
  filter(!is.na(crop)) %>%
  filter(crop=="corn" & date>="2006-09-21" & date<"2007-02-28" ) %>%
  ggplot()+ geom_point(aes(x=date, y=strsw, colour= "strsw"))+
  geom_point(aes(x=date, y=strstmp, colour= "strstmp"))+
  geom_point(aes(x=date, y=strsn, colour= "strsn"))+
  geom_point(aes(x=date, y=strsp, colour= "strsp"))+
  geom_point(aes(x=date, y=strsa, colour= "strsa"))

RendObs <- read.csv("C:/Users/fhastings/Documents/INNOVAGRO/2.-InformacionBase/Cultivos/Rendimientos.csv")


CropSwatObs <- crop_hru %>%
  mutate(op_crop=paste(operation,crop,sep="_")) %>%
  group_by(yr,op_crop) %>%
  filter(yield>0) %>%
  summarise(YldHRUwm=weighted.mean(yield,area), 
            crop=first(crop)) %>%
  filter(crop =="corn") %>%
  merge (RendObs, by="yr") 


YieldSwatObsINIA_lm<- lm(YldHRUwm ~ Corn1_INIALE, data = CropSwatObs)
print("sd Yield promedio INIA-LE")
sd(YieldSwatObsINIA_lm$residuals)

YieldSwatObsCREA_lm<- lm(YldHRUwm ~ Corn1_CREA, data = CropSwatObs)
print("sd Yield promedio CREA")
sd(YieldSwatObsCREA_lm$residuals)


CropSwatObs %>%
  ggplot() + geom_point(aes(YldHRUwm, Corn1_CREA, colour="CREA"))+
  geom_point(aes(YldHRUwm, Corn1_INIALE, colour="INIA"))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+coord_fixed()
#+  xlim(2500,10000)+ ylim(2500,10000)

CropSwatObs %>%
  ggplot() + geom_point(aes(yr, YldHRUwm, colour="Swat"))+
  geom_point(aes(yr, Corn1_CREA, colour="CREA"))+
  geom_point(aes(yr, Corn1_INIALE, colour="INIA"))



#saco los estadisticos
#Miro la biomasa el d?a antes de la cosecha

#Agrego en la columna operation "PrintBiom" para filtrar la biomasa un d?a antes de la cosecha
PrintBiom <- crop_hru %>%
  filter(operation=="HARV/KILL") %>%
  mutate(date_hru = paste((date-1), hru, sep="_"), operation="PrintBiom") %>%
  group_by(date_hru) %>%
  summarise(operation=first(operation))

crop_hru <- crop_hru %>% 
  merge(PrintBiom, by="date_hru", all.x=T) %>% 
  mutate(operation=ifelse(is.na(operation.x), operation.y, as.character(operation.x)))  %>%
  select(date_hru:crop, operation)


pander(crop_hru %>% 
         #group_by(yr, date, crop) %>%
         filter(crop=="corn")  %>% 
         filter(operation=="PrintBiom") %>%
         group_by(yr, date, crop) %>% 
         summarise(BiomHRUwm=weighted.mean(bioms,area)) %>%
         group_by(crop) %>% 
         summarise(BiomHRUwm=mean(BiomHRUwm)))

#saco los estadisticos
pander(crop_hru %>% mutate(op_crop=paste(operation,crop,sep="_")) %>% 
         group_by(op_crop) %>% filter(yield>0) %>%
         summarise(YldHRUwm=weighted.mean(yield,area), crop=first(crop)) %>% 
         filter(crop=="corn"))


pcp <- read.table("C:/Users/fhastings/Documents/INNOVAGRO/4.-QSWATplus/CCa3/Scenarios/Default/TxtInOut/PcpLB.pcp", skip = 3, col.names=c("year", "doy", "pcp")) %>% mutate(date= as.Date(paste(year, doy), format = "%Y %j")) %>% 
  mutate(Estacion= ifelse(month(date) >= 9 | month(date)<3,"P-V","O-I"),
         AnoEst= ifelse(month(date)<3, (year(date)), year(date)+1))    

#Defino PV de setiembre a febrero (crecimiento maiz)
#Defino a?o estival como el a?o de cosecha

pcpPV <- pcp %>%
  filter(Estacion=="P-V") %>%
  group_by(Estacion, AnoEst) %>%
  summarise(PP=sum(pcp)) %>%
  mutate(TendPcp = PP/mean(PP))

pcpPV %>%
  filter(AnoEst> 2003 & AnoEst<2019) %>%
  #filter(!(AnoEst == 1985 & Estacion=="P-V")) %>%
  ggplot() + geom_col(aes(x=AnoEst, y=PP/mean(PP)), fill="light blue")+
  geom_hline(yintercept= 1, color="red", linetype="dashed")+
  labs(y = "Precipitaci?n acumulada (mm/a?o)", x="")+
  geom_point(aes(Yld[16:30,]$yr, Yld[16:30,]$YldHRUwm/mean(Yld[16:30,]$YldHRUwm), colour="Yldswat"))+ 
  geom_point(aes(RendObs$yr, RendObs$Corn1_INIALE/mean(RendObs$Corn1_INIALE, na.rm=T), colour="Yldinia"), na.rm=T)+ 
  geom_point(aes(RendObs$yr, RendObs$Corn_DIEA/mean(RendObs$Corn_DIEA, na.rm=T), colour="YldDiea"), na.rm=T)
