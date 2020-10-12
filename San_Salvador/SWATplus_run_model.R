#CORRIDA DE LOS ESCENARIOS EN SWAT-----
rm(list = ls())

library(tidyverse)
#para implementar un escenario de riego:
#shell.exec("C:/SWAT/Florencia/Procedimiento_SWATedit_correr_riegot.txt")

model_files<- "C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/"
model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
  
#1 Settings-----
#1.1-Setear time-----
#check time
#shell.exec("C:/SWAT/Florencia/CCa3/Scenarios/Default/TxtInOut/time.sim")
setwd(model_files)

setwd(model_scripts)

yr_begin <- "2003"
yr_end <- "2025" 

source("Set_time.R")
time(year_begin = yr_begin, year_end = yr_end, path = model_files)

#1.2-Setear Print-----
#archivo print.prt
shell.exec(paste0(model_files, "print.prt"))

#las variables que necesitaria serian

##lulc file mgt_out
##area file hru.con
##yield file hru_pw
##riego

#doc: mgt_out
#asi que saco esos archivos a escala mensual, diaria y anual

setwd(model_files)

#2 Decision Table----
#2-debo ir al file lum.dtl
#ahi le pongo una tabla de decision que defina mi rutina de riego
#a implementar

#Esto lo defino  solo en ese doc, para eso, debo primero ver con 
#Francisco y Miguel que escenarios implementar teniendo en cuenta las rotaciones
shell.exec(paste0(model_files, "lum.dtl"))


#3 Edit Rot Irrigation----
#3-luego, voy a management.sch y edito la rotacion particular que quiero poner
#lo primero que debe aparecer en op_typ, en la parte superior 
#tambien puedo editar el resto de las operaciones de cada rotacion
shell.exec(paste0(model_files, "management.sch"))


setwd(model_scripts)
source("irr_management.R")
#Por ejemplo, si quiero asignarle a todas las rotaciones la misma irrigacion
#con goteo
#irr_management(rutina="irr_str40_unlim")
#con pivot
#irr_management(rutina="irr_sw40_unlim")

#Si quiero diferenciar la irrigacion por rotaciones el tipo de irrigacion
#Pongo esto:

#irr_management_all(rot1 = "irr_sw40_unlim",
#                   rot2 = "irr_sw30_unlim",
#                   rot3 = "irr_sw20_unlim",
#                   rot4 = "irr_sw10_unlim",
#                   rot5 = "irr_sw35_unlim",
#                   rot6 = "irr_sw25_unlim")

#Primera prueba, despues puedo loopear
irr_management(rutina="irr_str40_unlim")

readline(prompt = "Press any key only if management scenarios are done and saved")

##Forma de escribir el archivo: 
##3.1 Elegir el nombre de la rutina de riego
##3.2 Contar la cantidad de letras de ese nombre
##3.3 Identificar el ncol en el que termina op_typ
##3.4 Calcular ncol de op_typ menos num de letras del nombre, eso me da ncol
##3.5 El nrow lo saco haciendo el nrow del nombre de la rotacion +1
##3.6 Ahì, con nrow y ncol, pongo el nombre de la rutina el el txt de management

#4 Run the Model----
#4-Correr el modelo
shell.exec(paste0(model_files, "Rev59.3_64rel.exe"))

#5 Sacar Output----
#5-Sacar los resultados de los archivos que necesite

#las variables que necesitaria serian
##5.1 Get LUSE-----

##lulc file mgt_out
shell.exec(paste0(model_files, "mgt_out.txt"))

mgt_out <- 
  read_table2(paste0(model_files, "mgt_out.txt"),
              skip=1) %>% select("hru", "year", "mon", "day",
                                 "operation", "crop/fert/pest")

mgt_out <- mgt_out[-1,] %>% rename(crop_fert_pest=`crop/fert/pest`) 

mgt_out <-
mgt_out %>% 
mutate(date=lubridate::ymd(paste(year, mon, day))) %>%
relocate(date, .before=year)
#ver los dias entre operaciones (period variable)

mgt_out <-
mgt_out %>% 
  group_by(hru) %>% 
  arrange(date) %>%
  arrange(hru)%>% filter(crop_fert_pest=="barl" |
                           crop_fert_pest=="corn" |
                           crop_fert_pest=="oats" |
                           crop_fert_pest=="past" |
                           crop_fert_pest=="soyb" |  
                           crop_fert_pest=="soy2" |
                           crop_fert_pest=="wwht" ) %>%
mutate(date_begin=NA, date_end=NA) %>%
relocate(date_begin, date_end, .before=year) 


for(i in 1:dim(mgt_out)[1]){

print(i)
#caso general, begin  
if(mgt_out$operation[i]=="PLANT"){mgt_out$date_begin[i]=mgt_out$date[i]}
#caso general, end 
if(mgt_out$operation[i+1]=="HARV/KILL" & mgt_out$hru[i]==mgt_out$hru[i+1] & i!=dim(mgt_out)[1]){mgt_out$date_end[i]=mgt_out$date[i+1]}

if(mgt_out$operation[i+1]=="KILL" & mgt_out$hru[i]==mgt_out$hru[i+1] & i!=dim(mgt_out)[1]){mgt_out$date_end[i]=mgt_out$date[i+1]}

#caso end cuando es la ultima operacion de la hru  
if(mgt_out$hru[i]!=mgt_out$hru[i+1] & i!=dim(mgt_out)[1]){
  mgt_out$date_end[i]=lubridate::ymd(paste(yr_end, "12", "31"))
  mgt_out$date_begin[i]=mgt_out$date[i]

  }
}

for(j in 2:dim(mgt_out)[1]){
  print(j)
  #caso begin cuando es la primer operacion de la hru
  if(mgt_out$hru[j]!=mgt_out$hru[j-1] & mgt_out$operation[j]=="HARV/KILL"){
    mgt_out$date_begin[j]=lubridate::ymd(paste(yr_begin, "1", "1"))
    mgt_out$date_end[j]=mgt_out$date[j]
  }
  
}  

mgt_out$date_begin[1]=lubridate::ymd(paste(yr_begin, "1", "1"))
mgt_out$date_end[1]=mgt_out$date[1]

mgt_out <-
mgt_out %>% mutate(date_begin=lubridate::as_date(date_begin),
                   date_end=lubridate::as_date(date_end)) 
mgt_out <-
mgt_out %>% filter(is.na(date_begin)!=TRUE &
                   is.na(date_end)!=TRUE) %>%
            rename(crop=crop_fert_pest)

mgt_out <-
mgt_out %>% mutate(hru=as.numeric(hru),
                   month_end=zoo::as.yearmon(date_end)) %>% 
arrange(hru) %>% 
select(hru, date_begin, date_end, month_end, crop) 
  


#finalmente, ahora en mgt_out tenemos para cada
#hru el uso definido por intervalo de fechas
#el yield usara el crop que caiga en la fecha
#de cosechae

#las HRU que no aparecen en mgt_out son 
#las que tienen luse=gras, o sea, ganaderas
##

#algo asi como cultivo, 
#              date_begin
#              date_end
#              day_period
#ahora, se contabilizan los dias en un uso como
#la diferencia entre la operacion Plant/kill
#y la operacion plant
#tomando solo cuando hay cultivo
  

##5.2 Get Area----
##area file hru.con
shell.exec(paste0(model_files, "hru.con"))


n_hru <- read_table2(paste0(model_files, "hru.con"), skip=1) %>%
  select(id) %>% unique %>% nrow

#averiguar la unidad de medida
areas <- read_table2(paste0(model_files, "hru.con"), skip=1) %>%
         select(id, area) %>% rename(hru=id) %>%
         mutate(hru=as.numeric(hru)) %>%
         arrange(hru)

##5.3 Get Yield----
##yield file hru_pw

shell.exec(paste0(model_files, "hru_pw_day.txt"))

shell.exec(paste0(model_files, "hru_pw_mon.txt"))


hru_yield <-
  read_table2(paste0(model_files, "hru_pw_mon.txt"),
              #n_max = 100,
              skip=1) %>% select("unit", "yr", "mon", "day",
                                 "yield") %>%
  slice(-1) %>%
  mutate(date_yield=
       lubridate::ymd(paste(yr, mon, day))
       ) %>%
relocate(date_yield, .before=yr) %>%
rename(hru=unit) %>%
arrange(hru) %>%
relocate(hru, yield, .before=yr) %>%
select(hru, date_yield, yield) %>%
mutate(hru=as.numeric(hru),
       yield=as.numeric(yield),
       month_yield=zoo::as.yearmon(date_yield)) %>%
filter(yield>0)

#5.4 Yield and LUSE----


output <-
plyr::join(mgt_out, hru_yield, by="hru" ) %>%
  filter(month_yield==month_end) 
   
##5.5 Get Irrigation----
##riego

shell.exec(paste0(model_files, "mgt_out.txt"))

shell.exec(paste0(model_files, "mgt_out.txt"))
#doc: mgt_out

#"C:/SWAT/Florencia/CCa3/Scenarios/Default/TxtInOut/mgt_out.txt"
#de aca saco el lulc, hacer funcion, es complicado

shell.exec(paste0(model_files, "hru_wb_mon.txt"))
"hru_wb_day.txt"
  
irr <- read_table2(paste0(model_files, "hru_wb_mon.txt"),
                   skip=1) %>%
  select(unit,mon, day, yr, irr) %>%
  mutate(date_irr=lubridate::ymd(paste(yr, mon, day)),
         irr=as.numeric(irr)) %>%
  filter(irr>0) %>%
  relocate(date_irr, .before=mon) %>%
  rename(hru=unit) %>%
  slice(-1) %>%
  mutate(hru=as.numeric(hru))

irr_yr<-
  irr %>% group_by(hru, yr) %>% summarise(irr_sum=sum(irr)) 




setwd("C:/Users/Usuario/Desktop/Git/Tesis")

save.image("Datos_Modelo.RData")





