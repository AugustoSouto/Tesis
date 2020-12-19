#RUN SWAT SCENARIOS-----
rm(list = ls())

library(tidyverse)
#para implementar un escenario de riego:
#shell.exec("C:/SWAT/Florencia/Procedimiento_SWATedit_correr_riegot.txt")

model_files<- "C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/"
model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"

#0 Put Scenarios-----
sc1 <- c("", "", "irr_str80_unlim","irr_str80_unlim", "", "" )
sc2 <- c("", "", "irr_str95_unlim","irr_str95_unlim", "", "" )
sc3 <- c("", "", "irr_str40_unlim","irr_str40_unlim", "", "" )
sc4 <- c("", "", "irr_str80_unlim","", "", "" )
sc5 <- c("", "", "irr_str95_unlim","", "", "" )
sc6 <- c("", "", "irr_str40_unlim","", "", "" )
sc7 <- c("", "", "","irr_str80_unlim", "", "" )
sc8 <- c("", "", "","irr_str95_unlim", "", "" )
sc9 <- c("", "", "","irr_str40_unlim", "", "" )
scbase <- c("", "", "","", "", "" )

scenarios <-
rbind(sc1, sc2, sc3, sc4, sc5, sc6, sc7, sc8, sc9, scbase)


for (sc in 1:dim(scenarios)[1]) {
 


#1 Settings-----
#1.1-Set time-----
#check time
#shell.exec("C:/SWAT/Florencia/CCa3/Scenarios/Default/TxtInOut/time.sim")
setwd(model_files)

setwd(model_scripts)

yr_begin <- "1998"
yr_end <- "2019" 

source("Set_time.R")
time(year_begin = yr_begin, year_end = yr_end, path = model_files)

#1.2-Set Print-----
#archivo print.prt
#shell.exec(paste0(model_files, "print.prt"))

#las variables que necesitaria serian

##lulc file mgt_out
##area file hru.con
##yield file hru_pw
##riego file hru_wb

#doc: mgt_out
#asi que saco esos archivos a escala mensual y diaria


#2 Decision Table----
#2-debo ir al file lum.dtl
#ahi le pongo una tabla de decision que defina mi rutina de riego
#a implementar

#Esto lo defino  solo en ese doc, para eso
#Por ahora los escenario que file cambian el umbral
#en 0.3, 0.4 y 0.5, en base al 05 de claudio
#shell.exec(paste0(model_files, "lum.dtl"))


#3 Edit Rot Irrigation----
#3-luego, voy a management.sch y edito la rotacion particular que quiero poner
#lo primero que debe aparecer en op_typ, en la parte superior 
#tambien puedo editar el resto de las operaciones de cada rotacion
#shell.exec(paste0(model_files, "management.sch"))

#Fijarme que el archivo management.sch que use al inicio
#este bien, es decir, que tenga un renglon libre dejajo del
#nombre de la rotacion para poner la operacion de riego o dejarlo
#libre en el caso de que no se regue, sino va a haber un error al correr

setwd(model_scripts)

source("irr_management.R")
#Por ejemplo, si quiero asignarle a todas las rotaciones la misma irrigacion
#con goteo
#irr_management(rutina="irr_str40_unlim")
#con pivot
#irr_management(rutina="irr_sw40_unlim")

#Si quiero diferenciar la irrigacion por rotaciones el tipo de irrigacion
#Pongo esto:

# "" quiere decir que no se riega

set_irrigation<-
function(scenario){
  irr_management_all(rot1=scenario[1],
                     rot2=scenario[2],
                     rot3=scenario[3],
                     rot4=scenario[4],
                     rot5=scenario[5],
                     rot6=scenario[6])
  }

set_irrigation(scenario = scenarios[sc,])



#Primera prueba, despues puedo loopear

##Forma de escribir el archivo: 
##3.1 Elegir el nombre de la rutina de riego
##3.2 Contar la cantidad de letras de ese nombre
##3.3 Identificar el ncol en el que termina op_typ
##3.4 Calcular ncol de op_typ menos num de letras del nombre, eso me da ncol
##3.5 El nrow lo saco haciendo el nrow del nombre de la rotacion +1
##3.6 Ah?, con nrow y ncol, pongo el nombre de la rutina el el txt de management

#4 Run the Model----
#4-Correr el modelo
t1 <-Sys.time()
shell.exec(paste0(model_files, "Rev59.3_64rel.exe"))
print("coso")
Sys.sleep(60*27) # poner un tiempo que asegure la ejecucion
t2 <- Sys.time()
tiempo_corrida<- t2-t1; tiempo_corrida

#5 Sacar Output----
#5-Sacar los resultados de los archivos que necesite

#las variables que necesitaria serian
#5.1 Get LUSE-----
 #lulc file mgt_out
 #shell.exec(paste0(model_files, "mgt_out.txt"))

t3<- Sys.time()

mgt_out <- 
  read_table2(paste0(model_files, "mgt_out.txt"),
              skip=1) %>% dplyr::select("hru", "year", "mon", "day",
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
dplyr::select(hru, date_begin, date_end, month_end, crop) 
  
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
#shell.exec(paste0(model_files, "hru.con"))


n_hru <- read_table2(paste0(model_files, "hru.con"), skip=1) %>%
         dplyr::select(id) %>% unique %>% nrow


areas <- read_table2(paste0(model_files, "hru.con"), skip=1) %>%
         dplyr::select(id, area) %>% rename(hru=id) %>%
         mutate(hru=as.numeric(hru)) %>%
         arrange(hru)

#5.3 Get Rotations----
#can modify hru rotations by editing the hru-data file
hrus_rotations <- 
  read_table2(paste0(model_files, "hru-data.hru"),
              skip=1) %>% dplyr::select(id, lu_mgt) %>%
  rename(hru=id)

##5.4 Get Yield----
##yield file hru_pw

#shell.exec(paste0(model_files, "hru_pw_day.txt"))

#shell.exec(paste0(model_files, "hru_pw_mon.txt"))


hru_yield <-
  read_table2(paste0(model_files, "hru_pw_mon.txt"),
              #n_max = 100,
              skip=1) %>% dplyr::select(unit, yr, mon, day,
                                 yield) %>%
  slice(-1) %>%
  mutate(date_yield=
       lubridate::ymd(paste(yr, mon, day))
       ) %>%
relocate(date_yield, .before=yr) %>%
rename(hru=unit) %>%
arrange(hru) %>%
relocate(hru, yield, .before=yr) %>%
dplyr::select(hru, date_yield, yield) %>%
mutate(hru=as.numeric(hru),
       yield=as.numeric(yield),
       month_yield=zoo::as.yearmon(date_yield)) %>%
filter(yield>0)

#5.5 Yield and LUSE----

output <-
plyr::join(mgt_out, hru_yield, by="hru" ) %>%
  filter(month_yield==month_end) 
   
##5.6 Get Irrigation----
##riego

#shell.exec(paste0(model_files, "mgt_out.txt"))

#shell.exec(paste0(model_files, "mgt_out.txt"))
#doc: mgt_out

#"C:/SWAT/Florencia/CCa3/Scenarios/Default/TxtInOut/mgt_out.txt"
#de aca saco el lulc, hacer funcion, es complicado

#shell.exec(paste0(model_files, "hru_wb_mon.txt"))
  
irr <- read_table2(paste0(model_files, "hru_wb_mon.txt"),
                   skip=1) %>%
  dplyr::select(unit,mon, day, yr, irr) %>%
  mutate(date_irr=lubridate::ymd(paste(yr, mon, day)),
         irr=as.numeric(irr)) %>%
  filter(irr>0) %>%
  relocate(date_irr, .before=mon) %>%
  rename(hru=unit) %>%
  slice(-1) %>%
  mutate(hru=as.numeric(hru))

irr_yr<-
  irr %>% group_by(hru, yr) %>% summarise(irr_sum=sum(irr))  


#5.7 Get Environmental P and N----

#

environmental_output <-
read_table2(paste0(model_files, "channel_sd_mon.txt"),
            #n_max = 100,
            skip=1) %>% dplyr::select(unit, gis_id,
                               mon, day, yr,
                               no3_out, solp_out,
                               flo_out, precip, evap) %>%
  mutate(date_env=lubridate::ymd(paste(yr, mon, day))) %>%
  slice(-1)  %>% 
  rename(channel=unit,
         channel_id=gis_id,
         Nitrogen=no3_out,
         Phosphorus=solp_out) %>%
  mutate(Phosphorus=as.numeric(Phosphorus),
         Nitrogen=as.numeric(Nitrogen),
         flo_out=as.numeric(flo_out)) %>%
  mutate(P_Concentration=(Phosphorus*1000000)/(flo_out*60*60*24*1000*30),
         N_Concentration=(Nitrogen*1000000)/(flo_out*60*60*24*1000*30)
         )


t4 <- Sys.time()

tiempo_procesamiento <- t4-t3; tiempo_procesamiento 
tiempo_total <- t4-t1; tiempo_total

beepr::beep(sound=5)

#6 Save Output ----

setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))
save.image(paste0(rownames(scenarios)[sc], ".RData"))

}  

#Old Saving
#save.image("SWAT_Sim_03_Rot1.RData")
#save.image("SWAT_Sim_Without_Irrigation.RData")






