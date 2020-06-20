rm(list=ls())

##IMPORTANT FILES IN SWAT+ OUTPUT###



#Cambiar carpeta de trabajo POR QU? YA NO ME LO RECONOCE?? SI YA SE LA DI EN ROOT.DIR PREGUNTAR!!!
setwd("C:/SWAT/Florencia/CCa3/Scenarios/Default/TxtInOut/")

#EL EJECUTABLE
list.files(pattern=".exe")

#TIME.SIM----
#el archivo que controla el tiempo
list.files(pattern=".sim")

time<-list.files(pattern=".sim")
time<-read_lines(time)

#change simulation period
gregexpr(pattern ='0',time[3]) #posiciones de los dias
gregexpr(pattern ='1985',time[3]) #posicion del mes inicio
gregexpr(pattern ='2019',time[3]) #posicion mes final


#day_start
#substr(time[[3]],8,8)<-"2" #fijar dia de inicio
#day_end
#substr(time[[3]],28,28)<-"2" #fijar dia final
#yrc_start
#substr(time[[3]],15,19)<-"1990" #fijar año de inicio
#year_end
#substr(time[[3]],35,39)<- "2020" #fijar año final

write_lines(time, "time.sim") #sobrescribir archivo con valores nuevos

#PRINT.PRT----
#el archivo que controla que archivos se sobreescriben
list.files(pattern=".prt")
print<-list.files(pattern=".prt")
print<-read_lines(print)

#encontrar las n a cambiar en el txt, las filas con archivos van
#desde 11 a 48
fila<-48 #por ejemplo, fijo la ultima fila con el archivo pest
gregexpr(pattern ='n',print[fila])

substr(print[[fila]],30,30)<-"n" #fijar si quiero la variable diaria ("y" o "n")
substr(print[[fila]],30,30)<-"y" #fijar si quiero la variable mensual ("y" o "n")
substr(print[[fila]],30,30)<-"y" #fijar si quiero la variable anual ("y" o "n")
substr(print[[fila]],30,30)<-"n" #fijar si quiero average annual output("y" o "n")

write_lines(print, "print.prt") #sobrescribir archivo con valores nuevos

#una vez que termine de definir los archivos time.tim y print.prt
#voy a recoger los valores de los archivos de salida


#FLOW_OUT, TOT_Nkg Y TOT_Pkg  del archivo output.rch. También uso las variables YLDt/ha, LULC y IRR del archivo output.hru. 

#irr en el archivo hru_wb, 
#yield en el archivo hru_pw 
#flo_out en el archivo basin_sd_cha. 
#Sobre P y N no encontré las variables análogas a TOT_Nkg y TOT_Pkg, ni tampoco encontré el análogo a LULC.

list.files(pattern="hru")

#la variable de irrigación (irr) esta en el siguiente archivo
#verlo
read_table("hru_wb_day.txt", col_names = TRUE, skip=1) %>% View()
#la variable de yield esta en el siguiente archivo
#verlo
read_table("hru_wb_day.txt", col_names = TRUE, skip=1) %>% View()

#la variable flo_out esta en el siguiente archivo
#verlo
read_table("basin_sd_cha_day.txt", col_names = TRUE, skip=1) %>% View()



list.files(pattern="out")
read_table2("mgt_out.txt", col_names = TRUE, skip=1) %>% View()

read_table2("yield.out", col_names = TRUE, skip=1) %>% View()

list.files(pattern=".out")
list.files(pattern=".con")

list.files(pattern=".hru")
list.files(pattern="hru")

list.files(pattern=".txt")



library(SWATplusR)
run_swatplus
