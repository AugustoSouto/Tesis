#Time.sim----


time <-function(year_begin="2010", year_end="2030", path="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/"){
#Leo el documento con los settings de time
time<-readLines(paste0(path, "time.sim"))

#el string identifica los valores por defecto del ultimo setting de tiempo
#cambiar solo los años, los dias dejarlos filos en 1 enero (dia 0)
#CUIDADO: si cambi el año, fijarme que no altere los valores de los otros elementos

#Leer el file, y sacar todos los parametros de la linea 3, lo hago asi:
params <-
  read_table(paste0(path, "time.sim"),
             skip = 1)

#defino los parametros de tiempo
ds <-  params[1] %>% as.character()  #dia en el que comienza la simulacion (0=1 enero), rango 0-366
ys <-  params[2] %>% as.character()   # year en el que comienza la simulacion
de <-  params[3] %>% as.character()      #dia en el que termina la simulacion
ye <-  params[4] %>% as.character()   #year en el que termina la simulacion
stop<- params[5] %>% as.character()     #stop, es un parametro que no uso, lo dejo como está

#Obtengo las posiciones de los parametros en la tercera linea del documento
#asi, puedo sustituirlas con mis nuevos parametros

posiciones<-
  str_locate_all(time[3],c(ds, ys, de, ye, stop))

#En los casos de los valores que son 0, para los elementos 3 y 5,
#puede que si cambio los años cambie la cantidad de ceros antes en la lista
#por lo que tengo que detectar cuantos ceros antes hay para poder
#cambiar con exito el documento
#eso lo hago contando la cantidad de 0 antes en cada caso

antes_de<- str_count(
                     paste0(c(ds, ys), collapse=" "),
                     de
                     )

antes_stop<- str_count(
                       paste0(c(ds, ys, de, ye), collapse=" "),
                       stop
                       )


#el primer elemento es el primer cero
#el segundo es el unico 2000 o lo que sea que defina
#el tercer elemento, es el quinto cero o lo que se cuente 
#el cuarto elemento, es el unico 2030
#el quinto elemento, es el octavo cero o lo que se cuente

#saco las posiciones de los parametros, ahora si
pds <- posiciones[[1]][1]
pys <- posiciones[[2]][1:2]
pde <- posiciones[[3]][antes_de+1]
pye <- posiciones[[4]][1,1:2]
pstp<- posiciones[[5]][antes_stop+1]

#Ahora definir los parametros que yo quiero, pongo solo los años 
#Asi no complico mas.

#si quiero incluir los dias mas adelante, tener en cuenta que 
#pueden haber dias de 1,2 o 3 digitos, lo que puede hacer que cambie
#el orden de los caracteres

ds <-"0"   #dia en el que comienza la simulacion (0=1 enero), rango 0-366
ys <- year_begin    # year en el que comienza la simulacion
de <-"0"       #dia en el que termina la simulacion
ye <- year_end    #year en el que termina la simulacion
stop<- "0"     #stop, es un parametro que no uso, lo dejo como está

#definir la nueva linea con los parametros que elegi
time[3]<-  
paste0(c(rep("", pds-1),
         ds,
         rep("", pys[1]-pds-2),
         ys,
         rep("", pde-pys[2]-2),
         de,
         rep("", pye[1]-pde-2),
         ye,
         rep("", pstp-pye[2]-2),
         stop,
         rep("", 2)),          collapse = " ")

setwd(path)
#ahora sobreescribo el documento y listo
write(time, "time.sim")

return(cat("Setting: Model runs from year", year_begin, "to year", year_end))
}



