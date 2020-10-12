rm(list = ls())

library(tidyverse)

#search folder in pc
folder <-"C:/Users/Usuario/Desktop/Git/"

#Github folder
setwd(paste0(folder,"Tesis/HRU_DATA_FILES/"))

objetos<-list("area", "P", "rotaciones", "rotacion_hru",
              "land_uses", "yield_df", "hru_info", 
              "rotacion_1", "rotacion_2","rotacion_3",
              "rotacion_4", "rotacion_5", "rotacion_6",
              "rotacion_7", "rotacion_8" )

for (i in 1:length(objetos)) {
  
  assign(
    objetos[[i]],
    paste0(objetos[i], ".RDS") %>% readRDS()
  )
  
}

setwd(paste0(folder,"Tesis/"))

folder_SL<-"C:/Users/Usuario/"

dir_escenarios<-paste0(folder_SL,
                       "Google Drive/SWAT-SubSantaLucia/12-RN_out/output_buffer_diarioFW/"
                        )

escenario <- list.files(dir_escenarios, pattern="RDS")
escenario



#LIBRARIES----

library(SWATplusR)
library(SWATdata)
library(tidyverse)

#
costos  <- readxl::read_xlsx( "C:/Users/Usuario/Desktop/Git/Tesis/array_costos_nuevo.xlsx") 
precios <- readxl::read_xlsx( "C:/Users/Usuario/Desktop/Git/Tesis/array_precios_nuevo.xlsx")


# "GRAS" "rotacion_1"
# "EUCA" "rotacion_2"
# "AGRC" "rotacion_3"
# "AGRP" "rotacion_4"
# "AGRP" "rotacion_5"
# "LECH" "rotacion_6"
# "LECH" "rotacion_7"
# "MONT" "rotacion_8"

#create a matrix with the costs
#each row is a productive unit, and each column a month
costos_ha<-matrix(nrow=865, ncol = 72 )

for (i in 1:nrow(costos_ha)) {
  if(rotacion_hru[i]=="rotacion_1"){costos_ha[i,]=costos$rotacion_1}
  else if(rotacion_hru[i]=="rotacion_2"){costos_ha[i,]=costos$rotacion_2}
  else if(rotacion_hru[i]=="rotacion_3"){costos_ha[i,]=costos$rotacion_3}
  else if(rotacion_hru[i]=="rotacion_4"){costos_ha[i,]=costos$rotacion_4}
  else if(rotacion_hru[i]=="rotacion_5"){costos_ha[i,]=costos$rotacion_5}
  else if(rotacion_hru[i]=="rotacion_6"){costos_ha[i,]=costos$rotacion_6}
  else if(rotacion_hru[i]=="rotacion_7"){costos_ha[i,]=costos$rotacion_7}
  else if(rotacion_hru[i]=="rotacion_8"){costos_ha[i,]=costos$rotacion_8}
}

#traspose area data.frame 
ar <-(area %>% t())
#converstion to hectares: 1km2=100 ha
ar <-ar*100

#zonas de cada hru
zonaA <- c(1,7,8,10,14,15,16,17,18,19,20,22,24,28)
zonaB <- c(4,5,25,31,3,27,2,23,29)
zonaC <- c(41,12,21,30,9,13,32,38,36,34,6,11,37,35,39,40,26,33)

zonas<- rbind(cbind(zonaA, "A"),
              cbind(zonaB, "B"),
              cbind(zonaC, "C")
) %>% as.data.frame() %>% rename("Subbasin"=zonaA,
                                 "Zona"="V2") %>%
  dplyr::mutate(Subbasin=as.numeric(Subbasin)) %>% 
  arrange(Subbasin)

anchos <- list()

#GENERAR UNA LISTA CON LOS ANCHOS DE BUFFER EN CADA ESCENARIO
for(i in 1:length(escenario)){
  anchos[i]<- 
    escenario[i] %>% stringr::str_remove("PTcon_ecdf_") %>%
    stringr::str_remove(".RDS") %>%
    stringr::str_remove("A") %>%
    stringr::str_remove("B") %>%
    stringr::str_remove("C") %>%
    strsplit("_")
}               

bandas<-matrix(nrow=25, ncol=3)

for(i in 1:25){
  for(j in 1:3){
   
    bandas[i,j]<-  anchos[[i]][j]

     }
}

bandas <- mutate_all( bandas %>% as.data.frame(),
                      function(x)as.numeric(x)
                      ) 
names(bandas)<-c("A", "B", "C")
duplicated(bandas)

setwd(paste0(folder,"Tesis/Santa_Lucia/"))

#FUNCION QUE CALCULA EL AREA DE BUFFER EN kM2
source("area_buffer.R")
#PARA EL ESCENARIO A EVALUAR
source("area_buffer_ha.R")


#FUNCION QUE CALCULA EL AREA DE BUFFER EN HECTAREAS


areas_buff_ha<-matrix(nrow=865,ncol=1)

#creo la matriz con el area de cada HRU

areas_hru_ha<-hru_info[1:865, "HRU_AREA_km2"]*100

#PRICES----
#generate a matrix that contains the crop prices
prices<-matrix(nrow=865, ncol=72)

#use the prices excel, which contains the price vector for each land use
for (i in 1:nrow(prices)) {
  if(rotacion_hru[i]=="rotacion_1"){prices[i,]=precios$rotacion_1}
  else if(rotacion_hru[i]=="rotacion_2"){prices[i,]=precios$rotacion_2}
  else if(rotacion_hru[i]=="rotacion_3"){prices[i,]=precios$rotacion_3}
  else if(rotacion_hru[i]=="rotacion_4"){prices[i,]=precios$rotacion_4}
  else if(rotacion_hru[i]=="rotacion_5"){prices[i,]=precios$rotacion_5}
  else if(rotacion_hru[i]=="rotacion_6"){prices[i,]=precios$rotacion_6}
  else if(rotacion_hru[i]=="rotacion_7"){prices[i,]=precios$rotacion_7}
  else if(rotacion_hru[i]=="rotacion_8"){prices[i,]=precios$rotacion_7}
}

tot_prof<-array(dim = c(865, 25))
tot_prof_ha<-array(dim = c(865, 25))
tot_prof_mon<-array(dim = c(72, 25))
restriction<-array(dim=c(25))
obj_value<-array(dim=c(25)) #the whole basin profit

#RUN SCENARIOS----
for(i in escenario){

n_esc<-match(i, escenario)

print(n_esc)

for(i in 1:865){
  areas_buff_ha[i,]<- area_buffer_ha(hru=i) 
}

#calculo el area libre de buffers
areas_sin_buffer<-(areas_hru_ha-areas_buff_ha[,1])

areas_sin_buffer <- areas_sin_buffer %>% as.data.frame()

#para los casos en donde el area del buffer es mas grande que la del
#hru, dejo el area productiva como 0 km2
areas_sin_buffer <- mutate_all(areas_sin_buffer,
                               function(x)ifelse(x>0, x, 0)
) 

#COMPUTATION----
#equation: cost_t=Area(unit).CostRotation(per unit)_t
#create a total costs matrix
#each row is a productive unit and each column a month

costos_tot <- sapply(1:nrow(costos_ha),
                     function(x) costos_ha[x,] * t(areas_sin_buffer)[x]) %>%
                      t()

#put names to columns, use another matrix that already have the same names
colnames(costos_tot) <- rownames(yield_df) 
#reshape2::melt(get(paste0("yield_df_",i)), id="date")

#extract from costos_tot matrix the rows that belong to a specific land use
#for each
#plot to check that everything is ok


#YIELD----

#create a matrix with yields per hectare
#yield variable in swat is measured in tons/ha
yield_ha <- yield_df %>% t() %>% as.data.frame()

#euc swat output is expressed in tons/ha#
#assumption: density of 500 kg per m3 or#
#                       0.5 ton per m3or#
#                       1 ton for 2 m3  #

#since wood is paid per m3, we convert swat output#
#from tons to m3#

ind_r2 <- grep("rotacion_2", rotacion_hru ); ind_r2

#convert tons/ha output to m3/ha output
yield_ha[ind_r2,] <-  
  yield_ha %>% cbind(rotacion_hru) %>% 
  as.data.frame() %>%
  filter(rotacion_hru=="rotacion_2") %>%
  select(-rotacion_hru) %>%
  mutate_all(function(x){2*as.numeric(as.character(x))})

#generate the total yield in the hru by multiplying yield/ha by the ha
yield_tot<- sapply(1:nrow(yield_ha),function(x) yield_ha[x,] * t(areas_sin_buffer)[x] ) %>%
  t()

#PRICES----


#REVENUE-----

#revenue per hectare in the hrus by month
#revenue_ha=yield_ha*production_price
revenue_ha <- yield_ha*prices

#PROFIT----

#using the generated revenue_ha and cost_ha, generate profit_ha
profit_ha<-revenue_ha-costos_ha

profit_tot <- sapply(1:nrow(profit_ha),function(x) profit_ha[x,] * t(areas_sin_buffer)[x] ) %>%
  t() %>% as.data.frame() %>%
  mutate_all(function(x) {as.numeric(x)})  

#DISC FACTOR----

#beta discount the cash flow
#consider a 7.5% annual real social discount rate
#source: Oficina de Planeamiento y Presupuesta

y<-0.075
#real rate is adjusted by us cpi, estimated at 2.5 p.c:
#nominal rate
y<-y+0.025

#monthly effective rate:
y<-((1+0.075)^(1/12))-1

#discount vector
r<-vector(length = 72) 

for (i in 2:length(r)) {
  r[1]<-1
  r[i]<-(1/(1+y))^(i-1) 
}

mat_r <- matrix(0, ncol=72, nrow=72)
diag(mat_r) <- r

profit_ha_disc  <- as.matrix(profit_ha) %*% mat_r 
profit_tot_disc <- as.matrix(profit_tot) %*% mat_r 

colnames(profit_ha_disc)<- colnames(profit_ha)
colnames(profit_tot_disc)<- colnames(profit_tot)

#total discounted profit in the whole period by hru
profit_tot_periodo <- profit_tot_disc %>%  apply(1,sum)

obj_value[n_esc]<- profit_tot_periodo %>% sum()

#total per ha discounted profit in the whole period by hru
profit_tot_ha_periodo <- profit_ha_disc %>%  apply(1,sum)

#total profit in the basin by month
profit_tot_month <- apply(profit_tot_disc, 2, sum)

#STORE VARS----
assign( paste0("Prof_Esc_",n_esc,"_",paste0(anchos[[n_esc]], collapse = "_")) ,
        cbind(profit_tot_periodo, profit_tot_ha_periodo)
)

assign(
  paste0("Prof_Month_Esc_",n_esc,"_",paste0(anchos[[n_esc]], collapse = "_")),
  profit_tot_month
)

tot_prof[,n_esc] <-profit_tot_periodo

tot_prof_ha[,n_esc] <- profit_tot_ha_periodo

tot_prof_mon[,n_esc] <- profit_tot_month

#PHOSHPHORUS----
#busca la posicion en el vector del valor mas cercano a 0.025 mg/L
pos <- which.min(abs(0.025-P[,1,1]))  

#prob de no excedencia
#o sea, prob de que la concentración de P
#esté debajo del valor 0,025 mg/L

#la prob es calculada en base a la concentr diaria de P
restriction[n_esc]<-P[pos,,n_esc][2]>0.5

print(escenario[n_esc])
print(paste("Objective Value", obj_value[n_esc]))
print(paste("restriction is met?", restriction[n_esc]))

readline(prompt="Press any key to run next scenario")

}

#SELECT THE BEST SCENARIO----
eval_scenarios <- cbind(obj_value, restriction) %>%
                  as.data.frame() 

feasible<-eval_scenarios %>% filter(restriction==1)
feasible<-feasible %>% arrange(desc(obj_value))
feasible_scenarios<- match(feasible$obj_value, 
                           eval_scenarios$obj_value)

for (i in feasible_scenarios) {
  anchos[[i]]  %>% print()
}


val_max <- eval_scenarios %>% filter(restriction==1) %>% max()

feas_values<-eval_scenarios %>% filter(restriction==1) %>% arrange()

scen_values<-matrix(nrow=dim(feas_values)[1], ncol=2)
scen_values[,2]<-feas_values$obj_value 

for(i in 1:dim(feas_values)[1]){
scen_values[i,1]<-match(values[i,1], eval_scenarios$obj_value)
}

scen_values<-scen_values %>% as.data.frame()
colnames(scen_values)<-c("Scenario", "Value")

scenario_index<-match(val_max, eval_scenarios$obj_value)

#Buffer Optimo
bandas[scenario_index,]
bandas[feasible_scenarios,]




