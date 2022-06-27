#INTEGRATED ENVIRONMENT-ECONOMY ASSESSMENT MODEL #

#LOAD DATA----
rm(list = ls())

#LOAD OBJECTS GENERATED IN THE ANALYSIS SCRIPT
#DO THIS FOR EVERY SCENARIO
load("C:/SWAT/Analysis.RData")

library(SWATplusR)
library(SWATdata)
library(tidyverse)

costos  <- readxl::read_xlsx( "C:/SWAT/array_costos.xlsx") 
precios <- readxl::read_xlsx( "C:/SWAT/array_precios.xlsx")

#COSTS----

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
}


View(costos_ha)

#traspose area data.frame 
ar <-(area %>% t())
#converstion to hectares:1 square km2=100 ha
ar <-ar*100

#Calculo del area ocupada por el buffer--
##Area_Buffer----
#la funcion da el area en km2
area_buffer<-function(hru=1, ancho=10){

dir2<-"C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/12-RN_out/"

hru_info <- paste0(dir2,"hru_info.RDS") %>% readRDS()
head(hru_info)

n<-hru
#n = 700
hru_info[n,]

fname = paste0("C:/SWAT/Rafael/SL_20200401/",
               hru_info[n,"HRUGIS"],
               ".hru")

readLines(fname)[1:5]

#largo del hru
SLSUBBSN <- as.numeric(substr(readLines(fname)[3],1,16)) 

HRUarea = hru_info[n, "AREA..ha."];HRUarea
#area=largo*ancho-->ancho=area/largo
#10000 es para pasar de hect a m2
HRUW = 10000*HRUarea/SLSUBBSN; HRUW
HRUW

#diferentes anchos de buffer en metros
#FILTERW = c(5,10,20,40,80)
FILTERW =ancho

#el area del buffer es el largo del buff por el ancho del hru
#eventualmente, si el ancho de banda es muy grande, 
#puede pasar que tu area de buffer sea mayor a la del HRU
FILTER_area = HRUW*FILTERW; FILTER_area
FILTER_area_km2=FILTER_area/100; FILTER_area_km2
return(FILTER_area_km2)

}
#esta es la misama funcion, pero da el area para los 5 escenarios de
#5,10,20,40 y 80 metros de buffer

area_buffer_km2<-function(hru=1){
  
  dir2<-"C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/12-RN_out/"
  
  hru_info <- paste0(dir2,"hru_info.RDS") %>% readRDS()
  head(hru_info)
  
  n<-hru
  #n = 700
  hru_info[n,]
  
  fname = paste0("C:/SWAT/Rafael/SL_20200401/",
                 hru_info[n,"HRUGIS"],
                 ".hru")
  
  readLines(fname)[1:5]
  
  #largo del hru
  SLSUBBSN <- as.numeric(substr(readLines(fname)[3],1,16)) 
  
  HRUarea = hru_info[n, "AREA..ha."];HRUarea
  #area=largo*ancho-->ancho=area/largo
  #10000 es para pasar de hect a m2
  HRUW = 10000*HRUarea/SLSUBBSN; HRUW
  HRUW
  
  #diferentes anchos de buffer en metros
  FILTERW = c(5,10,20,40,80)
  
  #el area del buffer es el largo del buff por el ancho del hru
  #eventualmente, si el ancho de banda es muy grande, 
  #puede pasar que tu area de buffer sea mayor a la del HRU
  FILTER_area = HRUW*FILTERW; FILTER_area
  FILTER_area_km2=FILTER_area/100; FILTER_area_km2
  return(FILTER_area_km2)
  
}

area_buffer_ha<-function(hru=1){
  
  dir2<-"C:/SWAT/Rafael/"
  
  hru_info <- paste0(dir2,"hru_info_para_augusto.RDS") %>% readRDS()
  head(hru_info)
  
  n<-hru
  #n = 700
  hru_info[n,]
  
  fname = paste0("C:/SWAT/Rafael/SL_20200401/",
                 hru_info[n,"HRUGIS"],
                 ".hru")
  
  readLines(fname)[1:5]
  
  #largo del hru
  SLSUBBSN <- as.numeric(substr(readLines(fname)[3],1,16)) 
  
  HRUarea = hru_info[n, "HRU_AREA_km2"];HRUarea
  #area=largo*ancho-->ancho=area/largo
  #10000 es para pasar de hect a m2
  HRUW = 10000*HRUarea/SLSUBBSN; HRUW
  HRUW
  
  #diferentes anchos de buffer en metros
  FILTERW = c(5,10,20,40,80)
  
  #el area del buffer es el largo del buff por el ancho del hru
  #eventualmente, si el ancho de banda es muy grande, 
  #puede pasar que tu area de buffer sea mayor a la del HRU
  FILTER_area = HRUW*FILTERW; FILTER_area
  FILTER_area_ha=FILTER_area/10000; FILTER_area_km2
  return(FILTER_area_ha)
  
}

#creo una matriz para guardar el area de los buffer en cada escenario
areas_buff_ha<-matrix(nrow=865,ncol=5)

for(i in 1:865){
  areas_buff_ha[i,]<- area_buffer_ha(hru=i) 
}


#creo la matriz con el area de cada HRU
dir2<-"C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/12-RN_out/"
hru_info <- paste0(dir2,"hru_info.RDS") %>% readRDS()
areas_hru_ha<-hru_info[1:865, "AREA..ha."]

#calculo el area libre de buffers
areas_sin_buffer<-cbind(
areas_hru_ha-areas_buff_ha[,1],
areas_hru_ha-areas_buff_ha[,2],
areas_hru_ha-areas_buff_ha[,3],
areas_hru_ha-areas_buff_ha[,4],
areas_hru_ha-areas_buff_ha[,5]
)

areas_sin_buffer <- areas_sin_buffer %>% as.data.frame()

#para los casos en donde el area del buffer es mas grande que la del
#hru, dejo el area productiva como 0 km2
areas_sin_buffer <- mutate_all(areas_sin_buffer,
                               function(x)ifelse(x>0, x, 0)
                               ) 

#equation: cost_t=Area(unit).CostRotation(per unit)_t
#create a total costs matrix
#each row is a productive unit and each column a month
costos_tot<- sapply(1:nrow(costos_ha),function(x) costos_ha[x,] * ar[x] ) %>% t()

View(costos_tot)

#put names to columns, use another matrix that already have the same names
colnames(costos_tot)<- phosphorus[[1]] %>% rownames()
#reshape2::melt(get(paste0("yield_df_",i)), id="date")

#extract from costos_tot matrix the rows that belong to a specific land use
#for each
#plot to check that everything is ok
for (i in 1:7) {
  assign(paste0("costos_tot_",i),
         costos_tot[get(paste0("rotacion_",i)), ]
  )
  
  assign( paste0("costos_tot_",i),
          get(paste0("costos_tot_",i)) %>% t() %>% as.data.frame() %>% rownames_to_column("date")
  )
  
  assign(paste0("costos_tot_",i),
         reshape2::melt(get(paste0("costos_tot_",i)), id="date")
  )
  assign( 
    paste0("plot_costot_",i) , 
    
    ggplot(get(paste0("costos_tot_",i)), aes(x=date, y=value, group=variable))+
      geom_line()+
      theme_bw()+
      theme(panel.grid=element_blank(), axis.text.x = element_text(angle=90, hjust=1)) +
      geom_line(size=0.2, alpha=0.1)
  )
  
  ggsave(paste0("plot_costot_",i,".jpeg") , width = 4, height = 4)
  
  
}


plot_costot_1 
plot_costot_2
plot_costot_3
plot_costot_4
plot_costot_5
plot_costot_6
plot_costot_7


#YIELD----

#revenue in pasture, native forest and euca rotations?
yield$yield_1$Mean %>% View

#create a matrix with yields per hectare
#yield variable in swat is measured in tons/ha
yield_ha <- yield_df %>% t() %>% as.data.frame(); View(yield_ha)

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
yield_tot<- sapply(1:nrow(yield_ha),function(x) yield_ha[x,] * ar[x] ) %>%
            t()

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
}

dim(yield_ha)
dim(prices);View(prices)

#REVENUE-----

#revenue per hectare in the hrus by month
#revenue_ha=yield_ha*production_price
revenue_ha<-yield_ha*prices;View(revenue_ha)

#PROFIT----

#using the generated revenue_ha and cost_ha, generate profit_ha
profit_ha<-revenue_ha-costos_ha; View(profit_ha)

#see mean profit per hectare in each productive unit
#determine if it does make sense
apply(profit_ha, 1, mean) %>% View()
apply(profit_ha, 1, mean) %>% summary()
#seems to be 3 profits: 0 profit, 500usd7ha profit, and 2000usd/ha profit
apply(profit_ha, 1, mean) %>% density() %>% plot
apply(profit_ha, 1, mean) %>% plot()


#comments on the last plot
#there is huge heterogeneity in mean profit per hectare
#there are some hrus with mean profit_ha of around 1500 usd/ha while
#other hrus have around 500 usd/ha or less


#multiplying by the area obtain the total profit in each productive unit
profit_tot <- sapply(1:nrow(profit_ha),function(x) profit_ha[x,] * ar[x] ) %>%
  t() %>% as.data.frame() %>%
  mutate_all(function(x) {as.numeric(x)})  


apply(profit_tot, 1, mean) %>% View()
apply(profit_tot, 1, mean) %>% summary()
apply(profit_tot, 1, mean) %>% density() %>% plot
apply(profit_tot, 1, mean) %>% plot()


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


##HYPERBOLIC DISCOUNT###
#r<-vector(length = 72)
#for (i in 2:length(r)) {
#  r[1]<-1
#  r[i]<-1/(1+0.1*(i-1)) #same disc factor, faster decay at hyperbolic
#}

#cash<-matrix(1, nrow=72, ncol=1)

#cash<- profit_tot_year %>% as.matrix()

#discounted flow in million of dollars
#discounted_flow <- sum(cash*r)/1000000 



mat_r <- matrix(0, ncol=72, nrow=72)
diag(mat_r) <- r

#DISC PROFITS----

profit_ha_disc  <- as.matrix(profit_ha) %*% mat_r 
profit_tot_disc <- as.matrix(profit_tot) %*% mat_r 

colnames(profit_ha_disc)<-colnames(profit_ha)
colnames(profit_tot_disc)<-colnames(profit_tot)

#total discounted profit in the whole period by hru
profit_tot_periodo <- profit_tot_disc %>%  apply(1,sum)

#total profit in the basin by month
profit_tot_month <- apply(profit_tot_disc, 2, sum); plot(profit_tot_month)
plot(profit_tot_month, type = "line")
#in march of 2012 max profit is reached
#in june of 2010 min profit is reached

#total profit by hectare by month
#plot the basin disc profits per hectare
profit_ha_disc %>% apply( 2,sum)
profit_ha_disc %>% apply( 2,sum) %>% plot(type="line", col="red")

#plot the median disc profit per hectare
profit_ha_disc %>% apply( 2,median) %>% plot(type="line", col="red")


#RESULTS ANALYSIS------

#yield variance per rotation
#the graph shows the variance across hrus yields in each month
#the variance also depends on the land use
for (i in 1:7) {
  rbind(yield_df, rotacion_hru)  %>% 
    t() %>%
    as.data.frame() %>%
    filter(`73`==paste0("rotacion_",i)) %>%
    apply(2,sd) %>%
    plot(main=paste("yield rotacion ",i,"variance"), type="lines")

#    paste0("yield_rotacion",i,".jpg")
  }

#see the profit per hectare and its variance by rotation

for (i in 1:7) {
  cbind(profit_ha, rotacion_hru) %>%
    as.data.frame() %>% 
    filter(rotacion_hru==paste0("rotacion_",i)) %>%
    select(-rotacion_hru) %>% 
    apply(2,sd) %>%
    plot(main=paste("Variace_profit_rotation",i), type="lines")

#  jpeg(paste0("Varianza_profit_ha_rotacion",i,".jpg"))
  
  }

for (i in 1:7) {
  cbind(profit_ha, rotacion_hru) %>%
    as.data.frame() %>% 
    filter(rotacion_hru==paste0("rotacion_",i)) %>%
    select(-rotacion_hru) %>% 
    apply(2,mean) %>%
    plot(main=paste("Mean_profit_rotation",i), type="lines")
  
  #  jpeg(paste0("Varianza_profit_ha_rotacion",i,".jpg"))
  
}


#annual costs (without disc) show that rotation 3 is relatively more 
#expensive than the rest of the rotations

costos %>% apply(2, function(x)sum(x)/6)


#annual profit per hectare in each land use
#profit in cattle seems to be unrealistic (4000 usd/ha per annum)
#profit in eucaliptus also seems to be bigger than expected, but is not that unrealistic
#profit in pure agriculture is nearly zero
#profit in mixed ag and cattle land use is around 1500-2000 usd ha
#profit in rot 6, which is mainly pasture, assumed to produce cattle
#is way bigger than profit in pure cattle because yields in rot 6 are 3 or 4 times the yields of purely cattle hrus   



for (i in 1:7) {
  print(paste0("rotacion_",i, "profit_ha"))
  
    cbind(profit_ha_disc, rotacion_hru) %>%
    as.data.frame() %>% 
    filter(rotacion_hru==paste0("rotacion_",i)) %>%
    select(-rotacion_hru) %>% mutate_all(function(x)as.numeric(x)) %>%
    apply(1,function(x){sum(x)/6}) %>% mean() %>% print()

  #  jpeg(paste0("profit_ha_rotacion",i,".jpg"))
  
}


for (i in 1:7) {
  cbind(profit_ha, rotacion_hru) %>%
    as.data.frame() %>% 
    filter(rotacion_hru==paste0("rotacion_",i)) %>%
    select(-rotacion_hru) %>%
    mutate_all( function(x) as.numeric(as.character(x))) %>%
    apply(1, mean) %>%
    plot(main=paste("profit_ha rotacion ",i))
  
   jpeg(paste0("profit_ha_rotacion ",i,".jpg"))
}


cbind(profit_ha, rotacion_hru) %>% as.data.frame() %>% filter(rotacion_hru=="rotacion_1") %>% select(-rotacion_hru) %>% apply(1,sd) %>% plot()



