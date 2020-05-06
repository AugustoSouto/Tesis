#INTEGRATED ENVIRONMENT-ECONOMY ASSESSMENT MODEL #

#LOAD DATA----

rm(list = ls())
#LOAD OBJECTS GENERATED IN THE ANALYSIS SCRIPT
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
ar<-(area %>% t())
#converstion to hectares:1 square km2=100 ha
ar<-ar*100

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
apply(profit_ha, 1, mean) %>% plot() 

#comments on the last plot
#there is huge heterogeneity in mean profit per hectare
#there are some hrus with mean profit_ha of around 1500 usd/ha while
#other hrus have around 500 usd/ha or less


#multiplying by the area obtain the total profit in each productive unit
profit_tot <- sapply(1:nrow(profit_ha),function(x) profit_ha[x,] * ar[x] ) %>%
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

#total profit by hectare by month
a <- profit_ha_disc %>% apply( 2,sum)
names(a)<-colnames(profit_ha)

#plot the basin disc profits per hectare
options(scipen = 999)
plot(y=a, x=as.Date( names(a)), type = "lines", col="red")

#plot the median disc profit per hectare
b <- profit_ha_disc %>% apply( 2,median)
names(b)<-colnames(profit_ha)
plot(y=b, x=as.Date( names(a)), type = "lines", col="red")

#RESULTS ANALYSIS-----

#yield variance per rotation
for (i in 1:7) {
  rbind(yield_df, rotacion_hru)  %>% 
    t() %>%
    as.data.frame() %>%
    filter(`73`==paste0("rotacion_",i)) %>%
    apply(2,sd) %>%
    plot(main=paste("yield rotacion ",i,"variacion"))
}

for (i in 1:7) {
  cbind(profit_ha, rotacion_hru) %>%
    as.data.frame() %>% 
    filter(rotacion_hru==paste0("rotacion_",i)) %>%
    select(-rotacion_hru) %>% 
    apply(1,sd) %>%
    plot(main=paste("Variacion profit_ha rotacion ",i))
}

for (i in 1:7) {
  cbind(profit_ha, rotacion_hru) %>%
    as.data.frame() %>% 
    filter(rotacion_hru==paste0("rotacion_",i)) %>%
    select(-rotacion_hru) %>%
    mutate_all( function(x) as.numeric(as.character(x))) %>%
    apply(1, mean) %>%
    plot(main=paste("profit_ha rotacion ",i))
}


cbind(profit_ha, rotacion_hru) %>% as.data.frame() %>% filter(rotacion_hru=="rotacion_1") %>% select(-rotacion_hru) %>% apply(1,sd) %>% plot()



