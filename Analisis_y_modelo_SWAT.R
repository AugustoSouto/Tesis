#######SCRIPT MODELO INTEGRADO####### 
rm(list = ls())

#hacer funcion

library(SWATplusR)
library(SWATdata)
library(tidyverse)

#load("C:/SWAT/Data_vars.RData")

demo_path <- "C:/SWAT/SWATPlus"

proyecto_path <-"C:/SWAT/Drive/SWAT_mensual/Backup/"


load("C:/SWAT/Corridas_3sim.RData")

simulacion %>% View
data_hru %>% View

vars <- simulacion$simulation %>% names()

simulacion_backup<- simulacion

#delete the date variable, now it is on the dataframe rownames
simulacion<- lapply(simulacion$simulation, tibble::column_to_rownames, var="date")

#get the mean and variance for each hru variable at month t, 

simulacion<- lapply(simulacion, function(x){x[,"Mean"]<-rowMeans(x)
x[,"Variance"]<-rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)   
return(x)
})

# AREAS  ----

area_index <- grep("area", colnames(data_hru))

area <- data_hru[,area_index] %>% slice(1)

ggplot(area %>% t() %>% as.data.frame()  , aes(x=V1))+
  geom_density(fill="#69b3a2", color="green", alpha=0.8)+
  xlab("Area_KM")+
  ylab("Densidad")

ggsave(filename = "areas.jpeg", plot = last_plot(),
       path = "C:/SWAT",
       scale = 1 )

#number of hrus#
area %>% t() %>% length

#stats#
area %>% t() %>% as.data.frame() %>% 
  summarise(
    min=min(V1),
    p25=quantile(V1, probs = c(0.25)),
    mean=mean(V1, probs = c(0.50)),
    median=median(V1),
    p75=quantile(V1, probs = c(0.75)),
    max=max(V1)
  )

#LAND USE----

#euca: eucalyptus
#past: pastura (pasteoreo)
#oats: avena
#corn: maiz
#sghy: sorgo forrajero
#mont: monte nativo

land_use <- grep("land_use", colnames(data_hru))

seq <- TraMineR::seqdef(data_hru[,land_use]) 
TraMineR::seqIplot(seq, border = NA, with.legend = "right")

#number of crop rotations

data_hru[,land_use] %>% as.list() %>% unique() %>% length()

#sequence of states for each crop rotation
data_hru[,land_use] %>% as.list() %>% unique()

#see the duplicated columns using data.table 
#library(data.table)
#DT<-data.table(data_hru[,land_use])
#DT %>% t() %>% duplicated() 

#detect number of rotations and crop periods
#data frame with all 7 rotations
df <- data_hru[,land_use] %>% as.list() %>% unique() %>% as.data.frame()

#set colnames of unique rotations
colnames(df)<-c("rotacion_1","rotacion_2","rotacion_3",
                "rotacion_4","rotacion_5","rotacion_6",
                "rotacion_7")

data_rot_hru<- data_hru[,land_use]

df[,1] %in% data_hru[,land_use]

for (i in 1:7) {
  assign(paste0("rotacion_",i) ,apply(data_hru[,land_use] , 2 ,
                                      function(a) {identical(a,as.vector(get(paste0("df"))[,i]))} ) %>% as.vector()  
         
  )
}

#generated data.frame with the 7 rotations sequence in the basin
rotaciones<- cbind(rotacion_1, rotacion_2, rotacion_3,rotacion_4,
                   rotacion_5, rotacion_6, rotacion_7) 

#check that hrus only have 1 true value per rotation 
apply(rotaciones,1,sum) %>% all()
#which rotation is set in each HRU
apply(rotaciones, 1, function(x) {names(which(x == T))})

#generate a string that helps to identify the hru rotation
rotacion_hru <- apply(rotaciones, 1, function(x) {names(which(x == T))}); rotacion_hru

seq_df <- TraMineR::seqdef(df); seq_df 

#visualize the rotations in a graph
graph_plot <- TraMineR::seqIplot(seq_df, border = NA, with.legend = "right")


#soil use summary (by rotation)
for (i in colnames(df)) {
  print(i)  
  get("df")[,i] %>% table() %>% print()
  rle(as.vector(get(paste0("df"))[,i])) %>% print()
}


for (i in colnames(df)) {
  assign(paste0(i,"_gen"),   
         
         rle(as.vector(get(paste0("df"))[,i])) )
}

#number of hrus with each type of rotation

for (i in colnames(df)) {
  print(paste("Hrus con",i))
  apply(data_hru[,land_use] ,2 ,
        function(a) 
        {identical(a,as.vector(get(paste0("df"))[,i]))}
  ) %>%
    sum %>%
    print()
  
}

#rotations 1,2,3 and 5 are the most widespread within the basin



#see how many months a given land use last
#variable seque_rotacion_ gives the cumulative months of a given land use 
#change_rotation gives the index which indicates the month in which a new land use is started in the rotation

for (i in colnames(df)) {
  assign(  
    paste0("seque_",i),
    sequence(rle(as.vector(get(paste0("df"))[,i]))$lengths) 
  )
  assign(
    paste0("change_",i),
    which(get(paste0("seque_",i))==1)
  )
  
}

changes<- list(change_rotacion_1,change_rotacion_2,change_rotacion_3,
               change_rotacion_4,change_rotacion_5,change_rotacion_6,
               change_rotacion_7); changes

#days to change between land uses in each rotation
lapply(changes, diff)

#semesters to change between land uses in each rotation
lapply(changes, diff) %>% lapply(function(valor) {valor/6})
#years to change between land uses in each rotation
lapply(changes, diff) %>% lapply(function(valor) {valor/12})



# FLOWS AGGREGATION ----
#this is only used in the san salvador basin

flow_index <- grep("flow", vars)

flow <- simulacion[flow_index]

#run the following lines if the daily model is run
#x <- as.POSIXct(flow$date)
#mo <- strftime(x, "%m")
#yr <- strftime(x, "%Y")

#dd <- data.frame(mo, yr, cbind(flow[,flow_index]))

#flow.agg <- aggregate(. ~ mo + yr, dd, FUN = sum)
#flow.agg$date <- as.POSIXct(paste(flow.agg$yr, flow.agg$mo, "01", sep = "-"))

#otra forma de hacerlo
# dd<- library(dplyr)
#  dd %>%
#  group_by(mo, yr) %>% 
#  summarise_each(funs(sum)) %>% head %>% View

#NITROGEN AGGREGATION ----

nitro_index <- grep("nitro", vars)

nitro <- simulacion[nitro_index]

n_units<- nitro %>% length()

#generate a data.frame with observations
nitro_df <- matrix(0,ncol = n_units, nrow=72) %>% as.data.frame()

rownames(nitro_df)<- nitro[[1]] %>% rownames()

mean_col<- match("Mean", nitro[[1]] %>% colnames() )

for (i in 1:length(nitro)) {
  nitro_df[,i]<- nitro[[i]][mean_col]
}

nitro_df <- nitro_df %>% rownames_to_column("date") 
nitro_df_backup <- nitro_df

nitro_df <- reshape2::melt(nitro_df, id="date")

#plot the N concentration time series for all rchs
#observation: november, december, january and february seem to be
#the worst months in terms of N concentratrion 
ggplot(nitro_df, aes(x=date, y=value, group=variable))+
  geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank(), axis.text.x = element_text(angle=90, hjust=1)) +
  geom_line(size=0.2, alpha=0.1)


#x <- as.POSIXct(nitro$date)
#mo <- strftime(x, "%m")
#yr <- strftime(x, "%Y")

#dd <- data.frame(mo, yr, nitro)

#nitro.agg <- aggregate(. ~ mo + yr, dd, FUN = sum)
#nitro.agg$date <- as.POSIXct(paste(nitro.agg$yr, nitro.agg$mo, "01", sep = "-"))


#PHOSPHORUS AGGREGATION ----

phosphorus_index <- grep("phosphorus", vars)

phosphorus <- simulacion[phosphorus_index]

n_units<- phosphorus %>% length() 

phosphorus_df <- matrix(0,ncol = n_units, nrow=72) %>% as.data.frame()
rownames(phosphorus_df)<- phosphorus[[1]] %>% rownames()

mean_col<- match("Mean", phosphorus[[1]] %>% colnames() )

for (i in 1:length(phosphorus)) {
  phosphorus_df[,i]<- phosphorus[[i]][mean_col]
}

phosphorus_df <- phosphorus_df %>% rownames_to_column("date") 
phosphorus_df_backup <- phosphorus_df

phosphorus_df <- reshape2::melt(phosphorus_df, id="date")

ggplot(phosphorus_df, aes(x=date, y=value, group=variable))+
  geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank(), axis.text.x = element_text(angle=90, hjust=1)) +
  geom_line(size=0.2, alpha=0.1)

#x <- as.POSIXct(phosphorus$date)
#mo <- strftime(x, "%m")
#yr <- strftime(x, "%Y")

#dd <- data.frame(mo, yr, phosphorus)

#phosphorus.agg <- aggregate(. ~ mo + yr, dd, FUN = sum)
#phosphorus.agg$date <- as.POSIXct(paste(phosphorus.agg$yr, phosphorus.agg$mo, "01", sep = "-"))


#YIELD AGGREGATION ----
yield_index <- grep("yield", vars)

yield <- simulacion[yield_index]

yield <- lapply(yield, tibble::rownames_to_column, "date") 

yield_df <- matrix(0,ncol = 865, nrow=72) %>% as.data.frame()

rownames(yield_df)<- yield[[1]] %>% rownames()

mean_col<- match("Mean", yield[[1]] %>% colnames() )

for (i in 1:length(yield)) {
  yield_df[,i]<- yield[[i]][mean_col]
}

rownames(yield_df)<- phosphorus[[1]] %>% rownames()

for (i in 1:7) {
  
  a   <- get(paste0("rotacion_",i,"_gen"))$lengths %>% cumsum()
  num <- get(paste0("rotacion_",i,"_gen"))$lengths %>% cumsum() %>% length()
  c   <- nitro_df_backup$date[a]
  d   <- lead(c)
  
  dff <- data.frame(x1=c,x2=d, 
                    y1=rep(-Inf,num), y2=rep(Inf,num))
  
  periods<-dff[seq(1,as.numeric(num), by=2),]
  
  assign(paste0("yield_df_",i),
         yield_df[,get(paste0("rotacion_",i))]
  )
  
  assign( paste0("yield_df_",i),
          get(paste0("yield_df_",i)) %>% rownames_to_column("date")
  )
  
  assign(paste0("yield_df_",i),
         reshape2::melt(get(paste0("yield_df_",i)), id="date")
  )
  
  
  
  assign( 
    paste0("plot_yield_",i) , 
    
    ggplot()+
      geom_line( aes(x=date, y=value, group=variable, color="red"), data=get(paste0("yield_df_",i)))+
      guides(color=FALSE)+
      geom_rect(data = periods, 
                mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
                color='blue',
                alpha=0.4)+
      theme(panel.grid=element_blank(),
            axis.text.x = element_text(angle=90, hjust=1),
            text = element_text(size=7.5))
    
  )  
}

#visualize crop yield per rotation
#rotation 1: very different patterns among hrus, some are very productive
plot(plot_yield_1) #purely pasture
#rotation 2: there are some very productive hrus but the yield cycle is almost the same across hrus
plot(plot_yield_2) #purely eucalyptus
#rotation 3: very similar yields among hrus
plot(plot_yield_3)
#rotation 4: very similar yields among hrus
plot(plot_yield_4)
#rotation 4: very similar yields among hrus
plot(plot_yield_5)
#rotation 6: very similar yields among hrus
plot(plot_yield_6)
#rotation 7: lot of heterogeneity across hrus
plot(plot_yield_7) #purely native forest


#generate month and year variables in yield dataframes
yield_df_1 <- yield_df_1 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_2 <- yield_df_2 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_3 <- yield_df_3 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_4 <- yield_df_4 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_5 <- yield_df_5 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_6 <- yield_df_6 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_7 <- yield_df_7 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 

#see yield average by month
par(mfrow=c(3,3))

#rotation 1: important months: from december to may
y1<-yield_df_1  %>%  group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")
#rotation 2: important months: from december to may
y2<-yield_df_2  %>%  group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")
#rotation 3: important months: from january to march
y3<-yield_df_3  %>% group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")
#rotation 4: important months: from january to march
y4<-yield_df_4  %>% group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")
#rotation 5: important months: from january to march
y5<-yield_df_5  %>% group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")
#rotation 6: important months: january and also dec and november
y6<-yield_df_6  %>% group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")
#rotation 7: important months: december
y7<-yield_df_7  %>% group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")

#see yield average by year
yy1<-yield_df_1  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy2<-yield_df_2  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy3<-yield_df_3  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy4<-yield_df_4 %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy5<-yield_df_5  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy6<-yield_df_6  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy7<-yield_df_7  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")





#library(zoo)
##create a data.frame with the yields#
#yield_hru<-lapply(names(yield), function(x) zoo::zoo(yield[[x]][4]$Mean, yield[[x]][1]$date))

#yield_hru<- yield_hru %>% unlist() %>% matrix(ncol=865) %>% as.data.frame() 


#colnames(yield_hru)<- paste0("Yield_HRU_",seq(1:865))

#yield_hru$date<-yield$yield_1$date











#aggregate(yield_hru, 
#           paste(lubridate::quarter(yield_hru$date), lubridate::year(yield_hru$date)) %>% as.list() ,
#           sum)

#yield_hru$quarter <- paste(lubridate::quarter(yield_hru$date), lubridate::year(yield_hru$date)) 
#yield.agg <- aggregate(. ~ quarter, yield_hru[,-date], FUN = sum)


#see onlt harvest months
#a<-vector()
#for (i in 1:nrow(yield_hru)) {
#  a<-cbind(a,any(yield_hru[i,]>0))
#}

#a<-vector()
#for (i in 1:nrow(yield_hru)) {
#  a<-cbind(a,any(yield_hru[i,1:865]>0))
#}

#only harvest month data
#yield.agg[a,] %>% View() 



#COSTS-REVENUE ----
#costs and revenue in pasture, native forest and euca rotations?


#costs are defined by area (ha or sq km) in hru in a given time unit "t"
#each rotation has a vector (Tx1) with the production 
#cost in each period, which depends on the land use


#CostRotation(per unit)_t-->cost per area unit 
# at time t in the selected rotation
#time t--->monthly?

#import vector with rotation costs per hectare:
costos  <- readxl::read_xlsx( "C:/SWAT/array_costos.xlsx") 
precios <- readxl::read_xlsx( "C:/SWAT/array_precios.xlsx")

#create a matrix with the co
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



#pasar el area de km2 a ha  
ar<-(area %>% t())
#converstion to hectares:1 square km2=100 ha
ar<-ar*100

#equation: cost_t=Area(unit).CostRotation(per unit)_t
costos_tot<- sapply(1:nrow(costos_ha),function(x) costos_ha[x,] * ar[x] ) %>% t()

View(costos_tot)


colnames(costos_tot)<- phosphorus[[1]] %>% rownames()
#reshape2::melt(get(paste0("yield_df_",i)), id="date")

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

#cost dynamics in the hrus per rotation
plot_costot_1 
plot_costot_2
plot_costot_3
plot_costot_4
plot_costot_5
plot_costot_6
plot_costot_7

#revenue in pasture, native forest and euca rotations?
yield$yield_1$Mean %>% View

yield_ha <- yield_df %>% t() %>% as.data.frame()

#euc swat output is expressed in tons/ha#
#assumption: density of 500 kg per m3 or#
#                       0.5 ton per m3or#
#                       1 ton for 2 m3  #

#since wood is paid per m3, we convert swat output#
#from tons to m3#
ind_r2 <- grep("rotacion_2", rotacion_hru ); ind_r2

yield_ha[ind_r2,] <-  
  yield_ha %>% cbind(rotacion_hru) %>% 
  as.data.frame() %>%
  filter(rotacion_hru=="rotacion_2") %>%
  select(-rotacion_hru) %>%
  mutate_all(function(x){2*as.numeric(as.character(x))})


yield_tot<- sapply(1:nrow(yield_ha),function(x) yield_ha[x,] * ar[x] ) %>% t()

prices<-matrix(nrow=865, ncol=72)

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
dim(prices)

#revenue per hectare in the hrus by month
revenue_ha<-yield_ha*prices

profit_ha<-revenue_ha-costos_ha

profit_tot <- sapply(1:nrow(profit_ha),function(x) profit_ha[x,] * ar[x] ) %>%
  t() %>% as.data.frame() %>%
  mutate_all(function(x) {as.numeric(x)})  

profit_ha  %>% apply(1, sum)
#total profit in the whole period by hru
profit_tot_periodo <- profit_tot %>%  apply(1,sum)
#profit per hectare in the whole year by hru

plot(profit_ha %>%
       apply( 1,sum), ylab="Profit_ha"
)



#total profit in the basin by month
profit_tot_year <- apply(profit_tot, 2, sum); plot(profit_tot_year)
#total profit by hectare by month
a <- profit_ha %>% apply( 2,sum)

options(scipen = 999)
plot(y=a, x=as.Date( names(a)), type = "lines", col="red")

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
    apply(1, sum) %>%
    plot(main=paste("profit_ha rotacion ",i))
}


cbind(profit_ha, rotacion_hru) %>% as.data.frame() %>% filter(rotacion_hru=="rotacion_1") %>% select(-rotacion_hru) %>% apply(1,sd) %>% plot()


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

cash<-matrix(1, nrow=72, ncol=1)
cash<- profit_tot_year %>% as.matrix()

#discounted flow in million of dollars
discounted_flow <- sum(cash*r)/1000000 


save.image("C:/SWAT/Corridas_para_markdown.RData")

