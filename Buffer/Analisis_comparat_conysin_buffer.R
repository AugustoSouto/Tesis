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
#below, graph the land use sequence for each hru
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

#set colnames of unique land uses
#rotation 1: cattle *
#rotation 2: eucaliptus forestry *
#rotation 3: pure agricultural rotation
#rotation 4: mixed agricultural-cattle rotation
#rotation 5: mixed agricultural-cattle rotation
#rotation 6: dairy rotation
#rotation 7: native forestry

# *cattle, eucaliptus and native forestry land uses are not rotations
# only use the name rotation so as to put the same name to all sequences



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

save.image("C:/SWAT/Analysis.RData")

