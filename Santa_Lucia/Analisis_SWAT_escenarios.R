#######SCRIPT MODELO INTEGRADO####### 
rm(list = ls())

#hacer funcion

#LIBRARIES----
library(SWATplusR)
library(SWATdata)
library(tidyverse)

#DATA----
dir<- "C:/SWAT/Rafael/"
setwd(dir)
list.files(dir, pattern="RDS")
hru_info <- readRDS(paste0(dir, "hru_info.RDS"))
LULC <- readRDS(paste0(dir, "LULC_2006_2015.RDS"))

dir_escenarios<-"C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/12-RN_out/output_buffer_diarioFW/"
escenario <- list.files(dir_escenarios, pattern="RDS")

dir_yield <- "C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/12-RN_out/"
list.files(dir_yield, pattern="RDS")

yield<-paste0(dir_yield,
        "output_hru_yield_mensual_2006_2015.RDS") %>%
      readRDS()

#load("C:/SWAT/Data_vars.RData")

#demo_path <- "C:/SWAT/SWATPlus"


#proyecto_path <-"C:/SWAT/Drive/SWAT_mensual/Backup/"
#load("C:/SWAT/Corridas_3sim.RData")

#delete the date variable, now it is on the dataframe rownames
#simulacion<- lapply(simulacion$simulation, tibble::column_to_rownames, var="date")
stringr::str_remove(colnames(LULC), pattern = "LULC_")

hru_info %>% View
LULC %>% View

# AREAS  ----

area <- hru_info$HRU_AREA_km2

ggplot(area  %>% as.data.frame() %>% rename(Area=".")  , aes(x=Area))+
  geom_density(fill="#69b3a2", color="green", alpha=0.8)+
  xlab("Area_KM")+
  ylab("Densidad")

ggsave(filename = "areas.jpeg", plot = last_plot(),
       path = "C:/SWAT",
       scale = 1 )

#number of hrus#
area %>% t() %>% length

#stats#
area  %>% as.data.frame() %>% rename(Area=".")  %>% 
  summarise(
    min=min(Area),
    p25=quantile(Area, probs = c(0.25)),
    mean=mean(Area, probs = c(0.50)),
    median=median(Area),
    p75=quantile(Area, probs = c(0.75)),
    max=max(Area)
  )

#LAND USE----

#euca: eucalyptus
#past: pastura (pasteoreo)
#oats: avena
#corn: maiz
#sghy: sorgo forrajero
#mont: monte nativo

land_use <- LULC %>% tibble::column_to_rownames("date") %>% as_tibble()

seq <- TraMineR::seqdef(land_use) 
#below, graph the land use sequence for each hru
TraMineR::seqIplot(seq, border = NA, with.legend = "right")

#number of crop rotations
land_use %>% as.list() %>% unique() %>% length()

#sequence of states for each crop rotation
land_use %>% as.list() %>% unique()

#see the duplicated columns using data.table 
#library(data.table)
#DT<-data.table(data_hru[,land_use])
#DT %>% t() %>% duplicated() 

#detect number of rotations and crop periods
#data frame with all 7 rotations
df <- land_use %>% as.list() %>% unique() %>% as.data.frame()

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
                "rotacion_7", "rotacion_8")

#data_rot_hru<- data_hru[,land_use]

df[,1] %in% data_hru[,land_use]

for (i in 1:8) {
  assign(paste0("rotacion_",i) ,apply(land_use , 2 ,
                                      function(a) {identical(a,as.vector(get(paste0("df"))[,i]))} ) %>% as.vector()  
         
  )
}

#generated data.frame with the 7 rotations sequence in the basin
rotaciones<- cbind(rotacion_1, rotacion_2, rotacion_3,rotacion_4,
                   rotacion_5, rotacion_6, rotacion_7, rotacion_8) 

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
  apply(land_use ,2 ,
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
               change_rotacion_7,change_rotacion_8 ); changes

#days to change between land uses in each rotation
lapply(changes, diff)

#semesters to change between land uses in each rotation
lapply(changes, diff) %>% lapply(function(valor) {valor/6})
#years to change between land uses in each rotation
lapply(changes, diff) %>% lapply(function(valor) {valor/12})



# PHOSPHORUS CONCENTRATION ----
#this is only used in the san salvador basin

PTcon<- paste0(dir_escenarios, escenario[1]) %>% readRDS()

names(PTcon)

#concentracion de P 
PTcon$PTcon %>% head()
PTcon$PTcon %>% summary()

#Probabilidad de que ocurra la concentracion de PTcon en la subc 41(salida)
PTcon$rch_41

cbind(PTcon$PTcon, PTcon$rch_41) %>% head
cbind(PTcon$PTcon, PTcon$rch_41) %>% tail

#probabilidades de excedencia#
x <- PTcon$PTcon
y <- PTcon$rch_41

#en los valores altos la prob es 1 porque no 
#se sobrepasan esos valores en las simulaciones
#en los valores bajos la prob es baja porque en las 
#simulaciones se suele pasar esos niveles
plot(x,y$q50,type="l", xlim=c(0,0.2), xlab="mg/l", ylab="prob de no excedencia")
lines(x,y$q25,lty=2)
lines(x,y$q75,lty=2)
lines(x,y$qmin,lty=3)
lines(x,y$qmax,lty=3)
abline(v=0.025, col=2)
#

flow <- simulacion[flow_index]

#YIELD AGGREGATION ----

yield_data<-lapply(yield, tibble::column_to_rownames, var="date")

#get the mean and variance for each hru variable at month t, 

#the yield does not vary significantly
yield_data<- lapply(yield_data, function(x){x[,"Mean"]<-rowMeans(x)
x[,"Variance"]<-rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)   
return(x)
})

yield_data<-lapply(yield_data, function(x)x[,c("Mean", "Variance")])

yield <- lapply(yield_data, tibble::rownames_to_column, "date") 
#first 4 years erased, they are burn in period
yield <- lapply(yield, function(x)x[49:120,])

yield_df <- matrix(0,ncol = 865, nrow=72) %>% as.data.frame()
rownames(yield_df)<- yield[[1]][,1]  

mean_col<- match("Mean", yield[[1]] %>% colnames() )

for (i in 1:length(yield)) {
  yield_df[,i]<- yield[[i]][mean_col]
}

rownames(yield_df)<- phosphorus[[1]] %>% rownames()

for (i in 1:8) {
  
  a   <- get(paste0("rotacion_",i,"_gen"))$lengths %>% cumsum()
  num <- get(paste0("rotacion_",i,"_gen"))$lengths %>% cumsum() %>% length()
  c   <- yield[[1]]$date[a]
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
#rotation 8: lot of heterogeneity across hrus
plot(plot_yield_8) #purely native forest

#generate month and year variables in yield dataframes
yield_df_1 <- yield_df_1 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_2 <- yield_df_2 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_3 <- yield_df_3 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_4 <- yield_df_4 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_5 <- yield_df_5 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_6 <- yield_df_6 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_7 <- yield_df_7 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 
yield_df_8 <- yield_df_8 %>% mutate(month=lubridate::month(date), year=lubridate::year(date) ) 

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

y8<-yield_df_8  %>% group_by(month) %>% summarise(mean=mean(value)) %>% plot(,col="blue")

#see yield average by year
yy1<-yield_df_1  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy2<-yield_df_2  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy3<-yield_df_3  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy4<-yield_df_4 %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy5<-yield_df_5  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy6<-yield_df_6  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy7<-yield_df_7  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")
yy8<-yield_df_8  %>% group_by(year) %>% summarise(mean=mean(value)) %>% plot(col="blue")





#library(zoo)

cbind(rotacion_hru,
hru_info$LANDUSE
) %>% unique()
df
rotacion_hru
hru_info$LANDUSE

save.image("C:/SWAT/Analysis_buffer.RData")

