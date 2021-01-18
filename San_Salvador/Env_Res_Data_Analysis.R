rm(list=ls())

library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))

env_files <-
  list.files(pattern = "Env_Output_sc") %>% str_subset(pattern="RDS"); env_files
#load("Econ_Output_SWAT_Sim_Without_Irrigation.RData")

for (env in env_files) {
  
  envi <- str_remove(env, pattern = "Env_Output_")
  envi <- str_remove(envi, pattern=".RDS")
  envi <- str_remove(envi, pattern="SWAT_Sim_")
  assign(paste0("env_",envi) ,
         readRDS(env) %>% mutate(scenario=envi)
  )
  
}


env_f  <-
  list(                     
    env_sc1, env_sc2, env_sc3,
    env_sc4, env_sc5, env_sc6,
    env_sc7, env_sc8, env_sc9,
    env_scbase
  )


data_env <- Reduce(function(x,y){rbind(x=x, y=y)},env_f) 

res_ambientales<-
  "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales"

setwd(res_ambientales)

saveRDS(data_env, "Env_Results_Scenarios.RDS")

subbasin_map <-
  rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/Results/subs.shp"
                 #,
                 #layer ="sub" 
  )


library(RSQLite)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/SSalvador_LU2018_v0.sqlite")

## list all tables
tables <- dbListTables(con)

subbasins <-
  dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[99]], "'", sep=""))

saveRDS(subbasins, "subbasins.RDS")


library(raster)

raster::crs(subbasin_map) <- 
    CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador")


hru_info <-
  readRDS("hru_info.RDS")

sub_hru <-
  readRDS("sub_hru.RDS")

sub_chan <- 
  readRDS("sub_chan.RDS")

hru_info <-
plyr::join(hru_info, sub_hru, by="hru")


#RESULTADOS----

environmental_results <- 
  readRDS("Resultados_Ambientales/Env_Results_Scenarios.RDS")


library(RSQLite)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/SSalvador_LU2018_v0.sqlite")

## list all tables
tables <- dbListTables(con)

#info canales tabla 99
tables[[99]]
subbasins <-
dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[99]], "'", sep=""))


#get the outlet channel for each subbasin
subbasins_chan <-
subbasins %>% group_by(subbasin) %>% top_n(1, areac) 

subbasins_chan <-
subbasins_chan %>%  dplyr::select(id)

dbDisconnect(con)

env_results <-
environmental_results %>% filter(channel_id%in%subbasins_chan$id) %>%
  rename(id=channel_id) %>% plyr::join(subbasins_chan, by="id") %>%
  mutate(evap=as.numeric(evap), # measured in mm per month
         precip=as.numeric(precip) # measured in mm per month
         ) 

summary(env_results)

saveRDS(env_results,"Resultados_Ambientales/Subbasin_Env_Results_Scenarios.RDS")

env_sum_stats <-
env_results %>% group_by(scenario, subbasin) %>% 
  summarise(P=median(P_Concentration),
            N=median(N_Concentration),
            flo_out=median(flo_out),
            evap=median(evap),
            precip=median(precip)); env_sum_stats

env_sum_stats  %>%
  arrange(subbasin) %>% View

#P conentration distribution by subbasin----- 
env_results %>% names
ggplot(env_results, aes(x=scenario, y=P_Concentration))+
  geom_boxplot()+
  facet_wrap(~subbasin)+
  scale_y_continuous(limits = quantile(env_results$P_Concentration, c(0.01, 0.995)))+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))

#N conentration distribution by subbasin----- 
ggplot(env_results, aes(x=scenario, y=N_Concentration))+
  geom_boxplot()+
  facet_wrap(~subbasin)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))+
  scale_y_continuous(limits = quantile(env_results$N_Concentration, c(0.01, 0.995)))

#River Flow  distribution by subbasin----- 
ggplot(env_results, aes(x=scenario, y=flo_out))+
  geom_boxplot()+
  facet_wrap(~subbasin)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))+
  scale_y_continuous(limits = quantile(env_results$flo_out, c(0.01, 0.99)))


#Riego-----

#sub mean hru irrigation, to correct by hru area 

irr_yr <- 
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/irr_yr.RDS")

irr_yr<-
  irr_yr %>% plyr::join(hru_info, by="hru")

ggplot(irr_yr %>% filter(!is.na(Subbasin)), aes(x=scen, y=irr_cost))+
  geom_boxplot()+
  facet_wrap(~Subbasin)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))

ggplot(irr_yr %>% filter(!is.na(Subbasin)), aes(x=scen, y=irr_cost/area))+
  geom_boxplot()+
  facet_wrap(~Subbasin)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))

ggplot(irr_yr %>% filter(!is.na(Subbasin)), aes(x=scen, y=irr_cost))+
  geom_boxplot()+
  facet_wrap(~yr)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))

ggplot(irr_yr %>% filter(!is.na(Subbasin)), aes(x=scen, y=irr_cost/area))+
  geom_boxplot()+
  facet_wrap(~yr)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12))


irr_yr %>% group_by(Subbasin) %>%
  summarise(irr=sum(irr_sum)) %>% arrange(desc(irr))

irr_yr %>% group_by(Rotacion_riego) %>%
  summarise(irr=sum(irr_sum))

irr_yr %>% group_by(yr) %>%
  summarise(irr=sum(irr_sum))

irr_yr %>% group_by(Subbasin, Rotacion_riego) %>%
  summarise(irr=sum(irr_sum))