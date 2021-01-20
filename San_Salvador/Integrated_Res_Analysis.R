#Integrated Results Analysis
#The aim of this script is to measure the cost of 
#nutrient concentration reduction by using data from the proposed scenarios

rm(list=ls())

library(tidyverse)

setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador")

res_sub <-
readRDS("Resultados_Ambientales/Subbasin_Env_Results_Scenarios.RDS")

econ_tot<-
readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/Res_Econ_Tot.RDS")

econ_tot_1 <-
readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/Res_Econ_Tot_1.RDS")

econ_tot_6<-
readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/Res_Econ_Tot_6.RDS")

econ_ha <-
readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha.RDS")

econ_ha_1<-
readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha_1.RDS")

econ_ha_6 <-
readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha_6.RDS")

irr_yr <- 
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/irr_yr.RDS")

hru_info <-
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/hru_info.RDS")

#######THRESHOLD VALUES FOR N AND P CONCENTRATION ON WATER ###########

p_threshold <- 0.025 #normativa
n_threshold <- 1 #sugerido en doc mgap 

#####################

res_sub <-
res_sub %>% mutate(P_viol=case_when(P_Concentration>=p_threshold ~ 1,
                                    P_Concentration< p_threshold ~ 0),
                   N_viol=case_when(N_Concentration>=n_threshold ~ 1,
                                    N_Concentration< n_threshold ~ 0)) 

#Env Results per Scenario------
res_env_scen <-
res_sub %>%
  group_by(scenario) %>%
  summarise(N_v=sum(N_viol)/length(N_viol),
            P_v=sum(P_viol)/length(P_viol)); res_env_scen 


#See some yearly values (across all scenarios)
res_sub %>%
  group_by(yr) %>%
  summarise(N_v=sum(N_viol)/length(N_viol),
            P_v=sum(P_viol)/length(P_viol))

#See some subbasin values (across all scenarios)
res_sub  %>%
  group_by( subbasin) %>%
  summarise(N_v=sum(N_viol)/length(N_viol),
            P_v=sum(P_viol)/length(P_viol))


#Variations-------

#we merge the environmental results data base with the 
#economic mean results at the hectare level
#if, instead, we want to use the total results in the basin
#we should cbind using econ_tot data base


res_sub_outlet <-
  res_sub %>% group_by(scenario) %>%
  filter(channel==1) %>%
  summarise(n_mean=mean(N_Concentration),
            n_max=max(N_Concentration),
            p_mean=mean(P_Concentration),
            p_max=max(P_Concentration),
            n_viol=mean(N_viol),
            p_viol=mean(P_viol)) %>%
  cbind(econ_ha %>% dplyr::select(-rn) %>% t()); res_sub_outlet

colnames(res_sub_outlet)[8:15] <- 
  paste0( "ARA_" , seq(0,0.049, by=0.007))

res_sub_outlet_brutas<-
  apply(res_sub_outlet[,-1],
        1,
        function(x)(x-res_sub_outlet[10,-1])
  ) %>% 
  map_df(rbind); res_sub_outlet_brutas  

#When the CE is negative, compute the variation between scenarios as:
#variation/abs(CE) 

res_sub_outlet_porcent <-
  apply(res_sub_outlet[,-1],
        1 ,
        function(x) (x-res_sub_outlet[10,-1])/abs(res_sub_outlet[10,-1])
  ) %>% 
  map_df(rbind);res_sub_outlet_porcent  

#Elasticities----

#variation of CE in a response to a 1% variation in 
#the selected nutrient discharge varriables

elasticidades <-
  apply(res_sub_outlet_porcent[,1:6],
        2,
        function(x)(res_sub_outlet_porcent[,7:14]/x)
  )

rownames(elasticidades[[1]] )<- paste0("SC", seq(1,10,by=1)); elasticidades

reshape2::melt(elasticidades[[1]], )

mat_nitro<-
  reshape2::melt(data.table::setDT(elasticidades[[1]], keep.rownames = TRUE), "rn") 
colnames(mat_nitro)<-c("Escenario", "ARA", "Elasticidad")

#Spatial Analysis----

#Riego en Subcuencas----

areas_sub <-
aggregate(area~lu_mgt+Subbasin, data=hru_info, FUN=sum) %>% 
  group_by(Subbasin) %>% 
  mutate(porc_luse=area/sum(area),
         rot_riego=case_when(lu_mgt=="agrc3_lum" ~ "1",
                             lu_mgt=="agrc4_lum" ~ "6",
                             lu_mgt!="agrc3_lum" & lu_mgt!="agrc4_lum"  ~ "0")
         ); areas_sub

areas_sub_riego <-
aggregate(area~rot_riego+Subbasin, data=areas_sub, FUN=sum)  %>% 
  group_by(Subbasin) %>% 
  mutate(porc_luse=area/sum(area),
         no_regado=ifelse(rot_riego=="0",1,0))

porcentaje_riego_subcuencas <-
aggregate(porc_luse~Subbasin+no_regado, data=areas_sub_riego, FUN=sum) %>%
rename(porc_no_regado=porc_luse) %>%
mutate(porc_regado=1-porc_no_regado)


#subbasin 1 

ggplot(mat_nitro %>% filter(Escenario!="SC10") , aes(x=ARA, y=Escenario, fill=Elasticidad))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "red", labels=comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))


