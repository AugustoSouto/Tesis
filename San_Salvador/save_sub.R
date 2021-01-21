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

subs <- 
  readRDS("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/subs.RDS")

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

for(i in 1:13){
print(i)

  res_sub_outlet <-
  res_sub %>% group_by(scenario) %>%
  filter(subbasin==1) %>%
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

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
setwd(paste0(model_scripts, "Resultados_x_Subcuenca")) 

list

library(xlsx)
write.xlsx(res_sub_outlet, file= paste0("Resultados_Subbasin_",i,".xlsx"), sheetName="resultados")
write.xlsx(res_sub_outlet_brutas, paste0("Resultados_Subbasin_",i,".xlsx"),sheetName="res_var_brut", append=TRUE )
write.xlsx(res_sub_outlet_porcent, paste0("Resultados_Subbasin_",i,".xlsx"), sheetName="res_var_por", append=TRUE)
write.xlsx(elasticidades, paste0("Resultados_Subbasin_",i,".xlsx"), sheetName="res_elasticidades", append=TRUE)
}
