#Integrated Results Analysis
#The aim of this script is to measure the cost of 
#nutrient concentration reduction by using data from the proposed scenarios

rm(list=ls())

library(tidyverse)

setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador")

graphs_dir<-"C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_x_Subcuenca"

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

subbasin_map <-
  rgdal::readOGR(dsn="C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/Results/subs.shp"
                 #,
                 #layer ="sub" 
  )

#genero otra columna igual para mergear en caso de que los nombres sean dif
subbasin_map@data <-
  subbasin_map@data %>% mutate(subbasin=Subbasin)

subbasin_map %>% sp::plot()

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

#ECDF per scenario-----

#result per subbasin in base scenario
ggplot(res_sub %>% filter(scenario=="scbase"),
       aes(P_Concentration)) +
  stat_ecdf(geom="step")+
  xlim(0, 0.025)+
  facet_wrap(vars(subbasin))+
  geom_hline(yintercept=0.5, col="red")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10))

ggsave("Pcon_ECDF_subbasin_base.jpeg",
       path = graphs_dir
       #width =
       #height =
)  

ggplot(res_sub %>% filter(scenario=="scbase"),
       aes(N_Concentration)) +
  stat_ecdf(geom="step")+
  xlim(0, 10)+
  facet_wrap(vars(subbasin))+
  geom_hline(yintercept=0.5, col="red")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10))

ggsave("Ncon_ECDF_subbasin_base.jpeg",
       path = graphs_dir
       #width =
       #height =
)  


#result per subbasin in most intensive scenario
ggplot(res_sub %>% filter(scenario=="sc9"),
       aes(P_Concentration)) +
  stat_ecdf(geom="step")+
  xlim(0, 0.03)+
  facet_wrap(vars(subbasin))+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=0.025, col="red")

ggsave("Pcon_ECDF_subbasin_intensive.jpeg",
              path = graphs_dir
              #width =
              #height =
       )  


ggplot(res_sub %>% filter(scenario=="sc9"),
       aes(N_Concentration)) +
  stat_ecdf(geom="step")+
  xlim(0, 10)+
  facet_wrap(vars(subbasin))+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=1, col="red")

ggsave("Ncon_ECDF_subbasin_intensive.jpeg",
       path = graphs_dir
       #width =
       #height =
)  


ggplot(res_sub %>% filter(scenario=="sc9") %>%
         mutate(subbasin=as.character(subbasin)),
       aes(P_Concentration, col=subbasin)) +
  stat_ecdf(geom="step")+
  xlim(0, 0.03)+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=0.025, col="red")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10))


ggsave("Pcon_Overlapped_ECDF_subbasin_intensive.jpeg",
       path = graphs_dir,
       width =8,
       height =8
)  

ggplot(res_sub %>% filter(scenario=="scbase") %>%
         mutate(subbasin=as.character(subbasin)),
       aes(P_Concentration, col=subbasin)) +
  stat_ecdf(geom="step")+
  xlim(0, 0.03)+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=0.025, col="red")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10))


ggsave("Pcon_Overlapped_ECDF_subbasin_base.jpeg",
       path = graphs_dir,
       width = 8,
       height = 8
)  

ggplot(res_sub %>% filter(scenario=="sc9") %>%
         mutate(subbasin=as.character(subbasin)),
       aes(N_Concentration, col=subbasin)) +
  stat_ecdf(geom="step")+
  xlim(0, 10)+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=1, col="red")

ggsave("Ncon_Overlapped_ECDF_subbasin_intensive.jpeg",
       path = graphs_dir,
       width = 8,
       height = 8
)  


ggplot(res_sub %>% filter(scenario=="scbase") %>%
         mutate(subbasin=as.character(subbasin)),
       aes(N_Concentration, col=subbasin)) +
  stat_ecdf(geom="step")+
  xlim(0, 10)+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=1, col="red")

ggsave("Ncon_Overlapped_ECDF_subbasin_base.jpeg",
       path = graphs_dir,
       width = 8,
       height = 8
)  

ggplot(res_sub %>% filter(scenario=="scbase") %>%
         mutate(subbasin=as.character(yr)),
       aes(P_Concentration, col=yr)) +
  stat_ecdf(geom="step")+
  xlim(0, 0.03)+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=0.025, col="red")

ggsave("Pcon_Overlapped_ECDF_yr_base.jpeg",
       path = graphs_dir,
       width = 8,
       height = 8
)  

ggplot(res_sub %>% filter(scenario=="scbase", yr>2013) %>%
         mutate(subbasin=as.character(yr)),
       aes(P_Concentration, col=yr)) +
  stat_ecdf(geom="step")+
  xlim(0, 0.03)+
  geom_hline(yintercept=0.5, col="red")+
  geom_vline(xintercept=0.025, col="red")


#Variations-------

#we merge the environmental results data base with the 
#economic mean results at the hectare level
#if, instead, we want to use the total results in the basin
#we should cbind using econ_tot data base


res_sub_outlet <-
  res_sub %>% group_by(scenario) %>%
  filter(subbasin==7) %>%
  summarise(n_mean=mean(N_Concentration),
            n_median=median(N_Concentration),
            n_max=max(N_Concentration),
            p_mean=mean(P_Concentration),
            p_median=median(P_Concentration),
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

#1,2,3, 12, 9, 13

#Riego en Subcuencas---

areas_sub <-
aggregate(area~subbasin+rot, data=hru_info, FUN=sum) %>% 
  group_by(subbasin) %>% 
  mutate(porc_luse=area/sum(area),
         rot_riego=case_when(rot=="agrc3_rot" ~ "1",
                             rot=="agrc4_rot" ~ "6",
                             rot!="agrc3_rot" & rot!="agrc4_rot"  ~ "0")
         ) %>% arrange(subbasin); areas_sub

areas_sub_riego <-
aggregate(area~rot_riego+subbasin, data=areas_sub, FUN=sum)  %>% 
  group_by(subbasin) %>% 
  mutate(porc_luse=area/sum(area),
         no_regado=ifelse(rot_riego=="0",1,0))

porcentaje_rots <-
aggregate(porc_luse~subbasin+rot_riego,
          data=areas_sub_riego,
          FUN=sum) %>%
spread(rot_riego, porc_luse) %>%
rename("rot_0"="0", "rot_1"="1","rot_6"="6") %>%
mutate(porc_regado=rot_1+rot_6,
       porc_no_regado=rot_0)  

#la parte baja de la cuenca es la que tiene mas 
#riego, en particular, 12, 11, 1, 5, 13, 9
#las de menos riego son 10, 3, 2, 7, 6, 4, 8
porcentaje_rots %>%
  arrange(desc(porc_regado))

#12, 11, 1, 5, 13, 9 son las que tienen mas rot 1
porcentaje_rots %>%
  arrange(desc(rot_1))
#12, 11, 1, 5, 13 y 9 son las que tienen mas rot 6
porcentaje_rots %>%
  arrange(desc(rot_6))

subbasin_map@data <-
merge(subbasin_map@data, porcentaje_rots, by="subbasin")

subbasin_map %>% sp::plot()

sub_map <-
fortify(subbasin_map) %>%
mutate(subbasin=as.numeric(id)+1, group=as.numeric(group)) %>%
plyr::join(subbasin_map@data, by="subbasin")  

tag<-
  aggregate(cbind(long, lat)~id, 
            data=fortify(subbasin_map) %>% mutate(id=as.numeric(id)+1, group=as.numeric(group)), function(x)mean(range(x)))

r1y6<-
ggplot()+
geom_polygon(data=sub_map %>% mutate(Rotaciones_1y6=porc_regado*100), 
             aes(x=long,y=lat,group=id,fill=Rotaciones_1y6))+
geom_text(data = tag, aes(long, lat, label=id), size=4)+
theme_void()+
scale_fill_gradient(low="white", high="blue")  

r1<-
ggplot()+
  geom_polygon(data=sub_map %>% mutate(Rotacion_1=rot_1*100), 
               aes(x=long,y=lat,group=id,fill=Rotacion_1))+
  geom_text(data = tag, aes(long, lat, label=id), size=4)+
  theme_void()+
  scale_fill_gradient(low="white", high="blue")  

r6<-
ggplot()+
  geom_polygon(data=sub_map %>% mutate(Rotacion_6=rot_6*100), 
               aes(x=long,y=lat,group=id,fill=Rotacion_6))+
  geom_text(data = tag, aes(long, lat, label=id), size=4)+
  theme_void()+
  scale_fill_gradient(low="white", high="blue")  

gridExtra::grid.arrange(r1, r6, ncol=2) %>% plot

ggsave("Rot1_6.jpeg",
       path = graphs_dir
       #width =
       #height =
)  


ggsave("Rot1y6.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#o sea, las cuencas con mas rot 1 y 6 son practicamente las mismas
#la subcuenca 5 esta muy arriba pero es chica
#la subcuenca 9 esta en la mitad norte de la cuenca mas o menos


ggplot(mat_nitro %>% filter(Escenario!="SC10") , aes(x=ARA, y=Escenario, fill=Elasticidad))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "red")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))


#All Subbasins Results------
#iterate in all subbasins computing:
#nutrient discharge, concentration and econ results
#nutrient discharge & concentration sensivility to scenarios
#nutrient discharge & concentration elasticity 

for(i in 1:13){
  print(i)
  
  res_sub_outlet <-
    res_sub %>% group_by(scenario) %>%
    filter(subbasin==i) %>%
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
  

  
  elasticidades <-
    apply(res_sub_outlet_porcent[,1:6],
          2,
          function(x)(res_sub_outlet_porcent[,7:14]/x)
    )
  
  model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
  setwd(paste0(model_scripts, "Resultados_x_Subcuenca")) 
  
  row<-1
  
  out<- openxlsx::createWorkbook() 
  openxlsx::addWorksheet(out, "Resultados")
  openxlsx::addWorksheet(out, "Variaciones_Brutas")
  openxlsx::addWorksheet(out, "Variaciones_Porcent")
  openxlsx::addWorksheet(out, "elast_nmean")
  openxlsx::addWorksheet(out, "elast_nmax")
  openxlsx::addWorksheet(out, "elast_pmean")
  openxlsx::addWorksheet(out, "elast_pmax")
  openxlsx::addWorksheet(out, "elast_nviol")
  openxlsx::addWorksheet(out, "elast_pviol")
  openxlsx::writeData(out, x=res_sub_outlet, sheet="Resultados")
  openxlsx::writeData(out, x=res_sub_outlet_brutas, sheet="Variaciones_Brutas")
  openxlsx::writeData(out,x=res_sub_outlet_porcent,sheet="Variaciones_Porcent")
  openxlsx::writeData(out,x=elasticidades[[1]],sheet="elast_nmean",startCol=1, startRow = row)
  openxlsx::writeData(out,x=elasticidades[[2]],sheet="elast_nmax",startCol=1, startRow = row)
  openxlsx::writeData(out,x=elasticidades[[3]],sheet="elast_pmean",startCol=1, startRow = row)
  openxlsx::writeData(out,x=elasticidades[[4]],sheet="elast_pmax",startCol=1, startRow = row)
  openxlsx::writeData(out,x=elasticidades[[5]],sheet="elast_nviol",startCol=1, startRow = row)
  openxlsx::writeData(out,x=elasticidades[[6]],sheet="elast_pviol",startCol=1, startRow = row)
  
  openxlsx::saveWorkbook(out, paste0("Resultados_Subcuenca_",i,".xlsx"), overwrite = TRUE)
  
  #write_xlsx(sheets, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha.RDS" )
  #write.xlsx(res_sub_outlet, file= paste0("Resultados_Subbasin_",i,".xlsx"), sheetName="resultados")
  #write.xlsx(res_sub_okkutlet_brutas, paste0("Resultados_Subbasin_",i,".xlsx"),sheetName="res_var_brut", append=TRUE )
  #write.xlsx(res_sub_outlet_porcent, paste0("Resultados_Subbasin_",i,".xlsx"), sheetName="res_var_por", append=TRUE)
  #write.xlsx(elasticidades, paste0("Resultados_Subbasin_",i,".xlsx"), sheetName="res_elasticidades", append=TRUE)
}

#Areas-----

hru_info %>% 
  #  filter(lu_mgt== "agrc3_lum" | "agrc4_lum" ) %>%
  rename(rotacion=landuse) %>%
  mutate(rotacion=case_when(rotacion=="agrc3_lum" ~ "Rotacion_1",
                            rotacion=="agrc4_lum" ~ "Rotacion_6",
                            rotacion!= "agrc3_lum" & rotacion!= "agrc4_lum" ~ "Otra")) %>%
  ggplot(aes(area, after_stat(density), colour=rotacion))+
  geom_freqpoly(binwidth=2)+
  xlim(0,100)+
  ylab("Densidad")+
  xlab("Hectareas")

ggsave("Areas_Dist.jpeg",
       path = graphs_dir
       #width =
       #height =
)  
#
