#ANALISIS DE ESCENARIOS----
#(CE_irr/CE_sinirr)-1
rm(list = ls())

library(tidyverse)

#CARGAR DATOS----
model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
graphs_dir<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Graficos_Salidas"
docs_res_dir<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Docs_Resultados"

setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))

#load("Econ_Output_SWAT_Sim_Without_Irrigation.RData")

param_files <- list.files(pattern = "Prof_Volatility_sc");param_files

for (pf in param_files) {
  
  pfn <- str_remove(pf, pattern = "Prof_Volatility_SWAT_Sim_")
  pfn <- str_remove(pfn, pattern=".RDS")
  
  assign(paste0("par_",pfn) ,
         readRDS(pf) )
  
}

parameters <- Reduce(function(x,y){
  merge(x=x, y=y, by="HRU", all.x=TRUE)},
  list(                     
    par_Prof_Volatility_sc1, par_Prof_Volatility_sc2,
    par_Prof_Volatility_sc3, par_Prof_Volatility_sc4,
    par_Prof_Volatility_sc5, par_Prof_Volatility_sc6,
    par_Prof_Volatility_sc7, par_Prof_Volatility_sc8,
    par_Prof_Volatility_sc9, par_Prof_Volatility_scbase
    
  )
)

nom<- list(                     
  "par_high_Rot1y6", "par_base_Rot1y6", "par_low_Rot1y6",
  "par_high_Rot1", "par_base_Rot1", "par_low_Rot1",
  "par_high_Rot6", "par_base_Rot6", "par_low_Rot6",
  "par_without")

mean_name <- paste0("mean_", nom)
sd_name <- paste0("sd_", nom)

colnames(parameters)[seq(2,20, by=2)] <- mean_name
colnames(parameters)[seq(3,21, by=2)] <- sd_name

parameters <-
  parameters %>% mutate(HRU=as.numeric(HRU)) %>% 
  arrange(HRU)

#PROFITS/ha MEDIA Y SD----
apply(parameters[,-1], 2, mean) %>% sort(decreasing = TRUE)

#media de los profits 
#regar solo la rot 1 parece lo mejor, el parametro 05 es el mas ajustado
#esto va en linea con el parametro sugerido por Claudio García
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#los 3 escenarios de riego no parecen ser mejor que el base para rot 6
apply(parameters[,seq(2,20, by=2)], 2, mean) %>% sort(decreasing = TRUE)

#desviacion estandar de los profits
#la variabilidad mejora en casi todos los escenarios
#el base solo es mejor que los otros escenarios 
#para riego en rot 6 con parametro 04 y 05
apply(parameters[,seq(3,21, by=2)], 2, mean) %>% sort(decreasing = FALSE)


#Promedio y Varianza Yields por escenario----

hru_info <-
  readRDS(paste0(model_scripts, "HRU_info.RDS")) %>% rename(HRU=hru)

parameters <- 
  plyr::join( parameters, hru_info[,-c(2, 3)], by="HRU"); View(parameters)

#MEDIA SOLO EN ROTACIONES 1 Y 6
apply(parameters[,c(seq(2,20, by=2),22)] %>% filter(Rotacion_riego!=0)
      , 2, mean) %>% sort(decreasing = TRUE)

apply(parameters[,c(seq(2,20, by=2),22)] %>% filter(Rotacion_riego==1)
      , 2, mean) %>% sort(decreasing = TRUE)

apply(parameters[,c(seq(2,20, by=2),22)] %>% filter(Rotacion_riego==6)
      , 2, mean) %>% sort(decreasing = TRUE)


#CE por Escenario------

setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))

ce_files <-
  list.files(pattern = "Econ_Output_HRU_sc") %>% str_subset(pattern="RDS");ce_files
#load("Econ_Output_SWAT_Sim_Without_Irrigation.RData")

for (ce in ce_files) {
  
  cen <- str_remove(ce, pattern = "Econ_Output_HRU_")
  cen <- str_remove(cen, pattern=".RDS")
  cen <- str_remove(cen, pattern="SWAT_Sim_")
  assign(paste0("cen_",cen) ,
         readRDS(ce) %>%
           magrittr::set_colnames( paste0(cen, "_", colnames(readRDS(ce))))
  )
  
}

ces  <-
  list(                     
    cen_sc1, cen_sc2, cen_sc3,
    cen_sc4, cen_sc5, cen_sc6,
    cen_sc7, cen_sc8, cen_sc9,
    cen_scbase
      )

cnames<- lapply(ces, function(x){colnames(x)}) %>% unlist() 

ceq_rp <- Reduce(function(x,y){cbind(x=x, y=y)},ces) 

colnames(ceq_rp) <- cnames

#Calc CE----
#LOS RESULTADOS ESTAN POR HA, HAY QUE MULTIPLICAR POR 
#EL AREA PARA OBTENER EL RESULTADO A NIVEL DE CUENCA

ceq_rp <-
  cbind(parameters %>% dplyr::select("HRU", "Rotacion_riego"), ceq_rp)

ceq_rp %>% dplyr::select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  apply(2, sum)

#CE_SUM SCENARIO----
#ce a nivel de HRU
resultados<-
ceq_rp %>% dplyr::select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego"))

resultados_1<-
  ceq_rp %>% dplyr::select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego==1) %>% select(-c("HRU", "Rotacion_riego"))

resultados_6<-
  ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego==6) %>% select(-c("HRU", "Rotacion_riego"))

ar<-
hru_info %>% filter(Rotacion_riego!=0) %>% select(area)

ar_1<-
  hru_info %>% filter(Rotacion_riego==1) %>% select(area)

ar_6<-
  hru_info %>% filter(Rotacion_riego==6) %>% select(area)

#resultados con CE_ha*has para cada hru
resultados_total<-
mutate_all(resultados, function(x)x*ar) %>% 
  as.matrix() %>% as.data.frame() 

#media ponderada por el peso del area de la hru
resultados_mean <-
apply(resultados,2, function(x)sum(x*ar)/sum(ar)) %>%
  matrix(nrow=8, ncol=10)

resultados_mean_1 <-
  apply(resultados_1,2, function(x)sum(x*ar_1)/sum(ar_1)) %>%
  matrix(nrow=8, ncol=10)

resultados_mean_6 <-
  apply(resultados_6,2, function(x)sum(x*ar_6)/sum(ar_6)) %>%
  matrix(nrow=8, ncol=10)

#CE_ha MEAN, sin weight por area de la hru----
ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, mean) %>% sort(decreasing = TRUE)

rname <- paste0("ARA_",seq(0, 0.5, by=0.07)) %>% as.character()

cname <- c("Rot1y6_high", "Rot1y6_base", "Rot1y6_low",
           "Rot1_high", "Rot1_base", "Rot1_low",
           "Rot6_high", "Rot6_base", "Rot6_low",
           "base")
#suma de CEs en todas las hru de las rot 1y6
resultados<-
  apply(resultados_total, 2, sum) %>%  matrix(nrow=8, ncol=10) %>% 
  as.data.frame(row.names=rname)

resultados_mean <-
resultados_mean %>% 
  as.data.frame(row.names=rname)

resultados_mean_1 <-
  resultados_mean_1 %>% 
  as.data.frame(row.names=rname)

resultados_mean_6 <-
  resultados_mean_6 %>% 
  as.data.frame(row.names=rname)

colnames(resultados)<-cname
colnames(resultados_mean)<-cname

colnames(resultados_mean_1)<-cname
colnames(resultados_mean_6)<-cname

#Los promedios de los CE son mayores en las 
#hru regadas de rotacion 6

resultados2 <-
  reshape2::melt(data.table::setDT(resultados, keep.rownames = TRUE), "rn") 

resultados_mean2 <-
  reshape2::melt(data.table::setDT(resultados_mean, keep.rownames = TRUE), "rn") 

colnames(resultados2)<- c("ARA", "Escenario", "CE")
colnames(resultados_mean2)<- c("ARA", "Escenario", "CE")

library(scales)


#HEATMAP SCENARIO&ARA----
ggplot(resultados2 %>% filter(ARA!="ARA_0"), aes(x=ARA, y=Escenario, fill=CE))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "red", labels=comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))

ggsave("CE_Calor_ARA.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#HEATMAP MEAN----
ggplot(resultados_mean2 %>% filter(ARA!="ARA_0"), aes(x=ARA, y=Escenario, fill=CE))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "red", labels=comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))

ggsave("CE_Calor_ARA_mean.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#SCENARIO CE FACET-----
ggplot(resultados2 %>% filter(ARA!="ARA_0") %>%
         mutate(ARA=str_remove(ARA, "ARA_")), aes(x=ARA, y=CE, col="red"))+
  geom_point()+
  facet_wrap(~Escenario, nrow=4, ncol=3)+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12),
        legend.position="none") + labs(x="Escenario", y="CE")

ggsave("CE_Cuenca_ARA.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#SCENARIO MEAN CE FACET-----
ggplot(resultados_mean2 %>% filter(ARA!="ARA_0") %>%
         mutate(ARA=str_remove(ARA, "ARA_")), aes(x=ARA, y=CE, col="red"))+
  geom_point()+
  facet_wrap(~Escenario, nrow=4, ncol=3)+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12),
        legend.position="none") + labs(x="Escenario", y="CE")

ggsave("CE_Cuenca_ARA_mean.jpeg",
       path = graphs_dir
       #width =
       #height =
       )

#ARA CE FACET-----
ggplot(resultados2 %>% filter(ARA!="ARA_0") %>% mutate(ARA=str_remove(ARA, "ARA_")), 
       aes(x=reorder(Escenario, -CE), y=CE, col=CE))+
  geom_point()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5),
        legend.position="none") + labs(x="Escenario", y="CE")

ggsave("CE_Cuenca_ARA2.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#ARA CE MEAN FACET -----
ggplot(resultados_mean2 %>% filter(ARA!="ARA_0") %>%
         mutate(ARA=str_remove(ARA, "ARA_")), aes(x=reorder(Escenario,-CE), y=CE, col=CE))+
  geom_point()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5),
        legend.position="none") + labs(x="Escenario", y="CE")

ggsave("CE_Cuenca_ARA2_mean.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#CE BOXPLOT----

ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, sum) %>% sort(decreasing = TRUE) 

data_ce <-
ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  reshape2::melt()  

colnames(data_ce)<-c("Escenario", "CE")

data_ce <-
data_ce %>% mutate(ARA=case_when(str_detect(Escenario, "ne_0.049") ~ "0.049",
                                 str_detect(Escenario, "ne_0.042") ~ "0.042",
                                 str_detect(Escenario, "ne_0.035") ~ "0.035",
                                 str_detect(Escenario, "ne_0.028") ~ "0.028",
                                 str_detect(Escenario, "ne_0.021") ~ "0.021",
                                 str_detect(Escenario, "ne_0.014") ~ "0.014",
                                 str_detect(Escenario, "ne_0.007") ~ "0.007",
                                 str_detect(Escenario, "ne_0") ~ "0")) %>%

  mutate(Scenario=case_when(str_detect(Escenario, "sc1") ~ "sc1",
                       str_detect(Escenario, "sc2") ~ "sc2",
                       str_detect(Escenario, "sc3") ~ "sc3",
                       str_detect(Escenario, "sc4") ~ "sc4",
                       str_detect(Escenario, "sc5") ~ "sc5",
                       str_detect(Escenario, "sc6") ~ "sc6",
                       str_detect(Escenario, "sc7") ~ "sc7",
                       str_detect(Escenario, "sc8") ~ "sc8",
                       str_detect(Escenario, "sc9") ~ "sc9",
                       str_detect(Escenario, "scbase") ~ "scbase")) %>%
  select(-Escenario) %>% rename(Escenario=Scenario)
                     
                        
ggplot(data_ce  , aes(x=reorder(Escenario, -CE), y=CE))+
  geom_boxplot()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") + labs(x="Escenario", y="CE")

ggsave("CE_ARA_0.07_Boxplot.jpeg",
       path = graphs_dir
       ,
       width =6.84,
       height =8.5, limitsize = FALSE
)

ggplot(data_ce, aes(x=reorder(Escenario,-CE), y=CE))+
  geom_boxplot()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12),
        legend.position="none") + labs(x="Escenario", y="CE")+
  scale_y_continuous(limits = quantile(data_ce$CE, c(0.01, 0.99)))

ggsave("CE_ARA_Boxplot.jpeg",
       path = graphs_dir
       ,
       width =6.84,
       height =8.5, limitsize = FALSE
)

#TABLAS CE----

setwd(docs_res_dir)

resultados %>% tibble::rownames_to_column("ARA") %>% write_csv2("CE_Escenarios.csv")
resultados_mean %>% tibble::rownames_to_column("ARA") %>% write_csv2("CE_Escenarios.csv",
                                                                     append = TRUE)

#Resultados Ambientales-------


ggplot(data_env %>% filter(channel==1),
       aes(x=reorder(scenario,-N_Concentration), y=N_Concentration))+
  geom_boxplot()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") +
     labs(x="Escenario", y="N_Concentration")

ggsave("Nit_Boxplot.jpeg",
       path = graphs_dir
       #width =
       #height =
)




ggplot(data_env %>% filter(channel==2) %>% mutate(mon=as.numeric(mon)),
       aes(x=reorder(scenario, -N_Concentration), y=N_Concentration))+
  geom_boxplot()+
  facet_wrap(~mon #, scales = "free_y"
             )+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5),
        legend.position="none") +
  labs(x="Escenario", y="N_Concentration")

ggsave("Nit_Boxplot_scenario.jpeg",
       path = graphs_dir
       #width =
       #height =
)

ggplot(data_env %>% filter(channel==2) %>% mutate(mon=as.numeric(mon)),
       aes(x=reorder(scenario, -P_Concentration), y=P_Concentration))+
  geom_boxplot()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") +
  labs(x="Escenario", y="P_Concentration")

ggsave("Ph_Boxplot.jpeg",
       path = graphs_dir
       #width =
       #height =
)

ggplot(data_env %>% filter(channel==2) %>% mutate(mon=as.numeric(mon)),
       aes(x=scenario, y=P_Concentration))+
  geom_boxplot()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") +
  labs(x="Escenario", y="P_Concentration")+
  facet_wrap(~mon , scales = "free_y"
             )

ggsave("Ph_Scenarios_Boxplot.jpeg",
       path = graphs_dir
       #width =
       #height =
)

ggplot(data_env %>% filter(channel==2) %>% mutate(mon=as.numeric(mon)),
       aes(x=scenario, y=flo_out))+
  geom_boxplot()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") +
  labs(x="Escenario", y="Caudal")

ggsave("Caudal_Boxplot.jpeg",
       path = graphs_dir
       #width =
       #height =
)


ggplot(data_env %>% filter(channel==2) %>% mutate(mon=as.numeric(mon)),
       aes(x=scenario, y=flo_out))+
  geom_boxplot()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") +
  labs(x="Escenario", y="Caudal")+
  facet_wrap(~mon, scales = "free_y")

ggsave("Caudal_Escenario_Boxplot.jpeg",
       path = graphs_dir
       #width =
       #height =
)

#Subbasin_Env_Res----

env_sub_dir <-
"C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Resultados_Ambientales/Subbasin_Env_Results_Scenarios.RDS"

#umbrales ambientales 
p_threshold <- 0.025 #normativa
n_threshold <- 1 #sugerido en doc mgap 

res_sub <-
readRDS(env_sub_dir)

res_sub

res_sub %>% mutate(P_viol=case_when(P_Concentration>=p_threshold ~ 1,
                                    P_Concentration< p_threshold ~ 0),
                   N_viol=case_when(N_Concentration>=n_threshold ~ 1,
                                    N_Concentration< n_threshold ~ 0)) %>%
group_by(scenario) %>%
summarise(N_v=sum(N_viol)/length(N_viol),
          P_v=sum(P_viol)/length(P_viol))  

res_sub %>% mutate(P_viol=case_when(P_Concentration>=p_threshold ~ 1,
                                    P_Concentration< p_threshold ~ 0),
                   N_viol=case_when(N_Concentration>=n_threshold ~ 1,
                                    N_Concentration< n_threshold ~ 0)) %>%
  group_by(scenario, yr) %>%
  summarise(N_v=sum(N_viol)/length(N_viol),
            P_v=sum(P_viol)/length(P_viol))

res_sub %>% mutate(P_viol=case_when(P_Concentration>=p_threshold ~ 1,
                                    P_Concentration< p_threshold ~ 0),
                   N_viol=case_when(N_Concentration>=n_threshold ~ 1,
                                    N_Concentration< n_threshold ~ 0)) %>%
  group_by( subbasin) %>%
  summarise(N_v=sum(N_viol)/length(N_viol),
            P_v=sum(P_viol)/length(P_viol))




#Elasticidades-------

#si se quiere usar el valor total de los CE en la cuenca (rot1y6)
#en vez de los valores prom por ha, usar resultados en vez
#de resultados_mean

variaciones<-
data_env %>% group_by(scenario) %>%
  filter(channel==2) %>%
  summarise(n_mean=mean(N_Concentration),
            n_max=max(N_Concentration),
            p_mean=mean(P_Concentration),
            p_max=max(P_Concentration)) %>%
  cbind(resultados_mean %>% select(-rn) %>% t())

colnames(variaciones)[6:13] <- 
  paste0( "ARA_" , seq(0,0.049, by=0.007))

variaciones_brutas<-
apply(variaciones[,-1],
      1,
      function(x)(x-variaciones[10,-1])
      ) %>% 
      map_df(rbind)  

variaciones_porcent <-
  apply(variaciones[,-1],
        1,
        function(x)(x/variaciones[10,-1])
        ) %>% 
          map_df(rbind)  
  
variaciones_mixta <- #aca dejo la var porcent de N y P
                     #con la bruta de los CE
  apply(variaciones[,2:5],
        1,
        function(x)(x/variaciones[10,2:5])
  ) %>% 
  map_df(rbind)  %>% cbind(
    apply(variaciones[,6:13],
          1,
          function(x)(x-variaciones[10,6:13])
    ) %>% 
      map_df(rbind)
  )



#elasticidades----
elasticidades <-
apply(variaciones_1[,1:4],
      2,
      function(x)(variaciones_1[,5:12]/x)
      )
rownames(elasticidades[[1]] )<- paste0("SC", seq(1,10,by=1))

reshape2::melt(elasticidades[[1]], )

mat_nitro<-
reshape2::melt(data.table::setDT(elasticidades[[1]], keep.rownames = TRUE), "rn") 
colnames(mat_nitro)<-c("Escenario", "ARA", "Elasticidad")

mat_nitro %>%
mutate(Elasticidad_ha=Elasticidad/sum(ar))




ggplot(mat_nitro %>% filter(Escenario!="SC10") , aes(x=ARA, y=Escenario, fill=Elasticidad))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "red", labels=comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))

variaciones[1,-1]/variaciones[10,-1]

for (i in 1:9) {
  (variaciones[i,-1]-variaciones[10,-1]) %>% print()
}
variaciones[1,-1]-variaciones[10,-1]
variaciones[2,-1]-variaciones[10,-1]
variaciones[3,-1]-variaciones[10,-1]
variaciones[4,-1]-variaciones[10,-1]
variaciones[5,-1]-variaciones[10,-1]
variaciones[6,-1]-variaciones[10,-1]
variaciones[7,-1]-variaciones[10,-1]
variaciones[8,-1]-variaciones[10,-1]
variaciones[9,-1]-variaciones[10,-1]

variaciones[,-1]-variaciones[10,-1]


#hacer todo respecto al esc base
variaciones %>% select(-scenario) %>% as.matrix() %>%
  diff %>% as.data.frame()
  mutate_at(vars(-scenario), diff)

resultados %>% group_by(rn) %>% mean(base)
resultados %>% select(-rn) %>% t() 
  
variaciones_1<-
variaciones %>% select(-scenario) %>% as.matrix() %>%
diff %>% as.data.frame()

colnames(variaciones_1)[5:12] <- 
  paste0( "ARA_" , seq(0,0.049, by=0.007))

variaciones_1 %>% mutate(incremento_0=n_mean/ARA_0,
                         incremento_0.007=n_mean/ARA_0.007,
                         incremento_0.14=n_mean/ARA_0.014,
                         incremento_0.21=n_mean/ARA_0.021,
                         incremento_0.28=n_mean/ARA_0.028,
                         incremento_0.35=n_mean/ARA_0.035,
                         incremento_0.42=n_mean/ARA_0.042,
                         incremento_0.49=n_mean/ARA_0.049)
