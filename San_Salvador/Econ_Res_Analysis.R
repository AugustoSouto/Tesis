#ANALISIS DE ESCENARIOS----
#(CE_irr/CE_sinirr)-1
rm(list = ls())

library(tidyverse)

#CARGAR DATOS----
model_scripts <- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
graphs_dir <- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Graficos_Salidas"
docs_res_dir <- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Docs_Resultados"

setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))

#load("Econ_Output_SWAT_Sim_Without_Irrigation.RData")

param_files <- list.files(pattern = "Prof_Volatility_sc"); param_files

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

rname <- paste0("ARA_",seq(0, 0.05, by=0.007)) %>% as.character()

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
ggplot(resultados2 %>% 
         filter(ARA!="ARA_0" ), aes(x=ARA, y=Escenario, fill=CE))+
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
ggplot(resultados_mean2 %>% 
         filter( ARA!="ARA_0.042" & ARA!="ARA_0.049"),
       aes(x=ARA, y=Escenario, fill=CE))+
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
ggplot(resultados2 %>% 
         filter(ARA!="ARA_0.042" & ARA!="ARA_0.049") %>%
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
ggplot(resultados_mean2 %>% 
         filter(ARA!="ARA_0.042" & ARA!="ARA_0.049") %>%
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
ggplot(resultados2 %>% 
         filter(ARA!="ARA_0.042" & ARA!="ARA_0.049") %>% mutate(ARA=str_remove(ARA, "ARA_")), 
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
ggplot(resultados_mean2 %>% 
         filter(ARA!="ARA_0.042" & ARA!="ARA_0.049") %>%
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

  mutate(Scenario=case_when(str_detect(Escenario, "sc1") ~ "Esc_1",
                       str_detect(Escenario, "sc2") ~ "Esc_2",
                       str_detect(Escenario, "sc3") ~ "Esc_3",
                       str_detect(Escenario, "sc4") ~ "Esc_4",
                       str_detect(Escenario, "sc5") ~ "Esc_5",
                       str_detect(Escenario, "sc6") ~ "Esc_6",
                       str_detect(Escenario, "sc7") ~ "Esc_7",
                       str_detect(Escenario, "sc8") ~ "Esc_8",
                       str_detect(Escenario, "sc9") ~ "Esc_9",
                       str_detect(Escenario, "scbase") ~ "Esc_base")) %>%
  select(-Escenario) %>% rename(Escenario=Scenario)
                     
data_ce <- data_ce %>%
           mutate(Escenario=factor(Escenario, 
                                  levels=c("Esc_1", "Esc_2",
                                           "Esc_3", "Esc_4",
                                           "Esc_5", "Esc_6",
                                           "Esc_7", "Esc_8",
                                           "Esc_9", "Esc_base"),
                                  ordered = TRUE))
                        
ggplot(data_ce %>% filter(ARA!="0.049" & ARA!="0.042"), aes(x=Escenario, -CE), y=CE)+
  geom_boxplot()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") + labs(x="Escenario", y="CE")

ggsave("CE_dispersion.jpeg",
       path = graphs_dir
       ,
       width =6.84,
       height =8.5, limitsize = FALSE
)

ggplot(data_ce %>% filter(ARA!="0.049" & ARA!="0.042")
         , aes(x=Escenario, y=CE))+
  geom_boxplot()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=12),
        legend.position="none") + labs(x="Escenario", y="CE")+
  scale_y_continuous(limits = quantile(data_ce$CE, c(0.01, 0.99)))

ggsave("CE_dispersion_2.jpeg",
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


#Save Here Econ Results!----
saveRDS(resultados, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/Res_Econ_Tot.RDS")
saveRDS(resultados_1, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/Res_Econ_Tot_1.RDS")
saveRDS(resultados_6, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/Res_Econ_Tot_6.RDS")

saveRDS(resultados_mean, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha.RDS")
saveRDS(resultados_mean_1, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha_1.RDS")
saveRDS(resultados_mean_6, "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Res_Econ_ha_6.RDS")

#Time Series----

#profit time series----

rm(list = ls())

library(tidyverse)

graphs_dir <- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Graficos_Salidas"
model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))

scenarios <-
  list.files(pattern="sc"); scenarios

for(scenario in scenarios){
  
  print(scenario)  
  
  setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))
  
  load(scenario)
  
  profit_data <-
    profit_data %>%
    mutate(Rotacion_riego=case_when(lu_mgt=="agrc3_lum" ~ 1,
                                    lu_mgt=="agrc4_lum" ~ 6,
                                    lu_mgt!="agrc3_lum" & lu_mgt!="agrc4_lum" ~ 0),
           scenario=str_remove(scenario, ".RData"))
  
  assign(paste0("profit_sc",str_remove(scenario, ".RData") %>% str_remove("sc") ) ,
         profit_data )
  
}

setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador")

sub_hru <-
  readRDS("sub_hru.RDS")

profit_data <-
  do.call(rbind,lapply(ls(pattern = "profit_"), get)) %>%
  filter(Rotacion_riego==1 | Rotacion_riego==6) %>%
  plyr::join( sub_hru, by="hru") %>%
  mutate(scenario=str_remove(scenario, "sc")) %>%
  rename(Escenario=scenario)

econ_stats_yr <-
  profit_data %>% 
  group_by(Escenario, yr) %>%
  select(profit_hru, revenue, irr_cost, crop_cost_hru, hru, area) %>%
  summarise(profit_yr=sum(profit_hru)/sum(area),
            revenue_yr=sum(revenue)/sum(area),
            irr_cost_yr=sum(irr_cost)/sum(area),
            crop_cost_yr=sum(crop_cost_hru)/sum(area)) %>%
  as.data.frame()

econ_stats_yr_rot <-
  profit_data %>% 
  group_by(Escenario, yr, Rotacion_riego) %>%
  select(profit_hru, revenue, irr_cost, crop_cost_hru, hru, area) %>%
  summarise(profit_yr=sum(profit_hru)/sum(area),
            revenue_yr=sum(revenue)/sum(area),
            irr_cost_yr=sum(irr_cost)/sum(area),
            crop_cost_yr=sum(crop_cost_hru)/sum(area)) %>%
  as.data.frame()

econ_stats_yr %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=profit_yr, col=Escenario))+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Beneficio Promedio")



ggsave("Serie_Profits.jpeg",
       path = graphs_dir,
       width =6,
       height =4
)


econ_stats_yr_rot %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=profit_yr, col=Escenario))+
  geom_line()+
  facet_wrap(~Rotacion_riego)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Beneficio Promedio")

ggsave("Serie_Profits_rots.jpeg",
       path = graphs_dir,
       width =12,
       height =4
)

econ_stats_yr %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)),
                             y=revenue_yr, col=Escenario))+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Ingreso Promedio")

ggsave("Serie_Ingresos.jpeg",
       path = graphs_dir,
       width =6,
       height =4
)

econ_stats_yr_rot %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)),
                                 y=revenue_yr, col=Escenario))+
  facet_wrap(~Rotacion_riego)+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Ingreso Promedio")

ggsave("Serie_Ingresos_rot.jpeg",
       path = graphs_dir,
       width =12,
       height =4
)


econ_stats_yr %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=crop_cost_yr, col=Escenario))+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Costos Totales")#+
#  theme(legend.position = "none")

ggsave("Serie_CF_rot.jpeg",
       path = graphs_dir,
       width =6,
       height =4
)

econ_stats_yr %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=crop_cost_yr, col=Escenario))+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Costos Fijos")

econ_stats_yr %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=irr_cost_yr, col=Escenario))+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  xlab("Fecha")+ylab("Costos Riego")

profit_data %>% mutate(Rotacion_riego=as.factor(Rotacion_riego),
                       yr=as.Date(ISOdate(yr,12,31))) %>%
  ggplot(aes(x=yr, y=irr_cost_ha, 
             col=Rotacion_riego))+
  geom_point()+
  facet_wrap(~Escenario)

profit_data %>%
  ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=irr_cost_ha, 
             col=as.factor(Subbasin)))+
  geom_point()+
  facet_wrap(~Escenario)

econ_stats_yr_sub <-
  profit_data %>% 
  group_by(Escenario, yr, Subbasin) %>%
  select(profit_hru, revenue, irr_cost, crop_cost_hru, hru, area) %>%
  summarise(profit_yr=sum(profit_hru)/sum(area),
            revenue_yr=sum(revenue)/sum(area),
            irr_cost_yr=sum(irr_cost)/sum(area),
            crop_cost_yr=sum(crop_cost_hru)/sum(area)) %>%
  as.data.frame()


econ_stats_yr_sub %>% ggplot(aes(x=as.Date(ISOdate(yr,12,31)), y=irr_cost_yr, col=Escenario))+
  geom_line()+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))+
  scale_x_date(#date_labels = "%b%y", 
    date_breaks = "1 year"
    #,
    #date_labels = "%"
  )+
  facet_wrap(~Subbasin)+
  xlab("Fecha")+ylab("Costos Riego")






