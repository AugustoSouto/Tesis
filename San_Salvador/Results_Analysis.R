#ANALISIS DE ESCENARIOS----
#(CE_irr/CE_sinirr)-1
rm(list = ls())

library(tidyverse)

#CARGAR DATOS----
model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
graphs_dir<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Graficos_Salidas"

setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))
list.files(pattern = "RDS")
#load("Econ_Output_SWAT_Sim_Without_Irrigation.RData")

prof_params <-
  readRDS("Prof_Volatility_SWAT_Sim_Without_Irrigation.RDS")

param_files <- list.files(pattern = "Prof_Volatility")

for (pf in param_files) {
  
  pfn <- str_remove(pf, pattern = "Prof_Volatility_SWAT_Sim_")
  pfn <- str_remove(pfn, pattern=".RDS")
  
  assign(paste0("par_",pfn) ,
         readRDS(pf) )
  
}

parameters <- Reduce(function(x,y){
  merge(x=x, y=y, by="HRU", all.x=TRUE)},
  list(                     
    par_03_Rot1, par_03_Rot1y6, par_03_Rot6,
    par_04_Rot1, par_04_Rot1y6, par_04_Rot6,
    par_05_Rot1, par_05_Rot1y6, par_05_Rot6,
    par_Without_Irrigation
  )
)

nom<- list(                     
  "par_03_Rot1", "par_03_Rot1y6", "par_03_Rot6",
  "par_04_Rot1", "par_04_Rot1y6", "par_04_Rot6",
  "par_05_Rot1", "par_05_Rot1y6", "par_05_Rot6",
  "par_Without_Irrigation")

mean_name <- paste0("mean_", nom)
sd_name <- paste0("sd_", nom)

colnames(parameters)[seq(2,20, by=2)] <- mean_name
colnames(parameters)[seq(3,21, by=2)] <- sd_name

parameters <-
  parameters %>% mutate(HRU=as.numeric(HRU)) %>% 
  arrange(HRU)

#PROFITS MEDIA Y SD----
apply(parameters[,-1], 2, mean)

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
  list.files(pattern = "Econ_Output") %>% str_subset(pattern="RDS")
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
    cen_03_Rot1, cen_03_Rot1y6, cen_03_Rot6,
    cen_04_Rot1, cen_04_Rot1y6, cen_04_Rot6,
    cen_05_Rot1, cen_05_Rot1y6, cen_05_Rot6,
    cen_Without_Irrigation
  )

cnames<- lapply(ces, function(x){colnames(x)}) %>% unlist() 

ceq_rp <- Reduce(function(x,y){cbind(x=x, y=y)},ces) 

colnames(ceq_rp) <- cnames

#Calc CE----

ceq_rp <-
  cbind(parameters %>% select("HRU", "Rotacion_riego"), ceq_rp)

ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  apply(2, sum)

#CE SUM SCENARIO----
ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, sum) %>% sort(decreasing = TRUE)

#CE MEAN SCENARIO----
ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, mean) %>% sort(decreasing = TRUE)

rname <- paste0("ARA_",seq(0, 0.5, by=0.07)) %>% as.character()

cname <- c("Rot_1_0.3", "Rot_1y6_0.3","Rot_6_0.3",
           "Rot_1_0.4", "Rot_1y6_0.4","Rot_6_0.4",
           "Rot_1_0.5", "Rot_1y6_0.5","Rot_6_0.5",
           "Base_Sin_Riego")

resultados<-
  ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, sum) %>%  matrix(nrow=8, ncol=10) %>% 
  as.data.frame(row.names=rname)

resultados_mean<-
  ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2,mean) %>%  matrix(nrow=8, ncol=10) %>% 
  as.data.frame(row.names=rname)

colnames(resultados)<-cname
colnames(resultados_mean)<-cname


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
ggplot(coso2 %>% filter(ARA!="ARA_0") %>% mutate(ARA=str_remove(ARA, "ARA_")), 
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

  mutate(Scenario=case_when(str_detect(Escenario, "03_Rot1") ~ "03_Rot1",
                       str_detect(Escenario, "03_Rot1y6") ~ "03_Rot1y6",
                       str_detect(Escenario, "03_Rot6") ~ "03_Rot6",
                       str_detect(Escenario, "04_Rot1") ~ "04_Rot1",
                       str_detect(Escenario, "04_Rot1y6") ~ "04_Rot1y6",
                       str_detect(Escenario, "04_Rot6") ~ "04_Rot6",
                       str_detect(Escenario, "05_Rot1") ~ "05_Rot1",
                       str_detect(Escenario, "05_Rot1y6") ~ "05_Rot1y6",
                       str_detect(Escenario, "05_Rot6") ~ "05_Rot6",
                       str_detect(Escenario, "Without_Irrigation") ~ "Without_Irrigation")) %>%
  select(-Escenario) %>% rename(Escenario=Scenario)
                     
                        
ggplot(data_ce, aes(x=Escenario, y=CE))+
  geom_boxplot()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5),
        legend.position="none") + labs(x="Escenario", y="CE")

ggplot(data_ce, aes(x=reorder(Escenario,-CE), y=CE))+
  geom_boxplot()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=10),
        legend.position="none") + labs(x="Escenario", y="CE")+
  scale_y_continuous(limits = quantile(data_ce$CE, c(0.01, 0.99)))









ggplot(resultados_mean2 %>% filter(ARA!="ARA_0") %>%
         mutate(ARA=str_remove(ARA, "ARA_")), aes(fct_reorder(Escenario,-CE), CE, col=CE))+
  geom_point()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5),
        legend.position="none")+
  labs(x="Escenario", y="CE")



scale_fill_gradient(low = "yellow", high = "red", labels=comma)

coso2$Escenario <- as.factor(coso2$Escenario)



ceq_rp %>% select(contains("ce_ne")) %>%
  apply(2, sum) %>% sort(decreasing = TRUE)



ceq_rp %>% select("HRU", "Rotacion_riego",contains("rp_ne")) %>% 
  apply(2, sum)


#parameters <-
#  parameters %>% mutate(HRU=as.numeric(HRU)) %>% 
#  arrange(HRU)



profit_data %>% select(profit_ha, Rotacion_riego ) %>%
  filter(hru==n_hru)  %>% 
  as.data.frame() %>%  select(profit_ha)
#CE_2 is the CE of each scenario respect to base
#but with the base scenario volatility

