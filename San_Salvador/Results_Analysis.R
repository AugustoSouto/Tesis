#Join Data----
#(CE_irr/CE_sinirr)-1
rm(list = ls())

library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
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

apply(parameters[,-1], 2, mean)
apply(parameters[,seq(2,20, by=2)], 2, mean)
apply(parameters[,seq(3,21, by=2)], 2, mean)


#Promedio y Varianza Yields por escenario----

hru_info <-
  readRDS(paste0(model_scripts, "HRU_info.RDS")) %>% rename(HRU=hru)

parameters <- 
  plyr::join( parameters, hru_info[,-c(2, 3)], by="HRU"); View(parameters)

apply(parameters[,c(seq(2,20, by=2),22)] %>% filter(Rotacion_riego!=0)
      , 2, mean)
apply(parameters[,c(seq(2,20, by=2),22)] %>% filter(Rotacion_riego==1)
      , 2, mean)
apply(parameters[,c(seq(2,20, by=2),22)] %>% filter(Rotacion_riego==6)
      , 2, mean)


apply(parameters[,seq(3,21, by=2)], 2, mean)


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

ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego==1) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, sum) %>% sort(decreasing = TRUE)

rname <- paste0("ARA_",seq(0, 0.5, by=0.07)) %>% as.character()
cname <- c("Rot_1_0.3", "Rot_1y6_0.3","Rot_6_0.3",
           "Rot_1_0.4", "Rot_1y6_0.4","Rot_6_0.4",
           "Rot_1_0.5", "Rot_1y6_0.5","Rot_6_0.5",
           "Base_Sin_Riego")

coso<-
  ceq_rp %>% select("HRU", "Rotacion_riego",contains("ce_ne")) %>%
  filter(Rotacion_riego!=0) %>% select(-c("HRU", "Rotacion_riego")) %>%
  apply(2, sum) %>%  matrix(nrow=8, ncol=10) %>% 
  as.data.frame(row.names=rname)

colnames(coso)<-cname


coso2 <-
  reshape2::melt(data.table::setDT(coso, keep.rownames = TRUE), "rn") 
colnames(coso2)<- c("ARA", "Escenario", "CE")

library(scales)
ggplot(coso2 %>% filter(ARA!="ARA_0"), aes(x=ARA, y=Escenario, fill=CE))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "red", labels=comma)

ggplot(coso2 %>% filter(ARA!="ARA_0") %>%
         mutate(ARA=str_remove(ARA, "ARA_")), aes(x=ARA, y=CE, col="red"))+
  geom_point()+
  facet_wrap(~Escenario, nrow=4, ncol=3)+
  scale_y_continuous(labels = comma)


ggplot(coso2 %>% filter(ARA!="ARA_0") %>%
         mutate(ARA=str_remove(ARA, "ARA_")), aes(x=Escenario, y=CE, col="red"))+
  geom_point()+
  facet_wrap(~ARA, ncol=2, scales = "free_y")+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x =element_text(angle=90,
                                  hjust=1),
        text = element_text(size=7.5))


plotly::ggplotly(
  ggplot(coso2 %>% filter(ARA!="ARA_0") %>%
           mutate(ARA=str_remove(ARA, "ARA_")), aes(x=ARA, y=CE))+
    geom_line(aes(colour=Escenario, group=Escenario))+
    scale_y_continuous(labels = comma)+
    geom_jitter()
)




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

