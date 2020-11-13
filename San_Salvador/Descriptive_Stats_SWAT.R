rm(list = ls())
library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"

setwd(model_scripts)

scenarios <-
  list.files(model_scripts, pattern="RData"); scenarios

for(scenario in scenarios){
  
  print(scenario)  
  setwd(model_scripts)  
  load(scenario)
  
  area_plot <-
    ggplot(areas, aes(x=area))+
    geom_histogram()+
    xlab("Area en Hectareas")+
    ylab("HRUs")
  
  ggsave(paste0("area_plot_", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
         )  
  
  ggplot(hru_info, aes(x=area))+
    geom_histogram()+
    facet_wrap(~lu_mgt)+
    xlab("Area en Hectareas")+
    ylab("HRUs")
  
  ggsave(paste0("area_plot_rot", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
         )  
  
  
  Ph_lim <- 0.25
  N_lim <- 10
  
  
  ggplot(environmental_output %>%
           filter(channel==2) %>% 
           select( N_Concentration, date_env), aes(x=date_env, y=N_Concentration)
  ) +
    geom_line(size=1) +
    xlab("Fecha") +
    ylab("Mg/L") +
    geom_hline(aes(yintercept = N_lim,
                   colour="Normativa")) + 
    ylim(0, 50) +
    labs(color=NULL, x="Fecha", y="Mg/L")+
    ggtitle("Concentración de Nitrógeno")+
    scale_x_date(#date_labels = "%b%y", 
      date_breaks = "6 month"
      #,
      #date_labels = "%"
    )+
    theme(axis.text.x =element_text(angle=90,
                                    hjust=1),
          text = element_text(size=7.5))
  
  ggsave(paste0("Nitrogeno", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
  )  
  
  
  ggplot(environmental_output %>%
           filter(channel==2) %>% 
           select( P_Concentration, date_env), aes(x=date_env,
                                                   y=P_Concentration)
  )+
    geom_line(size=1) +
    xlab("Fecha") +
    ylab("Mg/L") +
    geom_hline(aes(yintercept = Ph_lim,
                   colour="Normativa")) + 
    ylim(0, 2) +
    labs(color=NULL, x="Fecha", y="Mg/L")+
    ggtitle("Concentración de Fósforo")+
    scale_x_date(#date_labels = "%b%y", 
      date_breaks = "6 month"
      #,
      #date_labels = "%"
    )+
    theme(axis.text.x =element_text(angle=90,
                                    hjust=1),
          text = element_text(size=7.5))
  
  ggsave(paste0("Fosforo", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
  )  
  
  ggplot(environmental_output %>%
           filter(channel==2) %>% 
           select( flo_out, date_env), aes(x=date_env, y=flo_out)
  ) +
    geom_line(size=1) +
    xlab("Fecha") +
    ylab("Mg/L") +
    labs(color=NULL, x="Fecha", y="M3/Seg") +
    ggtitle("Caudal") +
    scale_x_date(#date_labels = "%b%y", 
      date_breaks = "6 month"
      #,
      #date_labels = "%"
    )+
    theme(axis.text.x =element_text(angle=90,
                                    hjust=1),
          text = element_text(size=7.5))
  
  ggsave(paste0("Caudal", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
  )  

  ggplot(  profit_data %>% group_by(hru) %>%
             summarise(profit_ha_tot=sum(profit_ha)) %>%
             plyr::join(hrus_rotations, by="hru"),
           
           aes(x=profit_ha_tot))+
    geom_histogram()+
    facet_wrap(~lu_mgt)
  
  ggplot(profit_data %>% group_by(hru) %>%
           summarise(profit_ha_mean=mean(profit_ha)) %>%
           plyr::join(hrus_rotations, by="hru"),aes(x=profit_ha_mean))+
    geom_histogram() +
    facet_wrap(~lu_mgt)
  
  profit_data %>% group_by(yr) %>%
    summarise(profit_ha_mean=mean(profit_ha)) %>%
    write_csv(paste0("Profits_Year", str_remove( scenario, ".RData"))
      ,path = model_scripts
      )
  
  ggplot(profit_data , aes(x=profit_ha))+
    geom_histogram() +
    facet_wrap(~yr) +
    geom_vline(aes(xintercept = 0,
                   colour="Perdida/Beneficio")) +
    xlim(-1000, 1000)
  
ggsave(paste0("Profits_Year", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
  )  
  
  ggsave(paste0("Profit_ha_mean", str_remove( scenario, ".RData"), ".jpeg"),
         path = model_scripts
         #width =
         #height =
  )  
  
    
  
    
  
    hru_info %>% group_by(lu_mgt) %>% 
    summarise(area_rot=sum(area)) %>%
    mutate(pct_rot=area_rot/sum(area_rot)) # %>%
#    write_excel_csv2("Areas_Rot.csv")  

    hru_info %>% group_by(lu_mgt) %>% 
    summarise(area_rot=sum(area), 
              area_prom=mean(area),
              area_mediana=median(area),
              area_max=max(area),
              area_min=min(area), 
              hrus=n() )  
      
}
