#Calculo de la media y varianza de los profits de las HRU en cada escenario#
rm(list = ls())

#Load Data----
library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"
setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))

scenarios <-
  list.files(pattern="RData"); scenarios

for(scenario in scenarios){
  
  print(scenario)  
  
  setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))
  
  load(scenario)
  
  profit_data <-
    data.table::as.data.table(profit_data)
  
  hru_list <-
    profit_data %>% select(hru) %>% unique() %>% 
    unlist() %>% as.list()
  
  hru_prof_params <- 
    matrix(ncol=2, nrow=hru_list %>% length())
  
  for (n_hru in hru_list) {
    print(n_hru)
    
    hru_prof_params[ match( n_hru, hru_list), 1] <-
      profit_data[,c("profit_ha", "hru")][hru==n_hru][,lapply(.SD, mean),][,"profit_ha"] %>% as.numeric()
    
    hru_prof_params[ match( n_hru, hru_list), 2] <-
      profit_data[,c("profit_ha", "hru")][hru==n_hru][,lapply(.SD, sd),][,"profit_ha"] %>% as.numeric()
  }
  
hru_prof_params <-  
hru_prof_params %>% as.data.frame()

rownames(hru_prof_params)<-hru_list

hru_prof_params <-  
hru_prof_params %>% tibble::rownames_to_column("HRU")

colnames(hru_prof_params) <- c("HRU", "Mean", "Sd")

setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))

saveRDS(hru_prof_params,
        paste0("Prof_Volatility_", str_remove( scenario, ".RData"),
               ".RDS"))
  
}
