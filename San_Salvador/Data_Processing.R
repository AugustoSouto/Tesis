rm(list = ls())

library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"

setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))


scenarios <-
  list.files(pattern="RData"); scenarios

t1<-Sys.time()

#Definir el costo de irrigacion con el que quiero procesar los datos
#Irr_cost----
irr_cost=0.65

scenarios <-
  list.files(pattern="RData")

for(s in scenarios){
  
  print(s)  
  setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))
  load(s)
  
  hru_info <- 
    plyr::join(areas, hrus_rotations, by="hru"); head(hru_info)
  
  #Profit Computation----
  
  #Time Unit:Yearly
  
  #Profit Equation
  #1-Profit=Revenue-Cost
  
  #Prices:
  p_soybean <- 300
  p_wheat <- 195
  p_barley <- 130
  p_corn <- 205 
  p_oats <- 195
  
  #
  #yield is measured in kg/ha
  #area is measured in has
  
  #Revenue=Price.Crop
  output<-
    output %>% as.data.frame() %>%
    dplyr::left_join(areas,by="hru") %>%
    mutate(price_ton=case_when(crop== "soyb" |
                                 crop== "soy2" ~ p_soybean,
                               crop== "wwht" ~ p_wheat,
                               crop== "barl" ~ p_barley,
                               crop== "corn" ~ p_corn,
                               crop== "oats" ~ p_oats)) %>%
    select_at(vars(-ends_with(".y"))) %>%
    rename(area=area.x) %>%
    mutate(revenue=yield*area*price_ton/1000,
           revenue_ha=yield*price_ton/1000) ; head(output)
  
  #Cost=Variable Cost+Fixed Cost
  #Variable Cost=Water Price.Irrigated Water
  
  
  #cost source: okara
  
  #corn 1ra
  #costs are measured in cost/ha (usd/ha)
  
  cost_corn <- 694
  cost_soyb <- 488
  cost_soy2 <- 395
  cost_oats <- 393
  cost_wheat <- 476
  cost_barley <- 539
  
  output <-
    output %>%
    mutate(crop_cost_ha=case_when(crop== "soyb" ~ cost_soyb,
                                  crop== "soy2" ~ cost_soy2,
                                  crop== "wwht" ~ cost_wheat,
                                  crop== "barl" ~ cost_barley,
                                  crop== "corn" ~ cost_corn,
                                  crop== "oats" ~ cost_oats)) %>%
    mutate(crop_cost_hru=crop_cost_ha*area,
           year=lubridate::year(date_end)) %>%
    relocate(year, .before=crop); head(output)  
  
  #plyr::join(output, hrus_rotations, by="hru") %>%
  #select(lu_mgt) %>% table()
  
  #plyr::join(output, hrus_rotations, by="hru") %>%
  #filter(lu_mgt=="eec_lum") %>% View
  
  #Reported price by Santiago Arana is 0.65 usd/mm 
  #Reported price by Claudio Garcia for 2017/18 is 1.4
  
  cost_irrigation <- irr_cost
  
  irr <-
    irr %>%
    mutate(irr_cost=irr*cost_irrigation) %>%
    select(hru, date_irr, irr, irr_cost)  
  
  irr_yr<-
    irr_yr %>%
    mutate(irr_cost=irr_sum*cost_irrigation) 
  
  #Yearly Data----
  
  output_yr <-
    output %>% group_by(hru,year) %>%
    summarise(revenue=sum(revenue),
              revenue_ha=sum(revenue_ha),
              crop_cost_ha=sum(crop_cost_ha),
              crop_cost_hru=sum(crop_cost_hru)
    ) %>%
    rename(yr=year)
  
  profit_data <-
    plyr::join(output_yr, 
               irr_yr,
               by=c("hru", "yr")
    ) %>% 
    select(-irr_sum) %>%
    plyr::join(areas,by="hru") %>%
    mutate(irr_cost=replace_na(irr_cost,0),
           irr_cost_ha=round(irr_cost/area,2)) %>%
    mutate(profit_hru=revenue-crop_cost_hru-irr_cost,
           profit_ha=revenue_ha-crop_cost_ha-irr_cost_ha)  %>%
    plyr::join(hrus_rotations, by="hru")
  
  print(s)
  t2
  readline(prompt = "Press any key only if management scenarios are done and saved")
  
  setwd(paste0(model_scripts, "Data_Simulaciones_SWAT"))
  
  save.image(s)
}

t2<- Sys.time()

print(t2-t1)

  