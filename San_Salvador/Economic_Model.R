rm(list = ls())
load("C:/Users/Usuario/Desktop/Git/Tesis/Datos_Modelo.RData")

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
plyr::join(areas,by="hru") %>%
mutate(price_ton=case_when(crop== "soyb" |
                           crop== "soy2" ~ p_soybean,
                           crop== "wwht" ~ p_wheat,
                           crop== "barl" ~ p_barley,
                           crop== "corn" ~ p_corn,
                           crop== "oats" ~ p_oats)) %>%
mutate(revenue=yield*area*price_ton/1000,
       revenue_ha=yield*price_ton/1000) 
                       
#Cost=Variable Cost+Fixed Cost
#Variable Cost=Water Price.Irrigated Water

output

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
relocate(year, .before=crop)  


output %>% filter(hru==3) %>% View


#Reported price by Santiago Arana is 0.65 usd/mm 
#Reported price by Claudio Garcia for 2017/18 is 1.4

cost_irrigation <- 1.4

irr <-
irr %>%
mutate(irr_cost=irr*cost_irrigation) %>%
select(hru, date_irr, irr, irr_cost)  

irr_yr<-
irr_yr %>%
  mutate(irr_cost=irr_sum*cost_irrigation) 

#Yearly Data----

View(output)

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
       profit_ha=revenue_ha-crop_cost_ha-irr_cost_ha)  

#Fixed Cost=Fixed Prod Cost


#save data
setwd("C:/Users/Usuario/Desktop/Git/Tesis")
save.image("Economic_Profit.RData")

#Utility----

alpha <- 0.88 #Parameters from Kahneman-Tversky and 
              #Adopted by Rosas, Sans and Arana
#See "A parametric analysis of prospect theory's
#functionals for the general population"
#by Booij, Van Praag and Kuilen 2009
#alpha range:0.22 (Camerer and ho 1994) to 1.01 (Fehr-Duda 2006)

lambda <- 2.25 #Parameters from Kahneman-Tversky and 
               #Adopted by Rosas, Sans and Arana
#Lambda range in Booij et al is 1.07 to 3.2

#interpretation: alpha defines the curvature
#               of ut function 
#               lambda is the risk aversion, defined
#               as lambda=-u(-1)/u(1)


profit_data %>% dplyr::select(hru==3)

coso<-profit_data %>% select(profit_ha,yr ) %>% filter(hru==3)  %>% 
as.data.frame() %>%  select(profit_ha)

coso<- coso %>% as.vector

prueba<- c(1000, -1000)
prueba<- prueba  %>% as.matrix(ncol=1)

#put as an input, the farmer profit vector

farmer_utility<- 
function(alpha=0.88, profit, lambda=2.25){

profit <- as.matrix(profit, ncol=1)
n_years <- dim(profit)[1]

utility <- matrix(NA,nrow=n_years, ncol=1)

for (i in 1:n_years) {
  
if(profit[i,1]>=0){utility[i,1]=profit[i,1]^alpha} else
if(profit[i,1]<0){utility[i,1]=-lambda*((-profit[i,1])^alpha)}
}  

tot_value=sum(utility)
return(tot_value)
}

certainty_equivalent <-
function(alpha=0.88, profit, lambda=2.25){
  
  profit <- as.matrix(profit, ncol=1)
  n_years <- dim(profit)[1]
  
  mean_prof <- mean(profit)
  mean_vector <- rep(mean_prof, n_years) %>% 
                 as.matrix( ncol=1)
  
  utility <- matrix(NA,nrow=n_years, ncol=1)
  
  for (i in 1:n_years) {
    
    if(mean_vector[i,1]>=0){utility[i,1]=mean_vector[i,1]^alpha} else
      if(mean_vector[i,1]<0){utility[i,1]=-lambda*((-mean_vector[i,1])^alpha)}
  }  
  
  tot_value=sum(utility)
  
  ref_value <- farmer_utility(profit = profit)

  return(tot_value-ref_value)
}

certainty_equivalent(profit = prof_vec)

hrus <- profit_data$hru %>% unique

utilities <- matrix(nrow=length(hrus), ncol=1)


for (i in hrus){
    print(i)

    prof_vec<- profit_data %>% as.data.frame() %>%
    filter(hru==i) %>% select(profit_ha) 

    utilities[match(i, hrus)] <- farmer_utility(profit=prof_vec)

}

for (i in hrus){
  print(i)
  
  readline(prompt="next")
  
 
}

profit_data %>% as.data.frame() %>%
  filter(hru==3) %>% select(profit_ha) %>%
  View

basin_utility<- 


#Scenarios----
#NECESITO MANDAR UN MAIL A FRANCISCO PARA SABER QUE ESCENARIOS PONER
#O SEA, EN CADA ROTACION, NECESITO DEFINIR UN ESCENARIO DE RIEGO

