#to do: get area
#       compute utility and profit
rm(list = ls())
library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"

setwd(model_scripts)
#dd
scenarios <-
list.files(model_scripts, pattern="RData"); scenarios

#for(i in scenarios){
#print(i)  
#load(i)
#readline(prompt = "Press any key only if management scenarios are done and saved")
#}

load("SWAT_Sim_Without_Irrigation.RData")

#Rotation Area----
ggplot(areas, aes(x=area))+
  geom_histogram()

areas %>% select(area) %>% summary()
areas %>% select(area) %>% count()

#load("C:/Users/Usuario/Desktop/Git/Tesis/Datos_Modelo.RData")

areas %>% head
hrus_rotations %>% head

hru_info <- 
  plyr::join(areas, hrus_rotations, by="hru"); head(hru_info)

#area por rotacion-
hru_info %>% group_by(lu_mgt) %>% 
  summarise(area_rot=sum(area)) %>%
  mutate(pct_rot=area_rot/sum(area_rot)) %>%
write_excel_csv2("Areas_Rot.csv")
    
hru_info %>% group_by(lu_mgt) %>% 
  summarise(area_rot=sum(area), 
            area_prom=mean(area),
            area_mediana=median(area),
            area_max=max(area),
            area_min=min(area), 
            hrus=n())

ggplot(hru_info, aes(x=area))+
  geom_histogram()+
  facet_wrap(~lu_mgt)

#Env Output----
#Use Channel two (2) output,
#since this channel is located the at the basin outlet

#Phosphorus Concentration Limit: 0.25 mg/L
#Nitrogen Concentration Limit: 10 mg/L

Ph_lim <- 0.25
N_lim <- 10

environmental_output %>% 
  filter(channel==2) %>% select( N_Concentration) %>% as.ts() %>%
  forecast::autoplot(ylim=c(0,10))

environmental_output %>% 
  filter(channel==2) %>% select( P_Concentration) %>% as.ts() %>%
  forecast::autoplot(ylim=c(0,0.25))

Excess_Days <-
environmental_output %>% 
filter(channel==2) %>%
mutate(P_excess=case_when(P_Concentration>=Ph_lim ~ 1,
                          P_Concentration<Ph_lim ~ 0),
       N_excess=case_when(N_Concentration>=N_lim ~ 1,
                          N_Concentration<N_lim ~ 0)
       ) %>% 
select(P_excess, N_excess)  %>%
apply(2, function(x)round(sum(x)/(length(x)),2))   

#Check if the environmental restictions are violated
ifelse(Excess_Days>0.1,"TRUE","FALSE")

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


output %>% filter(hru==3) %>% View

plyr::join(output, hrus_rotations, by="hru") %>%
select(lu_mgt) %>% table()

plyr::join(output, hrus_rotations, by="hru") %>%
filter(lu_mgt=="eec_lum") %>% View

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
       profit_ha=revenue_ha-crop_cost_ha-irr_cost_ha)  %>%
plyr::join(hrus_rotations, by="hru")


#plot the profits for the whole period
ggplot(  profit_data %>% group_by(hru) %>%
         summarise(profit_ha_tot=sum(profit_ha)) %>%
         plyr::join(hrus_rotations, by="hru"),
       
       aes(x=profit_ha_tot))+
       geom_histogram()+
       facet_grid(~lu_mgt)

ggplot(profit_data %>% group_by(hru) %>%
       summarise(profit_ha_mean=mean(profit_ha)) %>%
       plyr::join(hrus_rotations, by="hru"),aes(x=profit_ha_mean))+
       geom_histogram() +
       facet_grid(~lu_mgt)

#Fixed Cost=Fixed Prod Cost

#save data
setwd("C:/Users/Usuario/Desktop/Git/Tesis")
save.image("Economic_Profit.RData")

#Utility Functions----




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




#vector profit para probar
coso <- profit_data %>% select(profit_ha,yr ) %>% filter(hru==3)  %>% 
        as.data.frame() %>%  select(profit_ha)

farmer_cara_utility <- function(){
  #DISC FACTOR
  
  #beta discount the cash flow
  #consider a 7.5% annual real social discount rate
  #source: Oficina de Planeamiento y Presupuesta
  
  y<-0.075
  #real rate is adjusted by us cpi, estimated at 2.5 p.c:
  #nominal rate
  y<-y+0.025
  
  #monthly effective rate:
  y<-((1+0.075)^(1/12))-1
  
  #discount vector
  r<-vector(length = 72) 
  
  for (i in 2:length(r)) {
    r[1]<-1
    r[i]<-(1/(1+y))^(i-1) 
  }
  
  
  ##HYPERBOLIC DISCOUNT###
  #r<-vector(length = 72)
  #for (i in 2:length(r)) {
  #  r[1]<-1
  #  r[i]<-1/(1+0.1*(i-1)) #same disc factor, faster decay at hyperbolic
  #}
  
  #cash<-matrix(1, nrow=72, ncol=1)
  
  #cash<- profit_tot_year %>% as.matrix()
  
  #discounted flow in million of dollars
  #discounted_flow <- sum(cash*r)/1000000 
  
  
  
  mat_r <- matrix(0, ncol=72, nrow=72)
  diag(mat_r) <- r
  
  
  profit_ha_disc  <- as.matrix(profit_ha) %*% mat_r #DISC FACTOR----
  
}


#put as an input, the farmer profit vector


#Calcuate the farmer utility from profits
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


objective_value <- farmer_utility(profit = coso)

#Put the profit vector and a scala representing the desires utility level
#Certainty Equivalent X_ce such as U(X_ce)=E(U[X]) where X is the observed vector

certainty_equivalent <- 
function(utility_level, profit, alpha=0.88, lambda=2.25){
  
certainty_equivalent<- NA
N <-dim(profit)[1]

if(utility_level>=0){certainty_equivalent = ((utility_level/N)^(1/alpha))*N} else
if(utility_level<0){certainty_equivalent = (-(-utility_level/(lambda*N))^(1/alpha))*N}

return(certainty_equivalent)

} 

certainty_equivalent(utility_level = -200, profit = prof_vec)

#Put the observed profit (E(X)) and CE to estimate risk premium
#calculate the risk premium the farmer is 
#willing to pay to obtain the certainty equivalent
#Risk_Premium=X-Certainty Equivalent

risk_premium <- 
function(profit, certainty_equivalent){
  
  exp_val <- profit %>% sum
  
  return(exp_val - certainty_equivalent)   
  
} 


 
hrus <- profit_data$hru %>% unique

results <- matrix(nrow=length(hrus), ncol=3)


for (i in hrus){
    print(i)

    prof_vec<- profit_data %>% as.data.frame() %>%
    filter(hru==i) %>% select(profit_ha) 

    results[match(i, hrus),1] <- farmer_utility(profit=prof_vec)
    results[match(i, hrus),2] <- sum(prof_vec)
    results[match(i, hrus),3] <- certainty_equivalent(utility_level = farmer_utility(profit=prof_vec),
                                                      profit = prof_vec)

}

results %>% as.data.frame() %>%
            rename(Utility=V1,
                   Profits=V2,
                   certainty_equivalent=V3) %>% 
           mutate(risk_premium=Profits-certainty_equivalent) %>% View

results<-
results %>% as.data.frame() %>% mutate(risk_premium=V2-V3) %>% View



#Utility Aggregation----
 
basin_utility <-  


#Scenarios----
#NECESITO MANDAR UN MAIL A FRANCISCO PARA SABER QUE ESCENARIOS PONER
#O SEA, EN CADA ROTACION, NECESITO DEFINIR UN ESCENARIO DE RIEGO

