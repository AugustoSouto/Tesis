#to do: get area
#       compute utility and profit

rm(list = ls())

library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"

setwd(model_scripts)
#dd
scenarios <-
list.files(model_scripts, pattern="RData"); scenarios

for(scenario in scenarios){
  
print(scenario)  

setwd(model_scripts)  
load(scenario)

}


hru_info <- 
  plyr::join(areas, hrus_rotations, by="hru"); head(hru_info)

#area por rotacion-
hru_info %>% group_by(lu_mgt) %>% 
  summarise(area_rot=sum(area)) %>%
  mutate(pct_rot=area_rot/sum(area_rot)) %>%
write_excel_csv2(paste0("Areas_Rot_", 
                        str_remove( scenario, ".RData"),
                        ".csv"))
    
hru_info %>% group_by(lu_mgt) %>% 
  summarise(area_rot=sum(area), 
            area_prom=mean(area),
            area_mediana=median(area),
            area_max=max(area),
            area_min=min(area), 
            hrus=n()) %>%
  write_excel_csv2(paste0("Areas_Rot_", 
                          str_remove( scenario, ".RData"),
                          ".csv"))


#environmental_output$P_Concentration


#Env Output----
#Use Channel two (2) output,
#since this channel is located the at the basin outlet

#Phosphorus Concentration Limit: 0.25 mg/L
#Nitrogen Concentration Limit: 10 mg/L

Ph_lim <- 0.25
N_lim <- 10



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
ifelse(Excess_Days>0.1,"TRUE","FALSE") %>% 
  as.logical() %>%
  all()

#plot the profits for the whole period

#Fixed Cost=Fixed Prod Cost

#save data
setwd("C:/Users/Usuario/Desktop/Git/Tesis")

saveRDS(paste0(str_remove(scenarios[i], ".RData"), "_profits.RDS"))

readline(prompt = "Press any key only if management scenarios are done and saved")


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

#cara utility in one period
cara_utility <- function(value, rho=0.5){
  ifelse(rho!=0, ((1-exp(-rho*value))/rho), log(value))
}
#discount cara utilities to t=1
disc_cara_utility <- function(profit_vector){
  

  utilities <- apply(profit_vector, 1, cara_utility)

  utility_discount <- 0.075 #see how to calibrate this, meanwhile, use social disc rate
  
  #set the discount rate vector r
  r <- vector(length = dim(profit_vector)[1]) 
  
  
  
  for (i in 2:length(r)) {
    r[1]<-1
    r[i]<-(1/(1+utility_discount))^(i-1) 
  }
   
  mat_r <- matrix(0, ncol=length(r), nrow=length(r))
  diag(mat_r) <- r
  
  discounted_utilities <- t(as.matrix(utilities)) %*% mat_r #DISC FACTOR----
  
  return(discounted_utilities)
  
 }

period_utility <- sum(disc_cara_utility(coso)) 

#
certainty_equivalent <- 
function(period_utility){
  

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

