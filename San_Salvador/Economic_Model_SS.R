#to do: get area
#       compute utility and profit

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
env_restriction_violation <-
ifelse(Excess_Days>0.1,"TRUE","FALSE") %>% 
  as.logical() %>%
  all() ; env_restriction_violation

profit_data <-
  profit_data %>%
  mutate(Rotacion_riego=case_when(lu_mgt=="agrc3_lum" ~ 1,
                                  lu_mgt=="agrc4_lum" ~ 6,
                                  lu_mgt!="agrc3_lum" & lu_mgt!="agrc4_lum" ~ 0)) 

#Fixed Cost=Fixed Prod Cost

#save data
setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))



#Ut Fun, CE and RP----

#vector profit para probar
#coso <- profit_data %>% select(profit_ha,yr ) %>% filter(hru==3)  %>% 
#        as.data.frame() %>%  select(profit_ha)

ne_utility <- function(value, rho=0.005){
  
  utility<-  -exp(-rho*value)
  return(utility)
}


#cara utility for the hru in the whole period
ne_utility_whole <- function(profit_vector, rho=0.005){
  
  
  utility<- sapply(profit_vector, function(x){-exp(-rho*x)}) %>% sum()
  
  return(utility)
}

#
certainty_equivalent_ne <- function(profit_vector, rho=0.005){
  
  inv_rho<- 1/-rho
  
  if(rho==0){
    ce <- profit_vector %>% unlist() %>% mean()
    }else{

   suma<- sapply(profit_vector, function(x){exp(-rho*x)}) %>% sum()
    
   ce <- log( suma/dim(profit_vector)[1])*(1/(-1*rho)) 

           }
  
  return(ce)

} 

#Put the observed profit (E(X)) and CE to estimate risk premium
#calculate the risk premium the farmer is 
#willing to pay to obtain the certainty equivalent
#Risk_Premium=X-Certainty Equivalent

risk_premium_ne <- function(profit_vector, rho=0.005){
  
  exp_val <- profit_vector %>% sum
  
  certainty_equivalent <-
  certainty_equivalent_ne(profit_vector=profit_vector,
                          rho=rho)
  
  risk_premium<- exp_val - certainty_equivalent
  
  return(risk_premium)   
  
} 

#Computing CE----

hru_list <-
  profit_data %>% select(hru) %>% unique() %>% 
  unlist() %>% as.list()

#Ara parameters are defining using the range specified in 
#Babcock Choi and Feinerman, 1993 (Risk and prob premiums for CARA utility functions)

#This range also is similar (and quite wider) to the range specified 
#in Hardaker, Richardson, Lien and Schumann (2003): 
#Stoch aff analysis with risk aversion bounds, a simplified approach

ara_par <- seq(0.000, 0.05, by=0.007) #eight parameters, include risk neutral 0

#


ce_ne <- matrix(nrow = hru_list %>% length(),
                ncol= length(ara_par)) 

rp_ne <- matrix(nrow = hru_list %>% length(),
                ncol=length(ara_par))

#coso <-
#profit_data %>% select(profit_ha,yr ) %>% filter(hru==3)  %>% 
#  as.data.frame() %>%  select(profit_ha)

#create an index variable which indicates the relevant rotations

for(ara in ara_par){
 for (n_hru in hru_list) {
  
ce_ne[match( n_hru, hru_list), match(ara, ara_par)] <-  
certainty_equivalent_ne(profit_vector = 
                        profit_data %>% select(profit_ha,yr ) %>%
                        filter(hru==n_hru)  %>% 
                        as.data.frame() %>%  select(profit_ha),
                        rho=ara
                       )



rp_ne[match( n_hru, hru_list), match(ara, ara_par)] <-  
  risk_premium_ne(profit_vector = 
                            profit_data %>% select(profit_ha,yr ) %>% filter(hru==n_hru)  %>% 
                            as.data.frame() %>%  select(profit_ha),
                  rho=ara)
print(ara)
print(n_hru)

print(c(ce_ne[match( n_hru, hru_list), match(ara, ara_par)],
        rp_ne[match( n_hru, hru_list), match(ara, ara_par)]
      ))  

}
}

resultados_hru <-
  cbind(ce_ne, rp_ne) %>%
  as.data.frame(); View(resultados_hru)


res_names<- c(paste0("ce_ne_", ara_par),
              paste0("rp_ne_", ara_par))

colnames(resultados_hru)<- res_names

saveRDS(resultados_hru, paste0("Econ_Output_HRU_", str_remove( scenario, ".RData"), ".RDS"))
saveRistDS(env_restriction_violation, paste0("Env_Restriction_Violation_", str_remove( scenario, ".RData"), ".RDS") )
saveRDS(environmental_output, paste0("Env_Output_", str_remove( scenario, ".RData"), ".RDS") )
saveRDS(Excess_Days, paste0("Excess_Days_", str_remove( scenario, ".RData"), ".RDS") )

#basin_CE_RP <- apply(resultados_hru, 2, sum)  

save.image(paste0("Econ_Output_", str_remove( scenario, ".RData")))


}  

hru_info <-
hru_info %>%
mutate(Rotacion_riego=case_when(lu_mgt=="agrc3_lum" ~ 1,
                                lu_mgt=="agrc4_lum" ~ 6,
                                lu_mgt!="agrc3_lum" & lu_mgt!="agrc4_lum" ~ 0))


#TOTAL Gain----
#(CE_irr/CE_sinirr)-1
setwd(paste0(model_scripts, "Data_Simulaciones_Eco"))
load()
#CE_2 is the CE of each scenario respect to base
#but with the base scenario volatility

