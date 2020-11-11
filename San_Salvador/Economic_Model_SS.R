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

save.image("Economic_Profit.RData")
#Utility Functions----

alpha <- 0.88 #Parameters from Kahneman-Tversky and 
              #Adopted by Rosas, Sans and Arana
#See "A parametric analysis of prospect theory's
#functionals for the general population"
#by Booij, Van Praag and Kuilen 2009
#alpha range:0.22 (Camerer and ho 1994) to 1.01 (Fehr-Duda 2006)

#interpretation: alpha defines the curvature
#                of ut function 

#vector profit para probar
coso <- profit_data %>% select(profit_ha,yr ) %>% filter(hru==3)  %>% 
        as.data.frame() %>%  select(profit_ha)

ne_utility <- function(value, rho=0.005){
  
  utility<-  -exp(-rho*value)
  return(utility)
}

pow_utility <- function(value, rho=0.5){
  
  utility<- value^(1-rho)
  
  return(utility)
}

#cara utility for the hru in the whole period
ne_utility_whole <- function(profit_vector, rho=0.005){
  
  
  utility<- sapply(profit_vector, function(x){-exp(-rho*x)}) %>% sum()
  
  return(utility)
}

pow_utility_whole <- function(profit_vector, rho=0.5){
  
  utility <- sapply(profit_vector, function(x){x^(1-rho)}) %>%
             sum(na.rm = TRUE)

  
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

certainty_equivalent_pow <- function(profit_vector, rho=0.5){
  
  if(rho==0){
    ce <- profit_vector %>% unlist() %>% mean()
  }else{
  ce <- 
      (pow_utility_whole(profit_vector=profit_vector, rho = rho)/
        dim(profit_vector)[1])^(1/(1-rho)) 
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

risk_premium_pow <- function(profit_vector, rho=0.5){
  
  exp_val <- profit_vector %>% sum
  
  certainty_equivalent <-
    certainty_equivalent_pow(profit_vector=profit_vector,
                            rho=rho)
  
  risk_premium<- exp_val - certainty_equivalent
  
  return(risk_premium)   
  
} 


#Utility Aggregation----

hru_list <-
  profit_data %>% select(hru) %>% unique() %>% 
  unlist() %>% as.list()

ara_par <- seq(0.002, 0.009, by=0.001)

ce_ne <- matrix(nrow = hru_list %>% length(),
                ncol=8 ) 
ce_pow <- matrix(nrow = hru_list %>% length(),
                 ncol=8 ) 

rp_ne <- matrix(nrow = hru_list %>% length(),
                ncol=8)
rp_pow <- matrix(nrow = hru_list %>% length(),
                 ncol=8)

coso <-
profit_data %>% select(profit_ha,yr ) %>% filter(hru==3)  %>% 
  as.data.frame() %>%  select(profit_ha)

certainty_equivalent_ne(profit_vector = coso, rho=0.005)


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


ara_par <- seq(0.5, 4, by=0.5)

for(ara in ara_par){
  for (n_hru in hru_list) {
    
    ce_pow[match( n_hru, hru_list), match(ara, ara_par)] <-  
      certainty_equivalent_pow(profit_vector = 
                                profit_data %>% select(profit_ha,yr ) %>%
                                filter(hru==n_hru)  %>% 
                                as.data.frame() %>%  select(profit_ha),
                              rho=ara
      )
    
    
    
    rp_pow[match( n_hru, hru_list), match(ara, ara_par)] <-  
      risk_premium_pow(profit_vector = 
                        profit_data %>% select(profit_ha,yr ) %>% filter(hru==n_hru)  %>% 
                        as.data.frame() %>%  select(profit_ha),
                      rho=ara)
    print(ara)
    print(n_hru)
    
    print(c(ce_pow[match( n_hru, hru_list), match(ara, ara_par)],
            rp_pow[match( n_hru, hru_list), match(ara, ara_par)]
    ))  
    
  }
}

resultados_hru <-
cbind(ce_ne, ce_pow, rp_ne, rp_pow) %>% as.data.frame(); View(resultados_hru)

res_names<- c(paste0("ce_ne_", rac_par), paste0("ce_pow_", rac_par),
              paste0("rp_ne_", rac_par), paste0("rp_now_", rac_par)
              )

colnames(resultados_hru)<- res_names

basin_utility <-  


#Scenarios----
#NECESITO MANDAR UN MAIL A FRANCISCO PARA SABER QUE ESCENARIOS PONER
#O SEA, EN CADA ROTACION, NECESITO DEFINIR UN ESCENARIO DE RIEGO

