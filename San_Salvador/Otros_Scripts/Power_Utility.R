pow_utility <- function(value, rho=0.5){
  
  utility<- (1-rho)*value^(1-rho)
  
  return(utility)
}

pow_utility_whole <- function(profit_vector, rho=0.5){
  
  utility <- sapply(profit_vector, function(x){(1-rho)*x^(1-rho)}) %>%
    sum(na.rm = TRUE)
  
  
  return(utility)
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

ce_pow <- matrix(nrow = hru_list %>% length(),
                 ncol=8 ) 

rp_pow <- matrix(nrow = hru_list %>% length(),
                 ncol=8)

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


