rm(list = ls())
setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/Data_Simulaciones_Eco/")
docs<-
list.files(pattern = "Econ_Output_sc")

for (doc in docs) {
  load(doc)
  scen<- doc  %>% str_remove("Econ_Output_") %>% str_remove(".RData") 
  
  assign(paste0("irr_yr",scen), irr_yr %>% mutate(scen=scen))
}

irr_yr <-
rbind(irr_yrsc1,irr_yrsc2, irr_yrsc3,
      irr_yrsc4,irr_yrsc5, irr_yrsc6,
      irr_yrsc7,irr_yrsc8, irr_yrsc9)

saveRDS(irr_yr,"irr_yr.RDS")

