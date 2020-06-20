area_buffer_ha<-function(hru=1){
  
  hru_sub <- hru_info %>% filter(HRU_ID1==hru) %>%
    select(Subbasin) 
  
  zona_hru <- zonas %>% filter(zonas$Subbasin==as.numeric(hru_sub)) %>%
    select(Zona)
  
  zona_hru <- if(zona_hru=="A")zona_hru=1 else
    if(zona_hru=="B")zona_hru=2 else
      if(zona_hru=="C")zona_hru=3    
  
  ancho <- anchos[[n_esc]][zona_hru] %>% as.numeric()
  
  dir2<-"C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/12-RN_out/"
  
  n<-hru
  #n = 700
  hru_info[n,]
  
  fname <- paste0("C:/SWAT/Rafael/SL_20200401/",
                  hru_info[n,"HRUGIS"],
                  ".hru")
  
  readLines(fname)[1:5]
  
  #largo del hru
  SLSUBBSN <- as.numeric(substr(readLines(fname)[3],1,16)) 
  
  HRUarea <- hru_info[n, "HRU_AREA_km2"];HRUarea
  #area=largo*ancho-->ancho=area/largo
  #1000000 es para pasar de km2 a m2
  HRUW <- 1000000*HRUarea/SLSUBBSN; HRUW
  HRUW
  
  #diferentes anchos de buffer en metros
  #FILTERW = c(5,10,20,40,80)
  FILTERW <- ancho
  
  #el area del buffer es el largo del buff por el ancho del hru
  #eventualmente, si el ancho de banda es muy grande, 
  #puede pasar que tu area de buffer sea mayor a la del HRU
  FILTER_area <- HRUW*FILTERW; FILTER_area
  FILTER_area_ha <- FILTER_area/10000; FILTER_area_ha
  return(FILTER_area_ha)
  
}