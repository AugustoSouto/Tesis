#CHANGE MANAGEMENT.SCH IRRIGATION ROUTINES#

#por ahora, la función implementa una rutina para todas#
#las rotaciones#
#FUNCION 1----
irr_management <- function(rutina="irr_str40_unlim"){

#rm(list = ls())

model_files<- "C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/"
model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"  
  
library(tidyverse)

management <- paste0(model_files, "management.sch")

doc <- readLines(management)

#Rutina de irrigacion
rutinas <- c("irr_str10_unlim",
             "irr_str15_unlim",
             "irr_str20_unlim",
             "irr_str25_unlim",
             "irr_str30_unlim",
             "irr_str35_unlim",
             "irr_str40_unlim",
             "irr_sw10_unlim",
             "irr_sw15_unlim",
             "irr_sw20_unlim",
             "irr_sw25_unlim",
             "irr_sw30_unlim",
             "irr_sw35_unlim",
             "irr_sw40_unlim")

#Lineas en donde aparecen las tablas de decision
lineas<- 
  str_detect(doc ,c("unlim"))

#Encontrar los numero de lineas, para reemplazar por las tablas que quiera
index <- which(lineas %in% "TRUE")

n_lineas <- doc[index] %>% length()

posiciones <- str_locate_all(doc[index],"unlim")

rut_len <- rutina %>% nchar()

#for (i in 1:n_lineas) {
#  doc[index][i] <-  paste0(c(rep("", posiciones[[i]][2]-rut_len+1),
#                             rutinas[match(rutina, rutinas)]) ,
#                             
#                             collapse = " ")
#}

for (i in 1:n_lineas) {
  doc[index][i] <-  paste0(c(rep("", 65-rut_len-1),
                             rutinas[match(rutina, rutinas)]) ,
                           
                           collapse = " ")
}

setwd(model_files)
#ahora sobreescribo el documento y listo
write(doc, "management.sch")
#shell.exec("management2.sch")

return(cat("Irrigation Rutine Setting:", rutina))
}


irr_management(rutina = "irr_sw20_unlim")


#FUNCION 2----

irr_management_all <- function(rot1="irr_str40_unlim",
                               rot2="irr_str40_unlim",
                               rot3="irr_str40_unlim",
                               rot4="irr_str40_unlim",
                               rot5="irr_str40_unlim",
                               rot6="irr_str40_unlim"){
  
  #rm(list = ls())
  
  library(tidyverse)
  management <- paste0(model_files, "management.sch")
#  management <- "C:/SWAT/Florencia/CCa3/Scenarios/Default/TxtInOut/management.sch"
  
  doc <- readLines(management)
  
  #Rutina de irrigacion
  rutinas <- c("irr_str10_unlim",
               "irr_str15_unlim",
               "irr_str20_unlim",
               "irr_str25_unlim",
               "irr_str30_unlim",
               "irr_str35_unlim",
               "irr_str40_unlim",
               "irr_sw10_unlim",
               "irr_sw15_unlim",
               "irr_sw20_unlim",
               "irr_sw25_unlim",
               "irr_sw30_unlim",
               "irr_sw35_unlim",
               "irr_sw40_unlim")  
  #Lineas en donde aparecen las tablas de decision
  lineas<- 
    str_detect(doc ,c("unlim"))
  
  #Encontrar los numero de lineas, para reemplazar por las tablas que quiera
  index <- which(lineas %in% "TRUE")
  
  n_lineas <- doc[index] %>% length()
  
  posiciones <- str_locate_all(doc[index],"unlim")

  rut_len1 <- rot1 %>% nchar()  
  rut_len2 <- rot2 %>% nchar()  
  rut_len3 <- rot3 %>% nchar()  
  rut_len4 <- rot4 %>% nchar()  
  rut_len5 <- rot5 %>% nchar()  
  rut_len6 <- rot6 %>% nchar()  
  

    doc[index][1] <-  paste0(c(rep("", 65-rut_len1-1),
                               rutinas[match(rot1, rutinas)]) ,
                             
                             collapse = " ")
    
    doc[index][2] <-  paste0(c(rep("", 65-rut_len2-1),
                               rutinas[match(rot2, rutinas)]) ,
                             
                             collapse = " ")
    
    doc[index][3] <-  paste0(c(rep("", 65-rut_len3-1),
                               rutinas[match(rot3, rutinas)]) ,
                             
                             collapse = " ")
    
    doc[index][4] <-  paste0(c(rep("", 65-rut_len4-1),
                               rutinas[match(rot4, rutinas)]) ,
                             
                             collapse = " ")
    
    doc[index][5] <-  paste0(c(rep("", 65-rut_len5-1),
                               rutinas[match(rot5, rutinas)]) ,
                             
                             collapse = " ")
    
    doc[index][6] <-  paste0(c(rep("", 65-rut_len6-1),
                               rutinas[match(rot6, rutinas)]) ,
                             
                             collapse = " ")
    

  
setwd(model_files)
  
  #ahora sobreescribo el documento y listo
  write(doc, "management.sch")
  #shell.exec("management2.sch")
  if(n_lineas!=6){
  return("The number of rotations is not 6, please, fix the function code")}else{
  
  return(cat("Irrigation Rutine Setting:", "rutina 1", rot1, 
             "rutina 2", rot2, "rutina 3", rot3, "rutina 4", rot4,
             "rutina 5", rot5, "rutina 6", rot6))
  }
      
}


