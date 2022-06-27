#CHANGE MANAGEMENT.SCH IRRIGATION ROUTINES#

#por ahora, la funcion implementa una rutina para todas#
#las rotaciones#
#FUNCION 1----
irr_management <- function(rutina="irr_str40_unlim",
                           model_files = "C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/",
                           model_scripts = "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"  
                           ){

#rm(list = ls())

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

lineas2<- 
  str_detect(doc ,c("op_typ"))

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


#irr_management(rutina = "irr_sw20_unlim")


#FUNCION 2----

irr_management_all <- function(rot1="",
                               rot2="",
                               rot3="irr_str40_unlim",
                               rot4="irr_str50_unlim",
                               rot5="",
                               rot6="",
                               model_files = "C:/Users/Usuario/Desktop/SWATsept/SSalvador_LU2018_v0/Scenarios/Default/TxtInOut/",
                               model_scripts = "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/" ){
  
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
               "irr_str40_unlim",
               "irr_str50_unlim",
               "irr_str95_unlim",
               "irr_str80_unlim",
               "irr_sw20_unlim",
               "irr_sw25_unlim",
               "irr_sw30_unlim",
               "irr_sw35_unlim",
               "irr_sw40_unlim",
               "")  
  
  nombres_rot <- paste0(c("agrc_rot", 
                          "agrc2_rot",
                          "agrc3_rot",
                          "agrc4_rot",
                          "agri_rot",
                          "agrp_rot"),
                          collapse="|")
  
  col_nombres_rot <- c("agrc_rot", 
                       "agrc2_rot",
                       "agrc3_rot",
                       "agrc4_rot",
                       "agri_rot",
                       "agrp_rot")
  
  #Lineas en donde aparecen las tablas de decision
  lineas<- 
    str_detect(doc ,nombres_rot)

  #Linea en donde aparece la columna de op_type,   
  lineas_optyp <- 
    str_detect(doc ,c("op_typ"))
  
  lineas_numbops <- 
    str_detect(doc ,c("numb_ops"))
  
  lineas_numbauto <- 
    str_detect(doc ,c("numb_auto"))
  
  index_optyp <- which(lineas_optyp %in% "TRUE")
  
  posiciones_optyp <- str_locate_all(doc[index_optyp],"op_typ")
  
  index_numbops <- which(lineas_numbops %in% "TRUE")
  
  posiciones_numbops <- str_locate_all(doc[index_optyp],"numb_ops")
  
  index_numbauto <- which(lineas_numbauto %in% "TRUE")
  
  posiciones_numbauto <- str_locate_all(doc[index_numbauto],"numb_auto")
  
  ncol_blanco1_1 <-
    (posiciones_numbops[[1]][2]-2)-str_count(col_nombres_rot[1])
  
  ncol_blanco1_2 <-
    (posiciones_numbops[[1]][2]-2)-str_count(col_nombres_rot[2])
  ncol_blanco1_3 <-
    (posiciones_numbops[[1]][2]-2)-str_count(col_nombres_rot[3])
  ncol_blanco1_4 <-
    (posiciones_numbops[[1]][2]-2)-str_count(col_nombres_rot[4])
  ncol_blanco1_5 <-
    (posiciones_numbops[[1]][2]-2)-str_count(col_nombres_rot[5])
  ncol_blanco1_6 <-
    (posiciones_numbops[[1]][2]-2)-str_count(col_nombres_rot[6])

  ncol_blanco2 <-
    (posiciones_numbauto[[1]][2]-1)-(posiciones_numbops[[1]][2]-2)
    
  #Encontrar los numero de lineas, para reemplazar por las tablas que quiera
  #la rutina se especifica en el renglon siguiente al del nombre de la rotacion
  index <- which(lineas %in% "TRUE")
  
  
  if(rot1==""){doc[index][1]=
  
  doc[index][1]<- 
    paste0(c("agrc_rot",
           rep("", ncol_blanco1_1-1),
           "14",
           rep("", ncol_blanco2-3),
           "0"), collapse = " ")
  
  }else{
    doc[index][1]<- 
      paste0(c("agrc_rot",
               rep("", ncol_blanco1_1-1),
               "14",
               rep("", ncol_blanco2-3),
               "1"), collapse = " ")
  }
  
  if(rot2==""){doc[index][2]=
    
    doc[index][2]<- 
    paste0(c("agrc2_rot",
             rep("", ncol_blanco1_2-1),
             "15",
             rep("", ncol_blanco2-3),
             "0"), collapse = " ")
  
  }else{
    doc[index][2]<- 
      paste0(c("agrc2_rot",
               rep("", ncol_blanco1_2-1),
               "15",
               rep("", ncol_blanco2-3),
               "1"), collapse = " ")
  }
  
  if(rot3==""){doc[index][3]=
    
    doc[index][3]<- 
    paste0(c("agrc3_rot",
             rep("", ncol_blanco1_3-1),
             "13",
             rep("", ncol_blanco2-3),
             "0"), collapse = " ")
  
  }else{
    doc[index][3]<- 
      paste0(c("agrc3_rot",
               rep("", ncol_blanco1_3-1),
               "13",
               rep("", ncol_blanco2-3),
               "1"), collapse = " ")
  }
  
  if(rot4==""){doc[index][4]=
    
    doc[index][4]<- 
    paste0(c("agrc4_rot",
             rep("", ncol_blanco1_4-1),
             "21",
             rep("", ncol_blanco2-3),
             "0"), collapse = " ")
  
  
  
  }else{
    doc[index][4]<- 
      paste0(c("agrc4_rot",
               rep("", ncol_blanco1_4-1),
               "21",
               rep("", ncol_blanco2-3),
               "1"), collapse = " ")
  }
  
  if(rot5==""){doc[index][5]=
    
    doc[index][5]<- 
    paste0(c("agri_rot",
             rep("", ncol_blanco1_5-1),
             "19",
             rep("", ncol_blanco2-3),
             "0"), collapse = " ")
  
  
  
  
  }else{
    doc[index][5]<- 
      paste0(c("agri_rot",
               rep("", ncol_blanco1_5-1),
               "19",
               rep("", ncol_blanco2-3),
               "1"), collapse = " ")
  }
  
  if(rot6==""){doc[index][6]=
    
    doc[index][6]<- 
    paste0(c("agrp_rot",
             rep("", ncol_blanco1_6-1),
             "18",
             rep("", ncol_blanco2-3),
             "0"), collapse = " ")
  
  
  
  
  
  
  
  
  }else{
    doc[index][6]<- 
      paste0(c("agrp_rot",
               rep("", ncol_blanco1_6-1),
               "18",
               rep("", ncol_blanco2-3),
               "1"), collapse = " ")
  }
  
  index <- index+1
  
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


#irr_management_all(rot1="irr_str30_unlim",
#                   rot2="",
#                   rot3="",
#                   rot4="irr_str50_unlim",
#                   rot5="",
#                   rot6="")

