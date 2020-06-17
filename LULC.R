rm(list = ls())

setwd("C:/Users/Usuario/Desktop/Git/Tesis/")

hru_in <- readRDS("hru_info.RDS")

LULC_2006_2015 <- readRDS("LULC_2006_2015.RDS")

land_use<- LULC_2006_2015 %>% tibble::column_to_rownames("date") %>% as_tibble()



seq <- TraMineR::seqdef(land_use) 
#below, graph the land use sequence for each hru
TraMineR::seqIplot(seq, border = NA, with.legend = "right")

#number of crop rotations
land_use %>% as.list() %>% unique() %>% length()

#sequence of states for each crop rotation
land_use %>% as.list() %>% unique()

#see the duplicated columns using data.table 
#library(data.table)
#DT<-data.table(data_hru[,land_use])
#DT %>% t() %>% duplicated() 

#detect number of rotations and crop periods
#data frame with all 7 rotations

df <-land_use %>% as.list() %>% unique() %>% as.data.frame()

#set colnames of unique land uses
#rotation 1: cattle *
#rotation 2: eucaliptus forestry *
#rotation 3: pure agricultural rotation
#rotation 4: mixed agricultural-cattle rotation
#rotation 5: mixed agricultural-cattle rotation
#rotation 6: dairy rotation
#rotation 7: native forestry

# *cattle, eucaliptus and native forestry land uses are not rotations
# only use the name rotation so as to put the same name to all sequences



colnames(df)<-c("rotacion_1","rotacion_2","rotacion_3",
                "rotacion_4","rotacion_5","rotacion_6",
                "rotacion_7", "rotacion_8")

data_rot_hru<- land_use

df[,1] %in% t(land_use)

for (i in 1:8) {
  assign(paste0("rotacion_",i) ,apply(land_use , 2 ,
                                      function(a) {identical(a,as.vector(get(paste0("df"))[,i]))} ) %>% as.vector()  
         
  )
}

#generated data.frame with the 7 rotations sequence in the basin
rotaciones<- cbind(rotacion_1, rotacion_2, rotacion_3,rotacion_4,
                   rotacion_5, rotacion_6, rotacion_7, rotacion_8) 

#check that hrus only have 1 true value per rotation 
apply(rotaciones,1,sum) %>% all()
#which rotation is set in each HRU
apply(rotaciones, 1, function(x) {names(which(x == T))})

#generate a string that helps to identify the hru rotation
rotacion_hru <- apply(rotaciones, 1, function(x) {names(which(x == T))}); rotacion_hru

seq_df <- TraMineR::seqdef(df); seq_df 

xlsx::write.xlsx(seq_df, "Secuencia_Usos.xlsx")

#visualize the rotations in a graph
graph_plot <- TraMineR::seqIplot(seq_df, border = NA, with.legend = "right")


#soil use summary (by rotation)
for (i in colnames(df)) {
  print(i)  
  get("df")[,i] %>% table() %>% print()
  rle(as.vector(get(paste0("df"))[,i])) %>% print()
}


for (i in colnames(df)) {
  assign(paste0(i,"_gen"),   
         
         rle(as.vector(get(paste0("df"))[,i])) )
}

#number of hrus with each type of rotation

for (i in colnames(df)) {
  print(paste("Hrus con",i))
  apply(data_hru[,land_use] ,2 ,
        function(a) 
        {identical(a,as.vector(get(paste0("df"))[,i]))}
  ) %>%
    sum %>%
    print()
  
}

#rotations 1,2,3 and 5 are the most widespread within the basin



#see how many months a given land use last
#variable seque_rotacion_ gives the cumulative months of a given land use 
#change_rotation gives the index which indicates the month in which a new land use is started in the rotation

for (i in colnames(df)) {
  assign(  
    paste0("seque_",i),
    sequence(rle(as.vector(get(paste0("df"))[,i]))$lengths) 
  )
  assign(
    paste0("change_",i),
    which(get(paste0("seque_",i))==1)
  )
  
}

changes<- list(change_rotacion_1,change_rotacion_2,change_rotacion_3,
               change_rotacion_4,change_rotacion_5,change_rotacion_6,
               change_rotacion_7); changes

#days to change between land uses in each rotation
lapply(changes, diff)

#semesters to change between land uses in each rotation
lapply(changes, diff) %>% lapply(function(valor) {valor/6})
#years to change between land uses in each rotation
lapply(changes, diff) %>% lapply(function(valor) {valor/12})

