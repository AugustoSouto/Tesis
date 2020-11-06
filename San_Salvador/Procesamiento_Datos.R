#to do: get area
#       compute utility and profit

rm(list = ls())
library(tidyverse)

model_scripts<- "C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador/"

setwd(model_scripts)

scenarios <-
  list.files(model_scripts, pattern="RData"); scenarios

for(i in scenarios){
  
  print(i)  
  setwd(model_scripts)  
  load(i)
  
  