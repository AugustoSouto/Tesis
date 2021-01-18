###This Script contains all necessary scripts to run the whole###
###Thesis results###

#Run all the Results:
rm(list=ls())

setwd("C:/Users/Usuario/Desktop/Git/Tesis/San_Salvador")

list.files(pattern = ".R")

#1: Run the SWAT model----
 #this runs the model for dif scenarios and stroes its key environmental results
 #each scenario runs in approx 1 hr, so 10 scen run in 10 or 11 hs
source("SWATplus_run_model.R")

#2: Process SWAT results and Generate Economic variables----
 #this creates economic variables such as irrigation cost, irrigation quantities, etc
source("Data_Processing.R")

#3: Calculate profit volatility for each scenario----
 # This calculates the mean profit and profit's sd per year per hru
source("Profit_Volatility_Calc.R")
#4: Automatic Descriptive plots per Scenarios----
 #of variables such as N, P, Profit, irrigation, etc
source("Descriptive_Stats_SWAT.R")
#5: Econ model----
 #Run the mean CE per ha calculation per HRU
 #Exec time for 10 scen and 8 ARA params: 5 hs
source("Economic_Model_SS.R")
#6: Environmental Results Analysis 
 #build and analyze env stats datasets
source("Env_Res_Data_Analysis.R")

#7: Econ Results Analysis 
#build and analyze econ results in terms of CEs
source("Econ_Res_Analysis.R")

#8: Integrated Results Analysis 
#show cost of N and P reductions and related analysis
source("Integrated_Res_Analysis.R")


