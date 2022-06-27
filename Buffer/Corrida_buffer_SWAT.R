#######SCRIPT MODELO INTEGRADO####### 
rm(list = ls())

#hacer funcion

library(SWATplusR)
library(SWATdata)
library(tidyverse)

#load("C:/SWAT/Data_vars.RData")

demo_path <- "C:/SWAT/SWATPlus"

proyecto_path <-"C:/SWAT/Drive/SWAT_mensual/Backup/"

#INTRODUCE PARAMETERS----

mypar = read.table(paste0("C:/Users/Usuario/Google Drive/SWAT-SubSantaLucia/",
                          "6-SWATCUP/Level5/20191210_Sim_monthlylanduse2015TambosSUFI2.Sufi2.SwatCup/",
                          "SUFI2.OUT/beh_pars.txt"))[,-1]
dim(mypar)

par_set = tibble("r1_CN2::CN2.mgt|change = pctchg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,1]*100,
                 "r1_ESCO::ESCO.hru|change = pctchg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,2]*100,
                 "r1_SOL_AWC::SOL_AWC.sol|change = pctchg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,3]*100,
                 "r1_ALPHA_BF::ALPHA_BF.gw|change = absval |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,4],
                 "r1_GWQMN::GWQMN.gw|change = abschg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,5],
                 "r1_GW_DELAY::GW_DELAY.gw|change = pctchg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,6]*100,
                 "r1_REVAPMN::REVAPMN.gw|change = abschg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,7],
                 "r1_GW_REVAP::GW_REVAP.gw|change = absval |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,8],
                 "r1_OV_N::OV_N.hru|change = absval |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,9],
                 "r1_SLSUBBSN::SLSUBBSN.hru|change = pctchg |
sub == c(3,4,5,9,11,12,13,25,27,31,32,38,39,40,41)" =
                   mypar[,10]*100,
                 "r2_CN2::CN2.mgt|change = pctchg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,11]*100,
                 "r2_ESCO::ESCO.hru|change = pctchg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,12]*100,
                 "r2_SOL_AWC::SOL_AWC.sol|change = pctchg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,13]*100,
                 "r2_ALPHA_BF::ALPHA_BF.gw|change = absval |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,14],
                 "r2_GWQMN::GWQMN.gw|change = abschg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,15],
                 "r2_GW_DELAY::GW_DELAY.gw|change = pctchg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,16]*100,
                 "r2_REVAPMN::REVAPMN.gw|change = abschg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,17],
                 "r2_GW_REVAP::GW_REVAP.gw|change = absval |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,18],
                 "r2_OV_N::OV_N.hru|change = absval |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,19],
                 "r2_SLSUBBSN::SLSUBBSN.hru|change = pctchg |
sub == c(2,6,7,8,10,14,15,17,19,20,24,26,28,29,30,33,34,35,36)" =
                   mypar[,20]*100,
                 "r3_CN2::CN2.mgt|change = pctchg |
                 sub == c(1,16,18,22,23)" =
                   mypar[,21]*100,
                 "r3_ESCO::ESCO.hru|change = pctchg |
sub == c(1,16,18,22,23)" =
                   mypar[,22]*100,
                 "r3_SOL_AWC::SOL_AWC.sol|change = pctchg |
sub == c(1,16,18,22,23)" =
                   mypar[,23]*100,
                 "r3_ALPHA_BF::ALPHA_BF.gw|change = absval |
sub == c(1,16,18,22,23)" =
                   mypar[,24],
                 "r3_GWQMN::GWQMN.gw|change = abschg |
sub == c(1,16,18,22,23)" =
                   mypar[,25],
                 "r3_GW_DELAY::GW_DELAY.gw|change = pctchg |
sub == c(1,16,18,22,23)" =
                   mypar[,30]*100,
                 "r3_REVAPMN::REVAPMN.gw|change = abschg |
sub == c(1,16,18,22,23)" =
                   mypar[,26],
                 "r3_GW_REVAP::GW_REVAP.gw|change = absval |
sub == c(1, 16, 18, 22,23)" =
                   mypar[,27],
                 "r3_OV_N::OV_N.hru|change = absval |
sub == c(1,16,18,22,23)" =
                   mypar[,28],
                 "r3_SLSUBBSN::SLSUBBSN.hru|change = pctchg |
sub == c(1,16,18,22,23)" =
                   mypar[,29]*100)

#how to define a buffer strip for a specific subbasin and land use
#in this case, there's an abs change of 50 m, in subbasin 1,2,3 only in hrus with agrp use

par_bound = c("FILTERW.mgt|change = absval |sub == c(1,2,3) | luse %in% c('AGRP') " = 50)


#for example:
#simple change, 50 m buffer for all hru
buffer<-matrix(30, nrow = 500) 

par_buffer<- tibble(
"FILTERW.mgt|change = absval " = buffer[,1]
) %>% cbind(par_set) 

#RUN SIMULATION----

#first, we run the model and extract non-numerical variables such as
#land use and hru area, then, we run multiple simulations on the model 
#based on different parameters, and extract the output for relevant
#numerical variables. Them, we will try to summarize the multiple runs
#using somethinf like a mean and 

data_hru <- run_swat2012(project_path = "C:/SWAT/Drive/SWAT_mensual/Backup/",
                         start_date = "2006-01-01",
                         end_date = "2015-12-31",
                         years_skip = 4,
                         output<- list(land_use = define_output(file = "hru",
                                                                variable = c("LULC"),
                                                                unit = 1:865),
                                       area = define_output(file = "hru",
                                                            variable = c("AREAkm2"),
                                                            unit = 1:865 )),
                         output_interval = "m",
                         keep_folder = FALSE,
                         n_thread = 4,
                         save_path = "C:/SWAT/Drive/SWAT_mensual/out_mensual/land_use_y_area/")


simulacion_buffer = run_swat2012(project_path = "C:/SWAT/Drive/SWAT_mensual/Backup/",
                          start_date = "2006-01-01",
                          end_date = "2015-12-31",
                          years_skip = 4,
                          output = list(flow = define_output(file = "rch", 
                                                             variable = c("FLOW_OUT"),
                                                             unit = 1:41),
                                        nitro = define_output(file = "rch",
                                                              variable = c("TOT_Nkg"),
                                                              unit = 1:41),
                                        
                                        yield = define_output(file = "hru",
                                                              variable = c("YLDt/ha"),
                                                              unit = 1:865),
                                        phosphorus = define_output(file = "rch",
                                                                   variable = c("TOT_Pkg"),
                                                                   unit = 1:41)
                                        # area = define_output(file = "hru",
                                        #                      variable = c("AREAkm2"),
                                        #                      unit = 1:865 )
                          ),
                          output_interval = "m",
                          parameter = par_buffer[1:3,],
                          keep_folder = FALSE,
                          n_thread = 4,
                          save_path = "C:/SWAT/Drive/SWAT_mensual/out_mensual/Buffer/con_buffer")



save.image("C:/SWAT/Corridas_Buffer_3sim.RData")
