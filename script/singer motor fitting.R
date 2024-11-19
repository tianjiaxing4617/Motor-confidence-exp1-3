
##### Load packages #####
library("purrr")
library("egg")
library("rjson")
library("loo")
library('nloptr')
library(cmdstanr)
library(posterior)
library(bayesplot)
# basic plots
library(ggplot2) 
library(dplyr)
library(patchwork)
library(tidyverse)
library(broom)
library("reshape2")
library("purrr")

library(lme4)
library(effects)
library(lmerTest)
#################################################################
################## motor meta learning  ################################
#################################################################
#################################################################
load("data/Cleandata_allblk.RData")
load("data/Cleandata_blk2_4.RData")
load("data/Cleandata_blk5.RData")
load("data/Cleandata_blk6_8.RData")

# fitting for each subject in task1

Subject_number <- length(all_subs)
mod_motor <- list()   # model list
fit_motor <- list()   # fitting list
Fitting_data_combined<- list()   # all fitting data together
Fitting_data_choice <- list()   # fitting data for choice data
Fitting_data_channel <- list()   # fitting data for channel data

# fitting for each subject

for (m in 1:Subject_number){
  
  mod_motor[[m]] <- list()
  fit_motor[[m]] <- list()
  #fitting data organize
  Fitting_data_combined[[m]] <- rbind(Cleandata_blk5[[m]], Cleandata_blk6_8[[m]])
  
  Fitting_data_combined[[m]] <- Fitting_data_combined[[m]]%>%
    mutate(f_Bvalc = -f_pvel/pvel, totaltri = total_tri - 200) 
  
  Fitting_data_choice[[m]] <- Fitting_data_combined[[m]]%>%
    dplyr::filter(ws != "CC" & ws != "C1") 
  
  Fitting_data_channel[[m]] <- Fitting_data_combined[[m]]%>%
    dplyr::filter(ws == "CC"& f_Bvalc != "NA")
  
  file_meta <- file.path("model/fitting_single_motor.stan") #need to change the path to the right path
  
  dataset_meta<-list(y=Fitting_data_combined[[m]]$bvalc,                # PTB data
                           T=length(Fitting_data_combined[[m]]$bvalc),        # number of PTB data 
                           t_channel=length(Fitting_data_channel[[m]]$bvalc),   #total number of channel
                           trial=Fitting_data_channel[[m]]$totaltri,     #index of channel trial
                           f=Fitting_data_channel[[m]]$f_Bvalc
  )
  save(dataset_meta, file = "data/dataset_singlemotor.RData")
  mod_motor[[m]] <- cmdstan_model(file_meta, pedantic = TRUE)
  mod_motor[[m]]$check_syntax(pedantic = TRUE) 
  data_list_meta <-dataset_meta
  # print(data_list_meta)
  # using MCMC to fitting 
  # fit_meta_motor[[m]]  <- mod_meta_motor[[m]]$sample(
  #   data = data_list_meta,
  #   seed = 123,
  #   chains = 4,
  #   # init = inits_meta,
  #   parallel_chains = 4,
  #   refresh = 500 # print update every 500 iters
  # )
  #using VB to fitting 
  fit_motor[[m]] <- mod_motor[[m]]$variational(
    data = data_list_meta,
    seed = 123,
    output_samples = 10
  )
}

###########   clean the summary data of the fitting result   ###############

singlemotor_memory<-list();  # motor state
summaryfitting_motor<-list(); # summary information

##############　summary data ########################

for (m in 1:Subject_number) {
  summaryfitting_motor[[m]]<-fit_meta_motor[[m]]$summary();
  tmp1_motor<-fit_motor[[m]]$summary("x");
  singlemotor_memory[[m]]<-tmp1_motor$mean;

}

singlemotor_model_output <- list(
  singlemotor_memory = singlemotor_memory  # kalman gain
)

#saving the fitting result
save(singlemotor_model_output, file = "data/singlemotor_model_output.RData")


#####################  data organize for the plotting   ####################

all_data_model <- data.frame()
#all subject 
for (m in 1:Subject_number) {
  data_group_sum <- data.frame(total_tri = 1:length(Fitting_data_choice[[m]]$totaltri),
                               motor = singlemotor_model_output$singlemotor_memory[[m]][Fitting_data_choice[[m]]$totaltri-1],
                               subject = factor(m),
                               block = Fitting_data_choice[[m]]$blk,
                               choice = Fitting_data_choice[[m]]$select_right
  ) 
  
  all_data_model <- rbind(all_data_model, data_group_sum)
}
all_data_model_blk5 <- all_data_model%>%
  dplyr::filter(block == 5) 

all_data_model_blk6_8 <- all_data_model%>%
  dplyr::filter(block != 5) 

