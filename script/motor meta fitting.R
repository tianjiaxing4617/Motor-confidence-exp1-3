
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
library(sjPlot)

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
mod_meta_motor <- list()   # model list
fit_meta_motor <- list()   # fitting list
Fitting_data_combined<- list()   # all fitting data together
Fitting_data_choice <- list()   # fitting data for choice data
Fitting_data_channel <- list()   # fitting data channel data

# fitting for each subject

for (m in 1:Subject_number){
  
  mod_meta_motor[[m]] <- list()
  fit_meta_motor[[m]] <- list()
  #fitting data organize
  Fitting_data_combined[[m]] <- rbind(Cleandata_blk5[[m]], Cleandata_blk6_8[[m]])
  
  Fitting_data_combined[[m]] <- Fitting_data_combined[[m]]%>%
    mutate(f_Bvalc = -f_pvel/pvel, totaltri = total_tri - 200) 
  
  Fitting_data_choice[[m]] <- Fitting_data_combined[[m]]%>%
    dplyr::filter(ws != "CC" & ws != "C1") 
  
  Fitting_data_channel[[m]] <- Fitting_data_combined[[m]]%>%
    dplyr::filter(ws == "CC"& f_Bvalc != "NA")
  
  file_meta <- file.path("model/fitting_meta.stan") #need to change the path to the right path
  
  dataset_meta<-list(y=Fitting_data_combined[[m]]$bvalc,                # PTB data
                           T=length(Fitting_data_combined[[m]]$bvalc),        # number of PTB data 
                           N=Subject_number,                             # Number of iterations
                           K=2,
                           t_choice_max=length(Fitting_data_choice[[m]]$bvalc),  #number of selection trial
                           trial=Fitting_data_choice[[m]]$totaltri,      #index of selection trial
                           select= Fitting_data_choice[[m]]$select_right,    #selection value
                           t_channel=length(Fitting_data_channel[[m]]$bvalc),   #total number of channel
                           trial_channel=Fitting_data_channel[[m]]$totaltri,     #index of channel trial
                           f=Fitting_data_channel[[m]]$f_Bvalc
  )
  save(dataset_meta, file = "data/dataset_meta.RData")
  mod_meta_motor[[m]] <- cmdstan_model(file_meta, pedantic = TRUE)
  mod_meta_motor[[m]]$check_syntax(pedantic = TRUE) 
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
  fit_meta_motor[[m]] <- mod_meta_motor[[m]]$variational(
    data = data_list_meta,
    seed = 123,
    output_samples = 10
  )
}

###########   clean the summary data of the fitting result   ###############

difference_entropy<-list();            #difference of entropy
toolA_state<-list();   # toolA state
toolB_state<-list();   # toolB state
uncertaintyA<-list();   # prediction uncertainty of toolA
uncertaintyB<-list();   # prediction uncertainty of toolB
pro<-list();           # probability of tool A
motor_memory<-list();  # motor state
meta_memory<-list();   # meta memory
kalgainA<-list();      # kalman gain of toolA
kalgainB<-list();      # kalman gain of toolB
metaA_state<-list();   # meta memory state toolA
metaB_state<-list();   # meta memory state toolB
summaryfitting_meta<-list(); # summary information

##############　summary data ########################

for (m in 1:Subject_number) {
  summaryfitting_meta[[m]]<-fit_meta_motor[[m]]$summary();
  tmp1_p1<-fit_meta_motor[[m]]$summary("P_meta1_prediction");
  uncertaintyA[[m]]<-tmp1_p1$mean;
  tmp1_p2<-fit_meta_motor[[m]]$summary("P_meta2_prediction");
  uncertaintyB[[m]]<-tmp1_p2$mean;
  tmp1_x1<-fit_meta_motor[[m]]$summary("u_motor1");
  toolA_state[[m]]<-tmp1_x1$mean;
  tmp1_x2<-fit_meta_motor[[m]]$summary("u_motor2");
  toolB_state[[m]]<-tmp1_x2$mean;
  tmp1_m1<-fit_meta_motor[[m]]$summary("u_meta1");
  metaA_state[[m]]<-tmp1_m1$mean;
  tmp1_m2<-fit_meta_motor[[m]]$summary("u_meta2");
  metaB_state[[m]]<-tmp1_m2$mean;
  tmp1_pro<-fit_meta_motor[[m]]$summary("pro");
  pro[[m]]<-tmp1_pro$mean;
  tmp1_k1<-fit_meta_motor[[m]]$summary("k1");
  kalgainA[[m]]<-tmp1_k1$mean;
  tmp1_k2<-fit_meta_motor[[m]]$summary("k2");
  kalgainB[[m]]<-tmp1_k2$mean;
  tmp1_motor<-fit_meta_motor[[m]]$summary("motor");
  motor_memory[[m]]<-tmp1_motor$mean;
  tmp1_meta<-fit_meta_motor[[m]]$summary("meta");
  meta_memory[[m]]<-tmp1_meta$mean;
  tmp1_En<-fit_meta_motor[[m]]$summary("En");
  difference_entropy[[m]]<-tmp1_En$mean;
}

tool_estimate_model_output <- list(
  toolA_state = toolA_state,  # state of toolA
  toolB_state = toolB_state,  # state of toolB
  motor_memory = motor_memory  # motor state
)
#saving the fitting result
save(tool_estimate_model_output, file = "data/tool_estimate_model_output.RData")

meta_model_output <- list(
  difference_entropy = difference_entropy,  #difference of entropy
  uncertaintyA = uncertaintyA,  # uncertainty of toolA
  uncertaintyB = uncertaintyB,  # uncertainty of toolB
  metaA_state = metaA_state,  # state of toolA
  metaB_state = metaB_state,  # state of toolB
  pro = pro,                 # probability of tool A
  kalgainA = kalgainA,          # kalman gain of toolA
  kalgainB = kalgainB,          # kalman gain of toolB
  meta_memory = meta_memory  # kalman gain
)

save(meta_model_output, file = "data/meta_model_output.RData")
load("data/meta_model_output.RData")

#####################  data organize for the plotting   ####################

all_data_En_model <- data.frame()
#all subject 
for (m in 1:Subject_number) {
  meta1 = meta_model_output$metaA_state[[m]][Fitting_data_choice[[m]]$totaltri]# meta memory1
  meta2 = meta_model_output$metaB_state[[m]][Fitting_data_choice[[m]]$totaltri]# meta memory2
  p1 = meta_model_output$uncertaintyA[[m]][Fitting_data_choice[[m]]$totaltri]# meta uncertainty1
  p2 = meta_model_output$uncertaintyB[[m]][Fitting_data_choice[[m]]$totaltri]# meta uncertainty2
  
  uncertainty1 <- 1 - pnorm(3, mean = meta1, sd = sqrt(p1))    # CDF of confidence1
  uncertainty2 <- pnorm(3, mean = meta2, sd = sqrt(p2))        # CDF of confidence2
  
  data_group_sum <- data.frame(total_tri = 1:length(Fitting_data_choice[[m]]$totaltri),
                               value = meta_model_output$difference_entropy[[m]][Fitting_data_choice[[m]]$totaltri],
                               motor = tool_estimate_model_output$motor_memory[[m]][Fitting_data_choice[[m]]$totaltri-1],
                               meta = meta_model_output$meta_memory[[m]][Fitting_data_choice[[m]]$totaltri],
                               En = meta_model_output$difference_entropy[[m]][Fitting_data_choice[[m]]$totaltri],
                               difference_meta = meta_model_output$metaA_state[[m]][Fitting_data_choice[[m]]$totaltri]-meta_model_output$metaB_state[[m]][Fitting_data_choice[[m]]$totaltri],
                               difference_motor = tool_estimate_model_output$toolA_state[[m]][Fitting_data_choice[[m]]$totaltri-1]-tool_estimate_model_output$toolB_state[[m]][Fitting_data_choice[[m]]$totaltri-1],
                               difference_uncertainty = sqrt(meta_model_output$uncertaintyA[[m]][Fitting_data_choice[[m]]$totaltri])-sqrt(meta_model_output$uncertaintyB[[m]][Fitting_data_choice[[m]]$totaltri]),
                               pro = meta_model_output$pro[[m]][Fitting_data_choice[[m]]$totaltri-1],
                               subject = factor(m),
                               meta_test= (p2/(p1+p2))*meta1+(p1/(p1+p2))*meta2,
                               meta_test2= meta1/sqrt(p1)+meta2/sqrt(p2),
                               confidence_test= uncertainty1-uncertainty2,
                               block = Fitting_data_choice[[m]]$blk,
                               choice = Fitting_data_choice[[m]]$select_right
  ) 
  
  all_data_En_model <- rbind(all_data_En_model, data_group_sum)
}
all_data_En_model_blk5 <- all_data_En_model%>%
  dplyr::filter(block == 5) 

all_data_En_model_blk6_8 <- all_data_En_model%>%
  dplyr::filter(block != 5) 

#####  Fitting results for GLMM models of difference factors in meta memory #########
model_motor <- glmer(choice ~ motor + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_meta <- glmer(choice ~ meta + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_difference_meta <- glmer(choice ~ difference_meta + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_difference_motor <- glmer(choice ~ difference_motor + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_difference_uncertainty <- glmer(choice ~ difference_uncertainty + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_pro <- glmer(choice ~ pro + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_En <- glmer(choice ~ En + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_meta_p <- glmer(choice ~ meta_test + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_meta_p2 <- glmer(choice ~ meta_test2 + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)
model_confidence <- glmer(choice ~ confidence_test + (1 | subject), family = binomial, data = all_data_En_model_blk6_8)


summary(model_motor)
summary(model_meta)
summary(model_difference_meta)
summary(model_difference_motor)
summary(model_difference_uncertainty)
summary(model_pro)
summary(model_En)
summary(model_meta_p)
