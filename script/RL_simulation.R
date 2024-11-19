
##### Load packages #####
library("purrr")
library("egg")
library("rjson")
library("loo")
library('nloptr')
library(cmdstanr)
library(posterior)
library(bayesplot)
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

load("data/data_for_choice.RData")
load("data/data_for_choice_blk2_4.RData")
load("data/data_for_choice_blk5.RData")
load("data/data_for_choice_blk6_8.RData")

# reinforcement learning function
source("script/subscript/RL.function.R")

##############################################################
########################### Q value for block 2~4 ##########################
##############################################################

# estimate the best parameters for block 6~8
optim_results_blk2_4 <- lapply(1:10, function(s) {
  optim_results_blk2_4 <- optim(par = c(0.8, 0.8), fn = log_likelihood_individual, 
                              method = "L-BFGS-B", 
                              lower = c(0, 0.1),  # lower bound, learning rate: 0~10
                              upper = c(10, 50),   # upper bound, softmax parameter: 0.1~50
                              block = c(2,3,4),    # fitting the data of block 2~4
                              data_ind = data_for_choice,
                              q1 =0.5,  # init q1 value
                              q2 =0.5,  # init q2 value
                              s = s)
})

# print the best parameters

params_estimates_blk2_4 <- sapply(optim_results_blk2_4, function(x) x$par)
print(params_estimates_blk2_4)

# use the best parameters to make Q value for block2~4
Q_values_for_each_subject_blk2_4 <- lapply(1:10, function(s) {
  best_params <- optim_results_blk2_4[[s]]$par  #  optim_results inclued the best parameters
  calculate_Q_values(best_params, data_for_choice_blk2_4,0.5,0.5, s)
})

final_data_blk2_4 <- data.frame(tri = 1:length(Q_values_for_each_subject_blk2_4[[1]]$Q1))
final_data_blk2_4$total_tri <-data_for_choice_blk2_4[[1]]$total_tri

# add list of Q1 and Q2 for each subject
for (s in 1:length(Q_values_for_each_subject_blk2_4)) {
  final_data_blk2_4[paste0("sub", s, "Q1")] <- Q_values_for_each_subject_blk2_4[[s]]$Q1
  final_data_blk2_4[paste0("sub", s, "Q2")] <- Q_values_for_each_subject_blk2_4[[s]]$Q2
}

# Calculate the mean value 
final_data_blk2_4$meanQ1 <- rowMeans(final_data_blk2_4[, grep("Q1", names(final_data_blk2_4))])
final_data_blk2_4$meanQ2 <- rowMeans(final_data_blk2_4[, grep("Q2", names(final_data_blk2_4))])
final_data_blk2_4$seQ1 <- apply(final_data_blk2_4[, grep("Q1", names(final_data_blk2_4))], 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
final_data_blk2_4$seQ2 <- apply(final_data_blk2_4[, grep("Q2", names(final_data_blk2_4))], 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
final_data_blk2_4$blk <- data_for_choice_blk2_4[[1]]$blk
for (s in 1:length(Q_values_for_each_subject_blk2_4)) {
  # calculate the difference of the Q value
  final_data_blk2_4[paste0("sub", s, "Q1-Q2")] <- final_data_blk2_4[paste0("sub", s, "Q1")] - final_data_blk2_4[paste0("sub", s, "Q2")]
}

# Plot the Q value for the block 2~4 
plot_Q_condition1.main = ggplot(data = final_data_blk2_4, aes(x =total_tri, y = meanQ1)) +
  geom_line(linewidth = 0.75,  color = "red") +
  geom_point(size = 2,  color = "red") +
  geom_ribbon(aes(ymin = pmax(meanQ1 - seQ1), ymax = pmin(meanQ1 + seQ1)),alpha = .2) +
  geom_line(aes(x =total_tri, y = meanQ2),linewidth = 0.75,  color = "blue") +
  geom_point(aes(x =total_tri, y = meanQ2),size = 2,  color = "blue") +
  geom_ribbon(aes(ymin = pmax(meanQ2 - seQ2), ymax = pmin(meanQ2 + seQ2)),alpha = .2) +
  xlab("Trial") +
  ylab("Q value")+ 
  ylim(0,80) +
  scale_x_continuous(name = "Trial",
                     breaks = seq(320, 950, by = 70),  
                     labels = seq(240, 870, by = 70)) + 
  scale_fill_manual(values = c("NULL" = "blue", "FF" = "red")) + 
  labs(fill = "Group") +  
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Q value in selection trial (condition %d)",3))
plot_Q_condition1.main
##############################################################
################# plot Q value for block 5 ###################
##############################################################

# estimate the best parameters
optim_results_blk5 <- lapply(1:10, function(s) {
  q1 <- tail(final_data_blk2_4[[paste0("sub", s, "Q1")]], 1)  # get the Q1 value in the last trial in blk 5 
  q2 <- tail(final_data_blk2_4[[paste0("sub", s, "Q2")]], 1)  # get the Q2 value in the last trial in blk 5 
  optim_results_blk5 <- optim(par = c(0.8, 0.8), fn = log_likelihood_individual, 
                         method = "L-BFGS-B", 
                         lower = c(0, 0.1),   # lower bound, learning rate: 0~10
                         upper = c(10, 50),   # upper bound, softmax parameter: 0.1~50
                         block = 5,           # fitting the data of block 5
                         data_ind = data_for_choice,
                         q1 =0.5,
                         q2 =0.5,
                         s = s)
})

# print the best parameters
params_estimates_blk5 <- sapply(optim_results_blk5, function(x) x$par)
print(params_estimates_blk5)

# use the best parameters to make Q value
Q_values_for_each_subject_blk5 <- lapply(1:10, function(s) {
  best_params <- optim_results_blk5[[s]]$par  #  optim_results inclued the best parameters
  q1 <- tail(final_data_blk2_4[[paste0("sub", s, "Q1")]], 1)  # get the Q1 value in the last trial in blk 5 
  q2 <- tail(final_data_blk2_4[[paste0("sub", s, "Q2")]], 1)  # get the Q2 value in the last trial in blk 5 
  calculate_Q_values(best_params, data_for_choice_blk5,q1,q2, s)
})

final_data_blk5 <- data.frame(tri = 1:length(Q_values_for_each_subject_blk5[[1]]$Q1))
final_data_blk5$total_tri <-data_for_choice_blk5[[1]]$total_tri

# add list of Q1 and Q2 for each subject

for (s in 1:length(Q_values_for_each_subject_blk5)) {
  final_data_blk5[paste0("sub", s, "Q1")] <- Q_values_for_each_subject_blk5[[s]]$Q1
  final_data_blk5[paste0("sub", s, "Q2")] <- Q_values_for_each_subject_blk5[[s]]$Q2
}

# Calculate the mean value
final_data_blk5$meanQ1 <- rowMeans(final_data_blk5[, grep("Q1", names(final_data_blk5))])
final_data_blk5$meanQ2 <- rowMeans(final_data_blk5[, grep("Q2", names(final_data_blk5))])
final_data_blk5$seQ1 <- apply(final_data_blk5[, grep("Q1", names(final_data_blk5))], 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
final_data_blk5$seQ2 <- apply(final_data_blk5[, grep("Q2", names(final_data_blk5))], 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
final_data_blk5$blk <- data_for_choice_blk5[[1]]$blk
for (s in 1:length(Q_values_for_each_subject_blk5)) {
  # calculate the difference of the Q value
  final_data_blk5[paste0("sub", s, "Q1-Q2")] <- final_data_blk5[paste0("sub", s, "Q1")] - final_data_blk5[paste0("sub", s, "Q2")]
}

#only plot the block5
final_data_blk5 = final_data_blk5 %>%
  dplyr::filter(blk == 5)

# plot the Q value block5 
plot_Q_condition2.main = ggplot(data = final_data_blk5, aes(x =total_tri, y = meanQ1)) +
  geom_line(linewidth = 0.75, color = "red") +
  geom_point(size = 2,  color = "red") +
  geom_ribbon(aes(ymin = pmax(meanQ1 - seQ1), ymax = pmin(meanQ1 + seQ1)),alpha = .2) +
  geom_line(aes(x =total_tri, y = meanQ2),linewidth = 0.75,  color = "blue") +
  geom_point(aes(x =total_tri, y = meanQ2),size = 2,  color = "blue") +
  geom_ribbon(aes(ymin = pmax(meanQ2 - seQ2), ymax = pmin(meanQ2 + seQ2)),alpha = .2) +
  xlab("Trial") +
  ylab("Q value")+ 
  scale_x_continuous(name = "Trial", 
                     breaks = seq(200, 320, by = 30),  
                     labels = seq(120, 240, by = 30)) + 
  scale_fill_manual(values = c("NULL" = "blue", "FF" = "red")) +
  labs(fill = "Group") +  
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Q value in selection trial (condition %d)",2))
plot_Q_condition2.main


##############################################################
############ Plot the Q value for block 6~8 ################## 
##############################################################

# estimate the best parameters for blk 6~8
optim_results_blk68 <- lapply(1:10, function(s) {
  q1 <- tail(final_data_blk5[[paste0("sub", s, "Q1")]], 1)  # get the Q1 value in the last trial in blk 5 
  q2 <- tail(final_data_blk5[[paste0("sub", s, "Q2")]], 1)  # get the Q2 value in the last trial in blk 5 
  optim_results_blk68 <-optim(par = c(0.8, 0.8), 
        fn = log_likelihood_individual, 
        method = "L-BFGS-B", 
        lower = c(0, 0.1),   # lower bound, learning rate: 0~10
        upper = c(10, 10),   # upper bound, softmax parameter: 0.1~50
        block = c(6,7,8),    # fitting the data of block 6~8
        q1 = q1,
        q2 = q2,
        data_ind = data_for_choice,
        s = s)
})

# print the best parameters
params_estimates_blk68 <- sapply(optim_results_blk68, function(x) x$par)
print(params_estimates_blk68)


# use the best parameters to make Q value
Q_values_for_each_subject_blk68 <- lapply(1:10, function(s) {
  best_params <- optim_results_blk68[[s]]$par  # best parameters for each subject
  q1 <- tail(final_data_blk5[[paste0("sub", s, "Q1")]], 1)  # get the Q1 value in the last trial in blk 5 
  q2 <- tail(final_data_blk5[[paste0("sub", s, "Q2")]], 1)  # get the Q2 value in the last trial in blk 5 
  calculate_Q_values(best_params, data_for_choice_blk6_8,q1,q2, s)
})

final_data_blk68 <- data.frame(tri = 1:length(Q_values_for_each_subject_blk68[[1]]$Q1))
final_data_blk68$total_tri <-data_for_choice_blk6_8[[1]]$total_tri

# add list of Q1 and Q2 for each subject
for (s in 1:length(Q_values_for_each_subject_blk68)) {
  final_data_blk68[paste0("sub", s, "Q1")] <- Q_values_for_each_subject_blk68[[s]]$Q1
  final_data_blk68[paste0("sub", s, "Q2")] <- Q_values_for_each_subject_blk68[[s]]$Q2
}

# Calculate the mean value
final_data_blk68$meanQ1 <- rowMeans(final_data_blk68[, grep("Q1", names(final_data_blk68))])
final_data_blk68$meanQ2 <- rowMeans(final_data_blk68[, grep("Q2", names(final_data_blk68))])
final_data_blk68$seQ1 <- apply(final_data_blk68[, grep("Q1", names(final_data_blk68))], 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
final_data_blk68$seQ2 <- apply(final_data_blk68[, grep("Q2", names(final_data_blk68))], 1, function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
final_data_blk68$blk <- data_for_choice_blk6_8[[1]]$blk
for (s in 1:length(Q_values_for_each_subject_blk68)) {
# calculate the difference of the Q value
  final_data_blk68[paste0("sub", s, "Q1-Q2")] <- final_data_blk68[paste0("sub", s, "Q1")] - final_data_blk68[paste0("sub", s, "Q2")]
}

# plot the Q value block6~8
plot_Q_condition3.main = ggplot(data = final_data_blk68, aes(x =total_tri, y = meanQ1)) +
  geom_line(linewidth = 0.75,  color = "red") +
  geom_point(size = 2,  color = "red") +
  geom_ribbon(aes(ymin = pmax(meanQ1 - seQ1), ymax = pmin(meanQ1 + seQ1)),alpha = .2) +
  geom_line(aes(x =total_tri, y = meanQ2),linewidth = 0.75,  color = "blue") +
  geom_point(aes(x =total_tri, y = meanQ2),size = 2,  color = "blue") +
  geom_ribbon(aes(ymin = pmax(meanQ2 - seQ2), ymax = pmin(meanQ2 + seQ2)),alpha = .2) +
  xlab("Trial") +
  ylab("Q value")+ 
  ylim(0,80) +
  scale_x_continuous(name = "Trial",
                     breaks = seq(320, 950, by = 70),  
                     labels = seq(240, 870, by = 70)) + 
  scale_fill_manual(values = c("NULL" = "blue", "FF" = "red")) + 
  labs(fill = "Group") +  
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Q value in selection trial (condition %d)",3))
plot_Q_condition3.main


