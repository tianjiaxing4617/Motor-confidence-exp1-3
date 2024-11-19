source("script/RL_simulation.R")
source("script/subscript/figure1.plotdata_summary.R")
##############################################################
########################### BIC ##############################
##############################################################

# Reinforcement learning BIC for block 5
blocks <- 5
BIC_by_block5 <- sapply(blocks, function(b) {
  log_likelihood <- log_likelihood_for_block(params_estimates_blk5, data_ind = data_for_choice, q_values_df = final_data_blk2_4, block = b)
  n_data_points <- sum(sapply(1:10, function(s) nrow(data_for_choice[[s]] %>% dplyr::filter(blk == b))))
  n_params <- nrow(params_estimates_blk5)  
  
  BIC <- n_params * log(n_data_points) - 2 * log_likelihood
  return(BIC)
})

# Reinforcement BIC for block 6~8
blocks <- 6:8
BIC_by_block68 <- sapply(blocks, function(b) {
  log_likelihood <- log_likelihood_for_block(params_estimates_blk68, data_ind = data_for_choice, q_values_df = final_data_blk5, block = b)
  n_data_points <- sum(sapply(1:10, function(s) nrow(data_for_choice[[s]] %>% dplyr::filter(blk == b))))
  n_params <- nrow(params_estimates_blk68)  
  
  BIC <- n_params * log(n_data_points) - 2 * log_likelihood
  return(BIC)
})
##############################################################
########################### glmm #############################
##############################################################

# BIC of factors in glmm method block5

glmm_data_blk5 <- data_plot_cc_blk5 %>%
  mutate(select_right = data_plot_s_blk5$select_right) 
glmm_data_blk5$sub <- factor(glmm_data_blk5$sub )

model_blk5 <- glmer(select_right ~ f_pvel + (1 | sub), family = binomial, data = glmm_data_blk5)

summary(model_blk5)

BIC(model_blk5)

# BIC of factors in glmm method block6~8

glmm_data_blk68 <- data_plot_cc_blk68 %>%
  mutate(select_right = data_plot_s_blk68$select_right) 
glmm_data_blk68$sub <- factor(glmm_data_blk68$sub )

model_blk68 <- glmer(select_right ~ f_pvel + (1 | sub), family = binomial, data = glmm_data_blk68)

summary(model_blk68)

BIC(model_blk68)

