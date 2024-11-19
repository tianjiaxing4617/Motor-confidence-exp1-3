###################  reinforcement learning fitting function ###################
# function for softmax calculate
softmax <- function(Q1, Q2) {
  max_Q <- max(Q1, Q2)
  exp_Q1 <- exp(Q1 - max_Q)
  exp_Q2 <- exp(Q2 - max_Q)
  sum_exp_Q <- exp_Q1 + exp_Q2
  c(exp_Q1 / sum_exp_Q, exp_Q2 / sum_exp_Q)
}
epsilon <- 1e-10

# function for loglikelihood of individuals
log_likelihood_individual <- function(params, s, data_ind, q1, q2, block = NULL) {
  eta <- params[1]
  tau <- params[2]
  ll <- 0  # init loglikelihood
  
  if (!is.null(block)) {
    data_filtered <- data_ind[[s]] %>% dplyr::filter(blk %in% block)
  } else {
    data_filtered <- data_ind[[s]]
  }
  
  select <- data_filtered$select_left
  gain <- data_filtered$gain
  probabilities <- matrix(nrow = length(gain), ncol = 2)
  Q1 <- rep(0, length(gain))  # init Q1 value
  Q2 <- rep(0, length(gain))  # init Q2 value
  p <- rep(0, length(gain))  # init probability
  Q1[1]=q1
  Q2[1]=q2
  p_left <- rep(0.5, length(gain))
  p_right <- rep(0.5, length(gain))
  for (t in 2:length(gain)) {
    if(select[t]==1){
      Q1[t] <- Q1[t-1] + eta * (gain[t-1] - Q1[t-1])
      Q2[t] <- Q2[t-1] 
    }
    else{
      Q1[t] <- Q1[t-1] 
      Q2[t] <- Q2[t-1] + eta * (gain[t-1] - Q2[t-1])
    }
    
    probabilities[t,] <- softmax(Q1[t] / tau, Q2[t] / tau)
    p_left[t] <- max(min(probabilities[t,1], 1 - epsilon), epsilon)
    p_right[t] <- max(min(probabilities[t,2], 1 - epsilon), epsilon)
    
    ll <- ll + log(p_left[t]) * select[t] + log(1 - p_left[t]) * (1 - select[t])
  }
  
  return(-ll)  # reture loglikelihood
}

# function for calculating the Q values
calculate_Q_values <- function(best_params, data_ind, q1, q2, s) {
  eta <- best_params[1]
  tau <- best_params[2]
  
  
  select <- data_ind[[s]]$select_left
  gain <- data_ind[[s]]$gain
  Q1 <- rep(0.3, length(gain))
  Q2 <- rep(0.3, length(gain))
  Q1[1]=q1
  Q2[1]=q2
  for (t in 2:length(gain)) {
    if (select[t] == 1) {
      Q1[t] <- Q1[t - 1] + eta * (gain[t - 1] - Q1[t - 1])
      Q2[t] <- Q2[t - 1]
    } else {
      Q1[t] <- Q1[t - 1]
      Q2[t] <- Q2[t - 1] + eta * (gain[t - 1] - Q2[t - 1])
    }
  }
  
  return(list(Q1 = Q1, Q2 = Q2))
}



# calculate the loglikelihood for blocks
log_likelihood_for_block <- function(params_matrix, data_ind, q_values_df, block) {
  ll <- 0  
  
  for (s in 1:10) {
    params <- params_matrix[, s]  # get the parameter of the subject of s 
    
    q1 <- tail(q_values_df[[paste0("sub", s, "Q1")]], 1)  
    q2 <- tail(q_values_df[[paste0("sub", s, "Q2")]], 1)  
    
    ll <- ll + log_likelihood_individual(params, s, data_ind, q1, q2, block = block)
  }
  
  return(-ll)  
}

