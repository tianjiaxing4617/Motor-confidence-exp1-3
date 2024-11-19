source("script/subscript/figure1.plotdata_summary.R")
source("script/RL_simulation.R")
#######################################################
##################### GLMM models #####################
#######################################################
# organize data in block 5
Q_difference_blk5_all <- data.frame(total_tri = integer(), subject = character(), value = integer(), stringsAsFactors = FALSE)
cols_to_merge <- grep("sub\\d+Q1-Q2", names(final_data_blk5), value = TRUE)
for (col in cols_to_merge) {
  temp <- data.frame(total_tri = final_data_blk5$total_tri,  subject = col, value = final_data_blk5[[col]])
  Q_difference_blk5_all <- rbind(Q_difference_blk5_all, temp)
} 
# organize data in block 6~8
Q_difference_blk68_all <- data.frame(total_tri = integer(), subject = character(), value = integer(), stringsAsFactors = FALSE)
cols_to_merge <- grep("sub\\d+Q1-Q2", names(final_data_blk68), value = TRUE)
for (col in cols_to_merge) {
  temp <- data.frame(total_tri = final_data_blk68$total_tri,  subject = col, value = final_data_blk68[[col]])
  Q_difference_blk68_all <- rbind(Q_difference_blk68_all, temp)
} 

# make the plot data
plot_gglm_blk5_all <- Q_difference_blk5_all%>% 
  mutate(Q_difference = value, f = data_plot_cc_blk5$f_pvel,  Pro = data_plot_s_blk5$select_right, sub = data_plot_cc_blk5$sub)

plot_gglm_blk68_all <- Q_difference_blk68_all%>% 
  mutate(Q_difference = value, f = data_plot_cc_blk68$f_pvel,  Pro= data_plot_s_blk68$select_right, sub = data_plot_cc_blk68$sub)
# Q value difference and force factor in glmm method in block 5 
model_Q_difference_blk5 <- glmer(Pro ~ Q_difference + (1 | sub), family = binomial, data = plot_gglm_blk5_all)
model_f_blk5 <- glmer(Pro ~ f + (1 | sub), family = binomial, data = plot_gglm_blk5_all)
# Q value difference and force factor in glmm method in block 6~8 
model_Q_difference_blk68 <- glmer(Pro ~ Q_difference + (1 | sub), family = binomial, data = plot_gglm_blk68_all)
model_f_blk68 <- glmer(Pro ~ f + (1 | sub), family = binomial, data = plot_gglm_blk68_all)
# summary the result
summary(model_Q_difference_blk5)
summary(model_f_blk5)
summary(model_Q_difference_blk68)
summary(model_f_blk68)

