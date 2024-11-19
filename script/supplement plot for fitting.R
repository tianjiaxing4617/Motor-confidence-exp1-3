library("dplyr")
library("purrr")
library("ggplot2")
library(sjPlot)
library("dplyr")
library("purrr")
library("ggplot2")
library(sjPlot)
library(devtools)
library(ggnewscale)
library(tidyr) 
library(cowplot)
library(gtable)

###################################################################################
###################################################################################
##########  plot supplement figure for single motor memory model  #################
###################################################################################
#####  Fitting results for GLMM models and figures #########
singlemodel_motor <- glmer(choice ~ motor + (1 | subject), family = binomial, data = all_data_model_blk6_8)

summary(singlemodel_motor)

#plot the glmm result
colors <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")
## Plot models
# memory motor model
p_tmp_singlemodel <-plot_model(singlemodel_motor, type = "pre", terms = c("motor[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.1, colors = "#009E73", ci.lvl = 0.95)+
  labs(x = "motor", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())

filename <- paste0("figures/Logistic_regression_singlemodelmemory.pdf")
ggsave(filename = filename, plot = p_tmp_singlemodel, width = 15, height = 10, units = "cm")


##################  Plot BIC result  ##################

bic_values <- c(
  BIC(model_motor),
  BIC(model_pro),
  BIC(model_confidence),
  BIC(singlemodel_motor)
)

model_names <- c(
  "model_motor",
  "model_pro",
  "model_confidence",
  "model_singlemotor"
)

model_colors <- c(
  "model_motor" = "#009E73",
  "model_pro" = "#D55E00",
  "model_confidence" = "#CC79A7",
  "model_singlemotor" = "orange"
)

bic_data <- data.frame(
  Model = model_names,
  BIC = bic_values
)

bic_data <- bic_data[order(bic_data$BIC),]

####### BIC plot #######
BIC_plot<-ggplot(bic_data, aes(x = reorder(Model, BIC), y = BIC, fill = Model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = model_colors) +
  labs(title = "BIC Values for Different Models",
       x = "Model",
       y = "BIC") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
filename <- paste0("figures/single memory model BIC.pdf")
ggsave(filename = filename, plot = BIC_plot, width = 15, height = 10, units = "cm")


colors <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

###################################################################################
###################################################################################
###############  plot supplement figure for meta memory model  ####################
###################################################################################
## Plot models
# memory motor model
p_tmp_motor <-plot_model(model_motor, type = "pre", terms = c("motor[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.1, colors = "#009E73", ci.lvl = 0.95)+
  labs(x = "motor", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_motormemory.pdf")
ggsave(filename = filename, plot = p_tmp_motor, width = 15, height = 10, units = "cm")


# Probability model
p_tmp_pro <-plot_model(model_pro, type = "pre", terms = c("pro[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.1, colors = "#D55E00", ci.lvl = 0.95)+
  labs(x = "pro", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_predicted_probability.pdf")
ggsave(filename = filename, plot = p_tmp_pro, width = 15, height = 10, units = "cm")


# confidence model
p_tmp_confidence <-plot_model(model_confidence, type = "pre", terms = c("confidence_test[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.1, colors = "#CC79A7", ci.lvl = 0.95)+
  labs(x = "confidence", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_confidence_difference.pdf")
ggsave(filename = filename, plot = p_tmp_confidence, width = 15, height = 10, units = "cm")


##################  BIC result  ##################

bic_values <- c(
  BIC(model_motor),
  BIC(model_pro),
  BIC(model_confidence)
)

model_names <- c(
  "model_motor",
  "model_pro",
  "model_confidence"
)

model_colors <- c(
  "model_motor" = "#009E73",
  "model_pro" = "#D55E00",
  "model_confidence" = "#CC79A7"
)

bic_data <- data.frame(
  Model = model_names,
  BIC = bic_values
)

bic_data <- bic_data[order(bic_data$BIC),]

####### BIC plot #######
BIC_plot<-ggplot(bic_data, aes(x = reorder(Model, BIC), y = BIC, fill = Model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = model_colors) +
  labs(title = "BIC Values for Different Models",
       x = "Model",
       y = "BIC") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
filename <- paste0("figures/BIC_meta_memory.pdf")
ggsave(filename = filename, plot = BIC_plot, width = 15, height = 10, units = "cm")

###################################################################################
###################################################################################
##########  plot supplement figure for Q difference model  #################
###################################################################################
#####  Fitting results for GLMM models and figures #########
Q_difference_blk68 <- Q_difference_blk68_all
Q_difference_blk68$subject <- as.numeric(sub("sub(\\d+).*", "\\1", Q_difference_blk68$subject))
Q_difference_blk68$choice <- all_data_model_blk6_8$choice
Q_difference_model <- glmer(choice ~ value + (1 | subject), family = binomial, data = Q_difference_blk68)

summary(Q_difference_model)

#plot the glmm result
colors <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")
## Plot models
# Q difference model
Q_difference <-plot_model(Q_difference_model, type = "pre", terms = c("value[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.1, colors = "#F0E442", ci.lvl = 0.95)+
  labs(x = "Q difference", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())

filename <- paste0("figures/Logistic_regression_Q_difference.pdf")
ggsave(filename = filename, plot = Q_difference, width = 15, height = 10, units = "cm")


##################  Plot BIC result  ##################

bic_values <- c(
  BIC(model_motor),
  BIC(model_pro),
  BIC(model_confidence),
  BIC(Q_difference_model),
  BIC(singlemodel_motor)
)

model_names <- c(
  "model_motor",
  "model_pro",
  "model_confidence",
  "model_Q_difference",
  "model_singlemotor"
)

model_colors <- c(
  "model_motor" = "#009E73",
  "model_pro" = "#D55E00",
  "model_confidence" = "#CC79A7",
  "model_Q_difference" = "#F0E442",
  "model_singlemotor" = "orange"
)

bic_data <- data.frame(
  Model = model_names,
  BIC = bic_values
)

bic_data <- bic_data[order(bic_data$BIC),]

####### BIC plot #######
BIC_plot<-ggplot(bic_data, aes(x = reorder(Model, BIC), y = BIC, fill = Model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = model_colors) +
  labs(title = "BIC Values for Different Models",
       x = "Model",
       y = "BIC") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
filename <- paste0("figures/Model BIC.pdf")
ggsave(filename = filename, plot = BIC_plot, width = 15, height = 10, units = "cm")


colors <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

