# GLMM result data
source("script/subscript/Glmm summary.R")

#color map setting
colors <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

## Plot models ##
# Q value difference model glmm result plot in block5
p_Q_difference_blk5 <-plot_model(model_Q_difference_blk5, type = "pre", terms = c("Q_difference[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.3, colors = "#009E73", ci.lvl = 0.95)+
  labs(x = "Q_difference", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_Q_difference_blk5.pdf")
ggsave(filename = filename, plot = p_Q_difference_blk5, width = 15, height = 10, units = "cm")


# motor memory model glmm result plot in block5
p_f_blk5 <-plot_model(model_f_blk5, type = "pre", terms = c("f[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.3, colors ="#D55E00", ci.lvl = 0.95)+
  labs(x = "force", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_f_blk5.pdf")
ggsave(filename = filename, plot = p_f_blk5, width = 15, height = 10, units = "cm")

# Q value difference model glmm result plot in block6~8
p_Q_difference_blk68 <-plot_model(model_Q_difference_blk68, type = "pre", terms = c("Q_difference[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.3, colors = "#009E73", ci.lvl = 0.95)+
  labs(x = "Q_difference", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_Q_difference_blk68.pdf")
ggsave(filename = filename, plot = p_Q_difference_blk68, width = 15, height = 10, units = "cm")


# motor memory model glmm result plot in block6~8
p_f_blk68 <-plot_model(model_f_blk68, type = "pre", terms = c("f[all]"), show.data = TRUE, jitter = 0.05, dot.size = 0.3, colors ="#D55E00", ci.lvl = 0.95)+
  labs(x = "force", y = "Choice") +
  theme_classic()+
  theme(
    panel.background = element_blank(),
    panel.grid=element_blank())
filename <- paste0("figures/Logistic_regression_f_blk68.pdf")
ggsave(filename = filename, plot = p_f_blk68, width = 15, height = 10, units = "cm")



