# calculate the BIC 
source("script/subscript/BIC_Q_force.R")
############################## BIC ########################################
# combine BIC in block6~8 
BIC68<-BIC_by_block68[1]+BIC_by_block68[2]+BIC_by_block68[3]
# BIC data
bic_values <- c(
  BIC_by_block5,
  BIC(model_blk5),
  BIC68,
  BIC(model_blk68)
)
# BIC tag
model_names <- c(
  "RL_block5",
  "Channel force block5",
  "RL_block6~8",
  "Channel force block6~8"
)
# setting color map
model_colors <- c("#009E73", "#D55E00", "#CC79A7", "orange")

# setting BIC data frame
bic_data <- data.frame(
  Model = model_names,
  BIC = bic_values
)

#plot the BIC in block5
bic_data$Model <- factor(bic_data$Model, levels = model_names)

bic_data_part1 <- bic_data[1:2, ]
bic_data_part2 <- bic_data[3:4, ]

# The max of the BIC 
y_max <- max(bic_data$BIC)

BIC_plot1 <- ggplot(bic_data_part1, aes(x = Model, y = BIC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(BIC, 2)), vjust = -0.3, color = "black", size = 3.5) +
  scale_fill_manual(values = model_colors[1:2]) +
  labs(title = "BIC Values for RL and Channel force (block5)",
       x = "Model",
       y = "BIC") +
  ylim(0, y_max) +  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#plot the BIC in block6~8
BIC_plot2 <- ggplot(bic_data_part2, aes(x = Model, y = BIC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(BIC, 2)), vjust = -0.3, color = "black", size = 3.5) +
  scale_fill_manual(values = model_colors[3:4]) +
  labs(title = "BIC Values for RL and Channel force (block6~8)",
       x = "Model",
       y = "BIC") +
  ylim(0, y_max) +  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = "figures/BIC_plot1.pdf", plot = BIC_plot1, width = 15, height = 10, units = "cm")
ggsave(filename = "figures/BIC_plot2.pdf", plot = BIC_plot2, width = 15, height = 10, units = "cm")

grid.arrange(BIC_plot1, BIC_plot2, ncol = 2)


