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

load("data/Cleandata_allblk.RData")
load("data/Cleandata_blk2_4.RData")
load("data/Cleandata_blk5.RData")
load("data/Cleandata_blk6_8.RData")

####################  organize the supplement plot data ########################

data_plot_blk2_4 = reduce(Cleandata_blk2_4, rbind) %>%
  dplyr::filter(ws != "CC" & ws != "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_blk2_4 = data_plot_blk2_4 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(gain, na.rm = T), val_median = median(gain, na.rm = T),
            val_sd = sd(gain, na.rm = T), val_se = sd(gain, na.rm = T)/sqrt(length(gain)))

data_plot_blk5 = reduce(Cleandata_blk5, rbind) %>%
  dplyr::filter(ws != "CC"& ws != "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_blk5 = data_plot_blk5 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(gain, na.rm = T), val_median = median(gain, na.rm = T),
            val_sd = sd(gain, na.rm = T), val_se = sd(gain, na.rm = T)/sqrt(length(gain)))

data_plot_blk6_8 = reduce(Cleandata_blk6_8, rbind) %>%
  dplyr::filter(ws != "CC"& ws != "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_blk6_8 = data_plot_blk6_8 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(gain, na.rm = T), val_median = median(gain, na.rm = T),
            val_sd = sd(gain, na.rm = T), val_se = sd(gain, na.rm = T)/sqrt(length(gain)))


sum_data_plot_r_blk2_4 = data_plot_blk2_4 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(select_left, na.rm = T), val_median = median(select_left, na.rm = T),
            val_sd = sd(select_left, na.rm = T), val_se = sd(select_left, na.rm = T)/sqrt(length(select_left)))

sum_data_plot_r_blk5 = data_plot_blk5 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(select_left, na.rm = T), val_median = median(select_left, na.rm = T),
            val_sd = sd(select_left, na.rm = T), val_se = sd(select_left, na.rm = T)/sqrt(length(select_left)))

sum_data_plot_r_blk6_8 = data_plot_blk6_8 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(select_left, na.rm = T), val_median = median(select_left, na.rm = T),
            val_sd = sd(select_left, na.rm = T), val_se = sd(select_left, na.rm = T)/sqrt(length(select_left)))


sum_data_plot_l = data_plot %>%
  dplyr::filter(ws == "L") %>%
  group_by(blk_tri,bvalc) %>%
  summarise(val_mean = mean(gain, na.rm = T), val_median = median(gain, na.rm = T),
            val_sd = sd(gain, na.rm = T), val_se = sd(gain, na.rm = T)/sqrt(length(gain)))

##################### Plot the score of the selection trial #######################
# for block 5 # 
plot_s2.main = ggplot(data = sum_data_plot_blk5, aes(x = total_tri, y = val_mean)) +
  geom_ribbon(aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd), fill = "gray", alpha = 0.5) +
  geom_point(size = 0.35,color = "blue") +
  geom_point(aes(x = 200, y = 0),color = "white") +
  geom_line(linewidth = 0.25)+
  scale_x_continuous(name = "Trial", 
                     breaks = seq(200, 320, by = 30),  
                     labels = seq(120, 240, by = 30)) + 
  xlab("Trial") +
  ylab("Score")+ 
  ylim(0,105) +
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Avg score in Choice trial(condition %d)",2))

plot_s2.main

# for block 6~8 # 
plot_s3.main = ggplot(data = sum_data_plot_blk6_8, aes(x = total_tri, y = val_mean)) +
  geom_ribbon(aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd), fill = "gray", alpha = 0.5) +
  geom_point(size = 0.35,color = "blue") +
  geom_point(aes(x = 320, y = 0),color = "white") +
  geom_line(linewidth = 0.25)+
  scale_x_continuous(name = "Trial",
                     breaks = seq(320, 950, by = 70),  
                     labels = seq(240, 870, by = 70)) + 
  xlab("Trial") +
  ylab("Score")+ 
  ylim(0,105) +
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Avg score in Choice trial (condition %d)",3))

plot_s3.main

# for block 2~4 # 
plot_s1.main = ggplot(data = sum_data_plot_blk2_4, aes(x = total_tri, y = val_mean)) +
  geom_ribbon(aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd), fill = "gray", alpha = 0.5) +
  geom_point(size = 0.35,color = "blue") +
  geom_line(linewidth = 0.25)+
  scale_x_continuous(name = "Trial",
                     breaks = seq(80,200, by = 10),  
                     labels = seq(0,120, by = 10)) + 
  xlab("Trial") +
  ylab("Score")+ 
  ylim(0,105) +
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Avg score in Choice trial (condition %d)",1))

plot_s1.main



############### Plot the probability of selection A for the selection trial #######################

sum_data_plot_r_blk5 <- sum_data_plot_r_blk5 %>%
  mutate(
    ymin = pmax(val_mean - val_se, 0),
    ymax = pmin(val_mean + val_se, 1)
  )

# for block 5 # 
plot_r2.main = ggplot(data = sum_data_plot_r_blk5, aes(x = total_tri, y = val_mean)) +
  geom_ribbon(aes(x = total_tri, ymin =ymin, ymax = ymax), fill = "gray", alpha = 0.5) +
  geom_point(size = 0.35,color = "blue") +
  geom_point(aes(x = 200, y = 0),color = "white") +
  geom_line(linewidth = 0.25)+
  scale_x_continuous(name = "Trial", 
                     breaks = seq(200, 320, by = 30),  
                     labels = seq(120, 240, by = 30)) + 
  xlab("Trial") +
  ylab("Probability of tool A")+ 
  ylim(0,1) +
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25) 
  )+
  ggtitle(sprintf("Grp Avg select tool A in Choice trial (condition %d)",2))

plot_r2.main

# for block 6~8 #
sum_data_plot_r_blk6_8 <- sum_data_plot_r_blk6_8 %>%
  mutate(
    ymin = pmax(val_mean - val_se, 0),
    ymax = pmin(val_mean + val_se, 1)
  )
plot_r3.main = ggplot(data = sum_data_plot_r_blk6_8, aes(x = total_tri, y = val_mean)) +
  geom_ribbon(aes(x = total_tri, ymin = ymin, ymax = ymax), fill = "gray", alpha = 0.5) +
  geom_point(size = 0.35,color = "blue") +
  geom_point(aes(x = 320, y = 0),color = "white") +
  geom_line(linewidth = 0.25)+
  scale_x_continuous(name = "Trial",
                     breaks = seq(320, 950, by = 70),  
                     labels = seq(240, 870, by = 70)) + 
  xlab("Trial") +
  ylab("Probability of tool A")+ 
  ylim(0,1) +
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Avg select tool A in Choice trial (condition %d)",3))

plot_r3.main

# for block 2~4 #
sum_data_plot_r_blk2_4 <- sum_data_plot_r_blk2_4 %>%
  mutate(
    ymin = pmax(val_mean - val_se, 0),
    ymax = pmin(val_mean + val_se, 1)
  )
plot_r1.main = ggplot(data = sum_data_plot_r_blk2_4, aes(x = total_tri, y = val_mean)) +
  geom_ribbon(aes(x = total_tri, ymin =ymin, ymax =ymax), fill = "gray", alpha = 0.5) +
  geom_point(size = 0.35,color = "blue") +
  geom_line(linewidth = 0.25)+
  scale_x_continuous(name = "Trial",
                     breaks = seq(80,200, by = 10),  
                     labels = seq(0,120, by = 10)) + 
  xlab("Trial") +
  ylab("Probability of tool A")+ 
  ylim(0,1) +
  theme_classic()+
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 4))+  
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid=element_blank())+
  theme(
    axis.line.x = element_line(linewidth = 0.25), 
    axis.line.y = element_line(linewidth = 0.25)  
  )+
  ggtitle(sprintf("Grp Avg select tool A in Choice trial (condition %d)",1))

plot_r1.main

###################################################################################
############################# Plot the error  #####################################

sum_data_plot_E_blk2_4 = data_plot_blk2_4 %>%
  dplyr::filter(ws != "C1") %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val*val, na.rm = T), val_median = median(val*val, na.rm = T),
            val_sd = sd(val*val, na.rm = T), val_se = sd(val*val, na.rm = T)/sqrt(length(val)))%>%
  ungroup()

sum_data_plot_E_blk5 = data_plot_blk5 %>%
  dplyr::filter(ws != "C1") %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val*val, na.rm = T), val_median = median(val*val, na.rm = T),
            val_sd = sd(val*val, na.rm = T), val_se = sd(val*val, na.rm = T)/sqrt(length(val)))%>%
  ungroup()

sum_data_plot_E_blk6_8 = data_plot_blk6_8 %>%
  dplyr::filter(ws != "C1") %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val*val, na.rm = T), val_median = median(val*val, na.rm = T),
            val_sd = sd(val*val, na.rm = T), val_se = sd(val*val, na.rm = T)/sqrt(length(val)))%>%
  ungroup()

# for block 2~4 #
plot_errorline_condition1.main = ggplot(data = sum_data_plot_E_blk2_4, aes(x =total_tri, y = val_mean)) +
  geom_line(linewidth = 0.25,  color = "blue") +
  geom_point(size = 0.35,  color = "blue") +
  geom_ribbon(aes(ymin = val_mean - val_se, ymax = val_mean + val_se), alpha = .2) +
  geom_point(aes(x = 80, y = 0),color = "white") +
  xlab("Trial") +
  ylab("Trajectory Error square(mm^2)")+ 
  ylim(0,530) +
  scale_x_continuous(name = "Trial",
                     breaks = seq(80,200, by = 10),  
                     labels = seq(0,120, by = 10)) + 
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
  ggtitle(sprintf("Grp Avg Trajectory Error square(mm^2) in selection trial (condition %d)",1))
plot_errorline_condition1.main

# for block 5 #
plot_errorline_condition2.main = ggplot(data = sum_data_plot_E_blk5, aes(x =total_tri, y = val_mean)) +
  geom_line(linewidth = 0.25,  color = "blue") +
  geom_point(size = 0.35,  color = "blue") +
  geom_ribbon(aes(ymin = val_mean - val_se, ymax = val_mean + val_se), alpha = .2) +
  geom_point(aes(x = 200, y = 0),color = "white") +
  xlab("Trial") +
  ylab("Trajectory Error square(mm^2)")+ 
  ylim(0,530) +
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
  ggtitle(sprintf("Grp Avg Trajectory Error square(mm^2) in selection trial (condition %d)",2))
plot_errorline_condition2.main

# for block 6~8 #
plot_errorline_condition3.main = ggplot(data = sum_data_plot_E_blk6_8, aes(x =total_tri, y = val_mean)) +
  geom_line(linewidth = 0.25,  color = "blue") +
  geom_point(size = 0.35,  color = "blue") +
  geom_ribbon(aes(ymin = val_mean - val_se, ymax = val_mean + val_se), alpha = .2) +
  geom_point(aes(x = 320, y = 0),color = "white") +
  xlab("Trial") +
  ylab("Trajectory Error square(mm^2)")+ 
  ylim(0,530) +
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
  ggtitle(sprintf("Grp Avg Trajectory Error square(mm^2) in selection trial (condition %d)",3))
plot_errorline_condition3.main


###################################################################################
############################ Plot the Q value  ####################################
source("script/RL_simulation.R")

# for block 2~4 #

plot_Q_condition1.main = ggplot(data = final_data_blk2_4, aes(x =total_tri, y = meanQ1)) +
  geom_line(linewidth = 0.75,  color = "red") +
  geom_point(size = 2,  color = "red") +
  geom_ribbon(aes(ymin = pmax(meanQ1 - seQ1), ymax = pmin(meanQ1 + seQ1)),alpha = .2) +
  geom_line(aes(x =total_tri, y = meanQ2),linewidth = 0.75,  color = "blue") +
  geom_point(aes(x =total_tri, y = meanQ2),size = 2,  color = "blue") +
  geom_ribbon(aes(ymin = pmax(meanQ2 - seQ2), ymax = pmin(meanQ2 + seQ2)),alpha = .2) +
  xlab("Trial") +
  ylab("Q value")+ 
  ylim(0,75) +
  scale_x_continuous(name = "Trial",
                     breaks = seq(80,200, by = 10),  
                     labels = seq(0,120, by = 10)) + 
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
  ggtitle(sprintf("Grp Q value in selection trial (condition %d)",1))
plot_Q_condition1.main

# for block 5 #

plot_Q_condition2.main = ggplot(data = final_data_blk5, aes(x =total_tri, y = meanQ1)) +
  geom_line(linewidth = 0.75, color = "red") +
  geom_point(size = 2,  color = "red") +
  geom_ribbon(aes(ymin = pmax(meanQ1 - seQ1), ymax = pmin(meanQ1 + seQ1)),alpha = .2) +
  geom_line(aes(x =total_tri, y = meanQ2),linewidth = 0.75,  color = "blue") +
  geom_point(aes(x =total_tri, y = meanQ2),size = 2,  color = "blue") +
  geom_ribbon(aes(ymin = pmax(meanQ2 - seQ2), ymax = pmin(meanQ2 + seQ2)),alpha = .2) +
  xlab("Trial") +
  ylab("Q value")+ 
  ylim(0,75) +
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

# for block 6~8 #

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




############### Smith motor memory plot  #######################
smith_model <- data.frame()
#all subject 
for (m in 1:Subject_number) {
  data_group_sum <- data.frame(total_tri = 1:length(Fitting_data_combined[[m]]$bvalc),
                               motor = smithmotor_model_output$smithmotor_memory[[m]],
                               xfmotor = smithmotor_model_output$smithmotor_xfmemory[[m]],
                               xsmotor = smithmotor_model_output$smithmotor_xsmemory[[m]],
                               force = smithmotor_model_output$smithmotor_xsmemory[[m]],
                               subject = factor(m),
                               block = Fitting_data_combined[[m]]$blk
  ) 
  
  smith_model <- rbind(smith_model, data_group_sum)
}
smith_model_blk5 <- smith_model%>%
  dplyr::filter(block == 5) 

smith_model_blk6_8 <- smith_model%>%
  dplyr::filter(block != 5) 


smith_model_long <- smith_model_blk6_8 %>%
  pivot_longer(
    cols = c("motor", "xfmotor", "xsmotor"),
    names_to = "memory_type",
    values_to = "memory_value"
  )


smith_model_summary <- smith_model_long %>%
  group_by(total_tri, memory_type) %>%
  summarise(
    val_mean = mean(memory_value),
    val_sd = sd(memory_value),
    .groups = 'drop'
  )

channel_data_list <- list()
Fitting_smith_channel<- list()

for (m in 1:Subject_number) {
  Fitting_smith_channel[[m]] <- Fitting_data_channel[[m]]%>%
    dplyr::filter(blk != 5)
  channel_data_m <- data.frame(
    total_tri = Fitting_smith_channel[[m]]$totaltri,
    memory_value = Fitting_smith_channel[[m]]$f_Bvalc,
    subject = m
  )
  
  channel_data_list[[m]] <- channel_data_m
}

channel_data_all <- do.call(rbind, channel_data_list)

channel_data_summary <- channel_data_all %>%
  group_by(total_tri) %>%
  summarise(
    val_mean = mean(memory_value, na.rm = TRUE),
    val_sd = sd(memory_value, na.rm = TRUE)
  )


plot_smith.main <- ggplot() +
  geom_line(data = smith_model_summary, aes(x = total_tri, y = val_mean, color = memory_type), size = 0.5) +
  geom_point(data = smith_model_summary, aes(x = total_tri, y = val_mean, color = memory_type), size = 0.75) +
  geom_ribbon(data = smith_model_summary, aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd, fill = memory_type), alpha = 0.2, linetype = 0) +
  geom_line(data = channel_data_summary, aes(x = total_tri, y = val_mean), color = 'black', size = 0.5) +
  geom_point(data = channel_data_summary, aes(x = total_tri, y = val_mean), color = 'black', size = 0.75) +
  geom_ribbon(data = channel_data_summary, aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd), fill = 'grey', alpha = 0.2, linetype = 0) +
  
  scale_x_continuous(
    name = "Trial",
    breaks = seq(320, 950, by = 70),
    labels = seq(240, 870, by = 70)
  ) +
  xlab("Trial") +
  ylab("Memory Value") +
  ylim(-5, 20) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 6),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    legend.position = "right",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    axis.line.y = element_line(linewidth = 0.25)
  ) +
  ggtitle(sprintf("Grp Avg Memory in Choice trial (condition %d)", 3))

print(plot_smith.main)
  
############### single motor memory plot  #######################
single_model <- data.frame()
#all subject 
for (m in 1:Subject_number) {
  data_group_sum <- data.frame(total_tri = 1:length(Fitting_data_combined[[m]]$bvalc),
                               motor = singlemotor_model_output$singlemotor_memory[[m]],
                               
                               subject = factor(m),
                               block = Fitting_data_combined[[m]]$blk
  ) 
  
  single_model <- rbind(single_model, data_group_sum)
}
single_model_blk5 <- single_model%>%
  dplyr::filter(block == 5) 

single_model_blk6_8 <- single_model%>%
  dplyr::filter(block != 5) 


single_model_long <- single_model_blk6_8 %>%
  pivot_longer(
    cols = c("motor"),
    names_to = "memory_type",
    values_to = "memory_value"
  )


single_model_summary <- single_model_long %>%
  group_by(total_tri, memory_type) %>%
  summarise(
    val_mean = mean(memory_value),
    val_sd = sd(memory_value),
    .groups = 'drop'
  )

channel_data_list <- list()
Fitting_smith_channel<- list()

for (m in 1:Subject_number) {
  Fitting_smith_channel[[m]] <- Fitting_data_channel[[m]]%>%
    dplyr::filter(blk != 5)
  channel_data_m <- data.frame(
    total_tri = Fitting_smith_channel[[m]]$totaltri,
    memory_value = Fitting_smith_channel[[m]]$f_Bvalc,
    subject = m
  )
  
  channel_data_list[[m]] <- channel_data_m
}

channel_data_all <- do.call(rbind, channel_data_list)

channel_data_summary <- channel_data_all %>%
  group_by(total_tri) %>%
  summarise(
    val_mean = mean(memory_value, na.rm = TRUE),
    val_sd = sd(memory_value, na.rm = TRUE)
  )


plot_single.main <- ggplot() +
  geom_line(data = single_model_summary, aes(x = total_tri, y = val_mean, color = memory_type), size = 0.5) +
  geom_point(data = single_model_summary, aes(x = total_tri, y = val_mean, color = memory_type), size = 0.75) +
  geom_ribbon(data = single_model_summary, aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd, fill = memory_type), alpha = 0.2, linetype = 0) +
  geom_line(data = channel_data_summary, aes(x = total_tri, y = val_mean), color = 'black', size = 0.5) +
  geom_point(data = channel_data_summary, aes(x = total_tri, y = val_mean), color = 'black', size = 0.75) +
  geom_ribbon(data = channel_data_summary, aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd), fill = 'grey', alpha = 0.2, linetype = 0) +
  
  scale_x_continuous(
    name = "Trial",
    breaks = seq(320, 950, by = 70),
    labels = seq(240, 870, by = 70)
  ) +
  xlab("Trial") +
  ylab("Memory Value") +
  ylim(-5, 20) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 6),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    legend.position = "right",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    axis.line.y = element_line(linewidth = 0.25)
  ) +
  ggtitle(sprintf("Grp Avg Memory in Choice trial (condition %d)", 3))

print(plot_single.main)

############### two motor memory plot  #######################
tool_model <- data.frame()
#all subject 
for (m in 1:Subject_number) {
  data_group_sum <- data.frame(total_tri = 1:length(Fitting_data_combined[[m]]$bvalc),
                               motor = tool_estimate_model_output$motor_memory[[m]],
                               motorA = tool_estimate_model_output$toolA_state[[m]],
                               motorB = tool_estimate_model_output$toolB_state[[m]],
                               subject = factor(m),
                               block = Fitting_data_combined[[m]]$blk
  ) 
  
  tool_model <- rbind(tool_model, data_group_sum)
}
tool_model_blk5 <- tool_model%>%
  dplyr::filter(block == 5) 

tool_model_blk6_8 <- tool_model%>%
  dplyr::filter(block != 5) 


tool_model_long <- tool_model_blk6_8 %>%
  pivot_longer(
    cols = c("motor","motorA","motorB"),
    names_to = "memory_type",
    values_to = "memory_value"
  )


tool_model_summary <- tool_model_long %>%
  group_by(total_tri, memory_type) %>%
  summarise(
    val_mean = mean(memory_value),
    val_sd = sd(memory_value),
    .groups = 'drop'
  )



plot_tool.main <- ggplot() +
  geom_line(data = tool_model_summary, aes(x = total_tri, y = val_mean, color = memory_type), size = 0.5) +
  geom_point(data = tool_model_summary, aes(x = total_tri, y = val_mean, color = memory_type), size = 0.75) +
  geom_ribbon(data = tool_model_summary, aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd, fill = memory_type), alpha = 0.2, linetype = 0) +
  geom_line(data = channel_data_summary, aes(x = total_tri, y = val_mean), color = 'black', size = 0.5) +
  geom_point(data = channel_data_summary, aes(x = total_tri, y = val_mean), color = 'black', size = 0.75) +
  geom_ribbon(data = channel_data_summary, aes(x = total_tri, ymin = val_mean - val_sd, ymax = val_mean + val_sd), fill = 'grey', alpha = 0.2, linetype = 0) +
  
  scale_x_continuous(
    name = "Trial",
    breaks = seq(320, 950, by = 70),
    labels = seq(240, 870, by = 70)
  ) +
  xlab("Trial") +
  ylab("Memory Value") +
  ylim(-5, 20) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 6),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    legend.position = "right",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    axis.line.y = element_line(linewidth = 0.25)
  ) +
  ggtitle(sprintf("Grp Avg Memory in Choice trial (condition %d)", 3))

print(plot_tool.main)
###################################################################################
###################################################################################
#########################  plot supplement figure  ########################
b<-plot_grid(probability_part2,plot_r2.main,plot_s2.main,plot_errorline_condition2.main,plot_Q_condition2.main, ncol = 1, align = "v", axis = "tblr")
c<-plot_grid(probability_part3,plot_r3.main,plot_s3.main,plot_errorline_condition3.main,plot_Q_condition3.main, ncol = 1, align = "v", axis = "tblr")
a<-plot_grid(probability_part1,plot_r1.main,plot_s1.main,plot_errorline_condition1.main,plot_Q_condition1.main, ncol = 1, align = "v", axis = "tblr",rel_heights = c(1, 1, 1, 1))

# save the plot figures

ggsave("figures/combined_plot1_supplement.pdf", plot = a, width = 15, height = 60, units = "cm")
ggsave("figures/combined_plot2_supplement.pdf", plot = b, width = 10, height = 60, units = "cm")
ggsave("figures/combined_plot3_supplement.pdf", plot = c, width = 17, height = 60, units = "cm")


