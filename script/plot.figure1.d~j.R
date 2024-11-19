
##### Load packages #####
library("dplyr")
library("purrr")
library("ggplot2")
library(sjPlot)
library(devtools)
library(ggnewscale)
library(tidyr) 
library(cowplot)
library(gtable)


setwd("C:/EXP1_new")
source("script/subscript/figure1.plotdata_summary.R") # plot data summary
source("script/subscript/plot.function.R") # plot function
##### Plotting #####
######################################################################
######################################################################
########################   figure1 d,e   ###############################
########################   Error plot  ###############################
######################################################################
#Error in block2~4 orange:channel, gray: selection trial green:practice trial
plot1.main <- plot_trajectory_error(sum_data_plot_blk24, sum_data_plot_s_blk24, sum_data_plot_vf_blk24, 
                                    x_breaks = seq(80, 200, by = 10), 
                                    x_labels = seq(0, 120, by = 10), 
                                    x_point = 80, 
                                    condition_number = 1)
#Error in block5 orange:channel, gray: selection trial green:practice trial
plot2.main <- plot_trajectory_error(sum_data_plot_blk5, sum_data_plot_s_blk5, sum_data_plot_vf_blk5, 
                                    x_breaks = seq(200, 320, by = 30), 
                                    x_labels = seq(120, 240, by = 30), 
                                    x_point = 200, 
                                    condition_number = 2)
#Error in block6~8 orange:channel, gray: selection trial green:practice trial
plot3.main <- plot_trajectory_error(sum_data_plot_blk68, sum_data_plot_s_blk68, sum_data_plot_vf_blk68, 
                                    x_breaks = seq(320, 950, by = 70), 
                                    x_labels = seq(240, 870, by = 70), 
                                    x_point = 320, 
                                    condition_number = 3)

plot1.main
plot2.main
plot3.main
##########################################################################
############################ map color plot ##############################
############################# figure1 j,k #################################

# Plot for Q value in block5
Q_blk5 <- plot_raster(
  data_long = data_long_blk5,
  data_summary = NULL,
  x_breaks = seq(187, 307, by = 30),
  x_labels = seq(120, 240, by = 30),
  fill_var = "Q_d",
  fill_limits = c(-90, 90),
  fill_label = "Probability of tool A",
  y_label = "Participant",
  x_label = "Trial"
)

########################  Q blk68     ##########################
# Plot for Q value in block6~8
Q_blk68 <- plot_raster(
  data_long = data_long_blk68,
  data_summary = NULL,
  x_breaks = seq(292, 922, by = 70),
  x_labels = seq(240, 870, by = 70),
  fill_var = "Q_d",
  fill_limits = c(-30, 90),
  fill_label = "Probability of tool A",
  y_label = "Participant",
  x_label = "Selection trial"
)
Q_blk68

########################### f block5 ########################
##########################  figure1 h,i #####################
# Plot for motor memory in block5
force_blk5 <- plot_raster(
  data_long = data_long_blk5,
  data_summary = f_blk5_long,
  x_breaks = seq(187, 307, by = 30),
  x_labels = seq(120, 240, by = 30),
  fill_var = "f",
  fill_limits = c(-5, 3),
  fill_label = "Probability of tool A",
  y_label = "Participant",
  x_label = "Trial"
)
force_blk5
########################### f block68 ########################
# Plot for motor memory in block6~8
force_blk6_8 <- plot_raster(
  data_long = data_long_blk68,
  data_summary = f_blk68_long,
  x_breaks = seq(292, 922, by = 70),
  x_labels = seq(240, 870, by = 70),
  fill_var = "f",
  fill_limits = c(-5, 3),
  fill_label = "Probability of tool A",
  y_label = "Participant",
  x_label = "Selection trial"
)

force_blk6_8
##############################################################################
####################  rec plot for probability  ##############################
#######################   figure1 f,g    #####################################
##############################################################################
# Plot for the selection probability in block2~4
probability_part1 <- plot_raster_with_annotations(
  data_long = data_long1,
  x_breaks = seq(84, 204, by = 10),
  x_labels = seq(0, 120, by = 10),
  fill_var = "value",
  fill_colors = c("cyan", "magenta"),
  rect_intervals = c(80, 20, 5, 204),  # Start, interval width, and width of first rectangle
  rect_colors = c("white", "white"),
  y_label = "Participant",
  x_label = "Selection trial"
)

# Plot for the selection probability in block5
probability_part2 <- plot_raster_with_annotations(
  data_long = data_long_blk5,  # Replace with appropriate data for p2
  x_breaks = seq(187, 307, by = 30),
  x_labels = seq(120, 240, by = 30),
  fill_var = "value",
  fill_colors = c("cyan", "magenta"),
  rect_intervals = c(187, 60, 30, 247),  # Adjust intervals as needed
  rect_colors = c("red", "blue"),
  y_label = "Participant",
  x_label = "Selection"
)

# Plot for the selection probability in block6~8
probability_part3<- plot_raster_with_annotations(
  data_long = data_long_blk68,  # Replace with appropriate data for p3
  x_breaks = seq(292, 922, by = 70),
  x_labels = seq(240, 870, by = 70),
  fill_var = "value",
  fill_colors = c("cyan", "magenta"),
  rect_intervals = c(292, 140, 70, 922),  # Adjust intervals as needed
  rect_colors = c("red", "blue"),
  y_label = "Participant",
  x_label = "Selection trial"
)


################### combined plot for d~k ########################
# combine the plots together
blk5<-plot_grid(probability_part2,plot2.main,force_blk5,Q_blk5,  ncol = 1, align = "v", axis = "tblr",rel_heights = c(1, 1.5, 1, 1))
blk6_8<-plot_grid(probability_part3,plot3.main,force_blk6_8,Q_blk68, ncol = 1, align = "v", axis = "tblr",rel_heights = c(1, 1.5, 1, 1))

# save the figures
ggsave("figures/combined_rec_plot_blk5.pdf", plot = blk5, width = 10, height = 25, units = "cm")
ggsave("figures/combined_rec_plot_blk6_8.pdf", plot = blk6_8, width = 17, height = 25, units = "cm")




###################################################################################
#########################   figure1.f bar plot   ##################################
#####################  probability in certain trial ###############################
###################################################################################
################################################################################
################################################################################
# Plot for Selection bar plot in block 5
plot_bar1_selection.main <- create_bar_plot(
  data = sum_data_plot_bar1,
  y_var = "val_mean",
  fill_var = "color_group",
  x_labels = custom_labels1,
  width = 0.6,
  alpha = 0.7,
  title = "Grp Avg select right in Choice",
  condition_number = 2
)

plot_bar1_selection.main


# Plot for Selection bar plot in block 6~8
plot_bar2_selection.main <- create_bar_plot(
  data = sum_data_plot_bar2,
  y_var = "val_mean",
  fill_var = "color_group",
  x_labels = custom_labels2,
  width = 0.9,
  alpha = 0.7,
  title = "Grp Avg select right in Choice",
  condition_number = 3
)

plot_bar2_selection.main

################################################################################
################################ Q #############################################
################################################################################

# Plot for Q value bar plot in block 5
plot_bar1_Q.main <- create_bar_plot(
  data = sum_data_plot_bar1,
  y_var = "Q_mean",
  fill_var = "color_group",
  x_labels = custom_labels1,
  width = 0.6,
  alpha = 0.7,
  title = "Grp Avg select right in Choice",
  condition_number = 2
)

plot_bar1_Q.main

# Plot for Q value bar plot in block 6~8
plot_bar2_Q.main <- create_bar_plot(
  data = sum_data_plot_bar2,
  y_var = "Q_mean",
  fill_var = "color_group",
  x_labels = custom_labels2,
  width = 0.9,
  alpha = 0.7,
  title = "Grp Avg select right in Choice",
  condition_number = 3
)

plot_bar2_Q.main
###########################################################################
############################ force bar ####################################
###########################################################################

# Plot for motor memory bar plot in block 5
plot_bar1_f.main <- create_bar_plot(
  data = sum_data_plot_vf_bar1,
  y_var = "val_mean",
  fill_var = "color_group",
  x_labels = f_labels1,
  width = 0.6,
  alpha = 0.7,
  title = "Grp Avg select right in Choice",
  condition_number = 2
)

plot_bar1_f.main

# Plot for motor memory bar plot in block 6~8
plot_bar2_f.main <- create_bar_plot(
  data = sum_data_plot_vf_bar2,
  y_var = "val_mean",
  fill_var = "color_group",
  x_labels = f_labels2,
  width = 0.9,
  alpha = 0.7,
  title = "Grp Avg select right in Choice",
  condition_number = 3
)

plot_bar2_f.main

###################################################################
###################################################################
################# combine the Bar plots and save ##################
# selection bar
plot_probability_bar<-plot_grid(plot_bar1_selection.main,plot_bar2_selection.main, ncol = 2, align = "h")
ggsave("figures/probability select right barplot.pdf", plot = plot_probability_bar, width = 12, height = 5, units = "cm")
# Q value bar
Q <-plot_grid(plot_bar1_Q.main,plot_bar2_Q.main, ncol = 2, align = "h")
Q
ggsave("figures/barplot Qvalue.pdf", plot = Q, width = 12, height = 5, units = "cm")
# motor memory bar
force <-plot_grid(plot_bar1_f.main,plot_bar2_f.main, ncol = 2, align = "h")
force
ggsave("figures/barplot motor before selection.pdf", plot = force, width = 12, height = 5, units = "cm")

