#################### the function for the plot  ####################
# plot function for the line and point plot
plot_trajectory_error <- function(data_main, data_s, data_vf, x_breaks, x_labels, x_point, condition_number, ylims = c(-30, 50)) {
  
  ggplot(data_main, aes(x = total_tri, y = val_mean)) +
    geom_line(linewidth = 0.75, color = "green") +
    geom_ribbon(aes(ymin = val_mean - val_se, ymax = val_mean + val_se), alpha = .2) +
    geom_point(size = 1, color = "green") +
    geom_point(aes(x = x_point, y = 0), color = "white") +
    geom_point(data = data_s, aes(x = total_tri, y = val_mean), size = 1, color = "gray") +
    geom_line(data = data_s, aes(x = total_tri, y = val_mean), linewidth = 0.75, color = "gray") +
    geom_ribbon(data = data_s, aes(ymin = val_mean - val_se, ymax = val_mean + val_se), alpha = .2) +
    geom_point(data = data_vf, aes(x = total_tri, y = err_mean), size = 1, color = "orange") +
    geom_line(data = data_vf, aes(x = total_tri, y = err_mean), linewidth = 0.75, color = "orange") +
    labs(fill = "Background Color") +
    xlab("Trial") +
    ylab("Trajectory Error (mm)") +
    ylim(ylims) +
    scale_x_continuous(name = "Trial", 
                       breaks = x_breaks,  
                       labels = x_labels) + 
    theme_classic() +
    theme(legend.text = element_text(size = 6),
          plot.title = element_text(size = 6),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 4)) +  
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(linewidth = 0.25), 
          axis.line.y = element_line(linewidth = 0.25)) +
    ggtitle(sprintf("Trajectory Error in trials (Condition %d)", condition_number))
}

########################### raster function ###################################

# General function to create the raster plots
plot_raster <- function(data_long, data_summary, x_breaks, x_labels, fill_var, fill_limits, fill_label, y_label, x_label) {
  
  # Add the summarized data (if applicable) to the main data frame
  if (!is.null(data_summary)) {
    data_long[[fill_var]] <- data_summary$f_pvel
  }
  
  # Create the plot
  plot <- ggplot(data_long, aes(x = row_number, y = participant, fill = !!sym(fill_var))) +
    geom_raster() +
    scale_fill_gradientn(colors = c("cyan", "magenta"), limits = fill_limits) + 
    scale_x_continuous(name = x_label, 
                       breaks = x_breaks,  
                       labels = x_labels) + 
    scale_y_discrete(labels = paste0("sub", 1:10)) +  # Rename the y-axis labels
    labs(x = x_label, y = y_label, fill = fill_label) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 6),
      plot.title = element_text(size = 6),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 4),
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", colour = NA)  # Set background color
    )
  
  # Add vertical lines to the plot
  for(x_pos in x_breaks) {
    plot <- plot + geom_vline(xintercept = x_pos, color = "black", size = 0.25)
  }
  
  return(plot)
}

########################## raster plots for probability  ##############################
# General function to create the raster plots with annotations
plot_raster_with_annotations <- function(data_long, x_breaks, x_labels, fill_var, fill_colors, rect_intervals, rect_colors, y_label, x_label) {
  
  # Create the base plot
  plot <- ggplot(data_long, aes(x = row_number, y = participant, fill = !!sym(fill_var))) +
    geom_raster() +
    scale_fill_gradientn(colors = fill_colors) + 
    scale_x_continuous(name = x_label, 
                       breaks = x_breaks,  
                       labels = x_labels) + 
    scale_y_discrete(labels = paste0("sub", 1:10)) +  
    labs(x = x_label, y = y_label, fill = "Probability of tool A") +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 6),
      plot.title = element_text(size = 6),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 4),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.background = element_rect(fill = "white", colour = NA)  
    )
  
  # Add rectangular annotations
  for(i in seq(rect_intervals[1], rect_intervals[4], by = rect_intervals[2])) {
    plot <- plot + annotate("rect", xmin = i, xmax = i + rect_intervals[3], ymin = 11, ymax = 12,
                            fill = rect_colors[1])
    plot <- plot + annotate("rect", xmin = i + rect_intervals[3], xmax = min(i + rect_intervals[2], max(x_breaks)), ymin = 11, ymax = 12,
                            fill = rect_colors[2])
  }
  
  # Add vertical lines at x axis breaks
  for(x_pos in x_breaks) {
    plot <- plot + geom_vline(xintercept = x_pos,  color = "black", size = 0.25)
  }
  
  return(plot)
}

##############################  bar plots #################################

# General function to create bar plots
create_bar_plot <- function(data, y_var, fill_var, x_labels, width = 0.6, alpha = 0.7, title, condition_number) {
  
  ggplot(data = data, aes(x = factor(total_tri), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_col(width = width, alpha = alpha) +
    geom_errorbar(aes(ymin = pmax(!!sym(y_var) - val_se), ymax = pmin(!!sym(y_var) + val_se)), 
                  position = position_dodge(width = 0.8), width = 0.25) +  
    xlab("Trial") +
    ylab("Probability") + 
    scale_fill_manual(values = c("NULL" = "red", "FF" = "blue")) + # Specify colors for the fill groups
    labs(fill = "Group") +  
    scale_x_discrete("Trial", labels = x_labels) + 
    theme_classic() +
    theme(
      legend.text = element_text(size = 6),
      plot.title = element_text(size = 6),
      axis.text = element_text(size = 4),  # Adjust axis label size
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line.x = element_line(linewidth = 0.25), # X-axis line width and color
      axis.line.y = element_line(linewidth = 0.25)  # Y-axis line width and color
    ) +
    ggtitle(sprintf("%s (condition %d)", title, condition_number))
}

