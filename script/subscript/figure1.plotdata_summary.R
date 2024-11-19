##### Load packages #####
library("dplyr")
library("purrr")
library("ggplot2")
library(sjPlot)

#load data
load("data/Cleandata_allblk.RData")
load("data/Cleandata_blk2_4.RData")
load("data/Cleandata_blk5.RData")
load("data/Cleandata_blk6_8.RData")

################################################################################
#################### summary plot data for error   ##########################
################################################################################
data_plot_blk24 = reduce(Cleandata_blk2_4, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

data_plot_blk5 = reduce(Cleandata_blk5, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

data_plot_blk68 = reduce(Cleandata_blk6_8, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

data_plot_tmp = reduce(Cleandata_allblk, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws != "CC" & ws != "C1") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_blk24 = data_plot_blk24 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val, na.rm = T), val_median = median(val, na.rm = T),
            val_sd = sd(val, na.rm = T), val_se = sd(val, na.rm = T)/sqrt(length(val)))

sum_data_plot_blk5 = data_plot_blk5 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val, na.rm = T), val_median = median(val, na.rm = T),
            val_sd = sd(val, na.rm = T), val_se = sd(val, na.rm = T)/sqrt(length(val)))

sum_data_plot_blk68 = data_plot_blk68 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val, na.rm = T), val_median = median(val, na.rm = T),
            val_sd = sd(val, na.rm = T), val_se = sd(val, na.rm = T)/sqrt(length(val)))

data_plot_s_blk24 = reduce(Cleandata_blk2_4, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "L"|ws == "R") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_s_blk24 = data_plot_s_blk24 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val, na.rm = T), val_median = median(val, na.rm = T),
            val_sd = sd(val, na.rm = T), val_se = sd(val, na.rm = T)/sqrt(length(val)))

data_plot_s_blk5 = reduce(Cleandata_blk5, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "L"|ws == "R") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_s_blk5 = data_plot_s_blk5 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val, na.rm = T), val_median = median(val, na.rm = T),
            val_sd = sd(val, na.rm = T), val_se = sd(val, na.rm = T)/sqrt(length(val)))

data_plot_s_blk68 = reduce(Cleandata_blk6_8, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "L"|ws == "R") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))

sum_data_plot_s_blk68 = data_plot_s_blk68 %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(val, na.rm = T), val_median = median(val, na.rm = T),
            val_sd = sd(val, na.rm = T), val_se = sd(val, na.rm = T)/sqrt(length(val)))
################################################################################
#################### summary plot data for channel force plot ##################
################################################################################
data_plot_cc_blk24 = reduce(Cleandata_blk2_4, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "CC") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))
data_plot_cc_blk24$f_pvel[146]<- NA
data_plot_cc_blk24$pvel[146]<- NA

sum_data_plot_vf_blk24 = data_plot_cc_blk24 %>%
  group_by(total_tri,bvalc) %>%
  summarise(err_mean = mean(val, na.rm = TRUE), err_median = median(val,na.rm = TRUE),
            err_sd = sd(val, na.rm = TRUE), err_se = sd(val, na.rm = TRUE)/sqrt(length(val)),
            val_mean = mean(f_pvel, na.rm = TRUE), val_median = median(f_pvel,na.rm = TRUE),
            val_sd = sd(f_pvel, na.rm = TRUE), val_se = sd(f_pvel, na.rm = TRUE)/sqrt(length(f_pvel)))

data_plot_cc_blk5 = reduce(Cleandata_blk5, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "CC") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))
data_plot_cc_blk5$f_pvel[146]<- NA
data_plot_cc_blk5$pvel[146]<- NA

sum_data_plot_vf_blk5 = data_plot_cc_blk5 %>%
  group_by(total_tri,bvalc) %>%
  summarise(err_mean = mean(val, na.rm = TRUE), err_median = median(val,na.rm = TRUE),
            err_sd = sd(val, na.rm = TRUE), err_se = sd(val, na.rm = TRUE)/sqrt(length(val)),
            val_mean = mean(f_pvel, na.rm = TRUE), val_median = median(f_pvel,na.rm = TRUE),
            val_sd = sd(f_pvel, na.rm = TRUE), val_se = sd(f_pvel, na.rm = TRUE)/sqrt(length(f_pvel)))

data_plot_cc_blk68 = reduce(Cleandata_blk6_8, rbind) %>%
  mutate(center_state = lag(bvalc,1)) %>% # bvalue in current train trials
  # dplyr::filter(lag(ws,1) == "CC") %>% # Only channel trials
  dplyr::filter(ws == "CC") %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub))
data_plot_cc_blk68$f_pvel[146]<- NA
data_plot_cc_blk68$pvel[146]<- NA

sum_data_plot_vf_blk68 = data_plot_cc_blk68 %>%
  group_by(total_tri,bvalc) %>%
  summarise(err_mean = mean(val, na.rm = TRUE), err_median = median(val,na.rm = TRUE),
            err_sd = sd(val, na.rm = TRUE), err_se = sd(val, na.rm = TRUE)/sqrt(length(val)),
            val_mean = mean(f_pvel, na.rm = TRUE), val_median = median(f_pvel,na.rm = TRUE),
            val_sd = sd(f_pvel, na.rm = TRUE), val_se = sd(f_pvel, na.rm = TRUE)/sqrt(length(f_pvel)))

################################################################################
################### summary reinforcement result data  #########################
################################################################################

source("script/RL_simulation.R")

probabilities1<-list()
probabilities2<-list()
probabilities3<-list()

for (m in 1:10){
  data1 = data_for_choice[[m]] %>%
    dplyr::filter(blk ==2| blk == 3 |blk == 4)
  
  data1 <- data1$select_left
  data_length <- length(data1)
  probabilities1[[m]] <- numeric(ceiling(data_length / 8))
  for (i in seq(1, data_length, by = 8)) {
    current_group <- data1[i:min(i+7, data_length)]
    probabilities1[[m]][ceiling(i / 8)] <- mean(current_group == 1)
  }
  
  data2 = data_for_choice[[m]] %>%
    dplyr::filter(blk == 5)
  
  data2 <- data2$select_left
  data_length <- length(data2)
  probabilities2[[m]] <- numeric(ceiling(data_length / 10))
  for (i in seq(1, data_length, by = 4)) {
    current_group <- data2[i:min(i+9, data_length)]
    probabilities2[[m]][ceiling(i / 10)] <- mean(current_group == 1)
  }
  
  data3 = data_for_choice[[m]] %>%
    dplyr::filter(blk == 6 |blk == 7|blk == 8)
  
  data3 <- data3$select_left
  data_length <- length(data3)
  probabilities3[[m]] <- numeric(ceiling(data_length / 10))
  for (i in seq(1, data_length, by = 10)) {
    current_group <- data3[i:min(i+9, data_length)]
    probabilities3[[m]][ceiling(i / 10)] <- mean(current_group == 1)
  }
  
}  

########################  Q value for block5     ########################## 
# Q1-Q2 for block5
Q_difference_blk5 <- data.frame()

# use apply function for each 10 trials
Q_difference_blk5 <- as.data.frame(apply(final_data_blk5, 2, function(col) {
  sapply(seq(1, length(col), by = 10), function(i) {
    mean(col[i:min(i + 9, length(col))])
  })
}))

Q_difference_blk5_long <- data.frame(total_tri = integer(), subject = character(), value = integer(), stringsAsFactors = FALSE)

cols_to_merge <- grep("sub\\d+Q1-Q2", names(Q_difference_blk5), value = TRUE)


for (col in cols_to_merge) {
  temp <- data.frame(total_tri = Q_difference_blk5$total_tri,  subject = col, value = Q_difference_blk5[[col]])
  Q_difference_blk5_long <- rbind(Q_difference_blk5_long, temp)
} 

# subject order by probability value
given_order <- c("sub_9", "sub_10", "sub_2", "sub_7", "sub_8", "sub_6", "sub_4", "sub_1", "sub_3", "sub_5")



data_plot_color2 <- as.data.frame(do.call(cbind, probabilities2))
colnames(data_plot_color2) <- paste("sub", seq_along(probabilities2), sep = "_")
data_df <- data_plot_color2
data_df$row_number <- seq_len(nrow(data_df))
data_long_blk5 <- gather(data_df, key = "participant", value = "value", -row_number)
data_long_blk5$row_number <- 172+as.numeric(as.character(data_long_blk5$row_number)) * 30
data_long_blk5$participant <- factor(data_long_blk5$participant, levels = given_order)
data_long_blk5$Q_d <- Q_difference_blk5_long$value

########################  Q value for block6~8     ##########################  
Q_difference_blk68 <- data.frame()

Q_difference_blk68 <- as.data.frame(apply(final_data_blk68, 2, function(col) {
  sapply(seq(1, length(col), by = 10), function(i) {
    mean(col[i:min(i + 9, length(col))])
  })
}))

Q_difference_blk68_long <- data.frame(total_tri = integer(), subject = character(), value = integer(), stringsAsFactors = FALSE)

cols_to_merge <- grep("sub\\d+Q1-Q2", names(Q_difference_blk68), value = TRUE)

for (col in cols_to_merge) {
  temp <- data.frame(total_tri = Q_difference_blk68$total_tri,  subject = col, value = Q_difference_blk68[[col]])
  Q_difference_blk68_long <- rbind(Q_difference_blk68_long, temp)
} 

given_order <- c("sub_9", "sub_10", "sub_2", "sub_7", "sub_8", "sub_6", "sub_4", "sub_1", "sub_3", "sub_5")



data_plot_color3 <- as.data.frame(do.call(cbind, probabilities3))
colnames(data_plot_color3) <- paste("sub", seq_along(probabilities3), sep = "_")
data_df <- data_plot_color3
data_df$row_number <- seq_len(nrow(data_df))
data_long_blk68 <- gather(data_df, key = "participant", value = "value", -row_number)
data_long_blk68$row_number <- 257+as.numeric(as.character(data_long_blk68$row_number)) * 70
data_long_blk68$participant <- factor(data_long_blk68$participant, levels = given_order)
data_long_blk68$Q_d <- Q_difference_blk68_long$value


############## summary force data for plot in block 5###########################
f_blk5_long <- data_plot_cc_blk5 %>%
  group_by(sub) %>% 
  mutate(group = ceiling(row_number() / 10)) %>%  
  group_by(sub, group) %>%  
  summarise(f_pvel = mean(f_pvel, na.rm = TRUE)) %>%  
  ungroup()  # 

data_long_blk5$f <- f_blk5_long$f_pvel

############## summary force data for plot in block 6~8###########################
f_blk68_long <- data_plot_cc_blk68 %>%
  group_by(sub) %>% 
  mutate(group = ceiling(row_number() / 10)) %>%  
  group_by(sub, group) %>%  
  summarise(f_pvel = mean(f_pvel, na.rm = TRUE)) %>%  
  ungroup()  # 

data_long_blk68$f <- f_blk68_long$f_pvel


################## probability  data for plot ###############################

# Process the first dataset (probabilities1)
data_plot_color1 <- as.data.frame(do.call(cbind, probabilities1))
colnames(data_plot_color1) <- paste("sub", seq_along(probabilities1), sep = "_")
data_df1 <- data_plot_color1
data_df1$row_number <- seq_len(nrow(data_df1))
data_long1 <- gather(data_df1, key = "participant", value = "value", -row_number)
data_long1$row_number <- 79 + as.numeric(as.character(data_long1$row_number)) * 10
data_long1$participant <- factor(data_long1$participant, levels = given_order)

# Process the second dataset (probabilities2)
data_plot_color2 <- as.data.frame(do.call(cbind, probabilities2))
colnames(data_plot_color2) <- paste("sub", seq_along(probabilities2), sep = "_")
data_df2 <- data_plot_color2
data_df2$row_number <- seq_len(nrow(data_df2))
data_long2 <- gather(data_df2, key = "participant", value = "value", -row_number)
data_long2$row_number <- 172 + as.numeric(as.character(data_long2$row_number)) * 30
data_long2$participant <- factor(data_long2$participant, levels = given_order)

# Process the third dataset (probabilities3)
data_plot_color3 <- as.data.frame(do.call(cbind, probabilities3))
colnames(data_plot_color3) <- paste("sub", seq_along(probabilities3), sep = "_")
data_df3 <- data_plot_color3
data_df3$row_number <- seq_len(nrow(data_df3))
data_long3 <- gather(data_df3, key = "participant", value = "value", -row_number)
data_long3$row_number <- 257 + as.numeric(as.character(data_long3$row_number)) * 70
data_long3$participant <- factor(data_long3$participant, levels = given_order)

###################################################################################
#########################   figure1.f~k bar plot   ##################################
#####################  probability in certain trial ###############################
###################################################################################
final_data_long <- data.frame(total_tri = integer(), subject = character(), value = integer(), stringsAsFactors = FALSE)
final_data_combined <- rbind(final_data_blk5, final_data_blk68)
cols_to_merge <- grep("sub\\d+Q1-Q2", names(final_data_combined), value = TRUE)

##################### certain trial data analyze ###############################
#analyze the trial for the trials before the PTB changes

for (col in cols_to_merge) {
  temp <- data.frame(total_tri = final_data_combined$total_tri,  subject = col, value = final_data_combined[[col]])
  final_data_long <- rbind(final_data_long, temp)
}
Qd_select<- data.frame()
Qd_select <- final_data_long %>%
  mutate(Q = lag(value))%>%
  dplyr::filter((total_tri %in% c(230, 233, 290, 293, 390, 397, 530, 537, 670, 677, 810, 817))) 

data_plot_analyze = reduce(Cleandata_allblk, rbind) %>%
  dplyr::filter(ws != "CC") %>%
  dplyr::filter((total_tri %in% c(230, 233, 290, 293, 390, 397, 530, 537, 670, 677, 810, 817))) %>%
  mutate(bvalc = factor(bvalc), sub = factor(sub),Q_difference = Qd_select$Q)


sum_data_plot_bar = data_plot_analyze %>%
  group_by(total_tri,bvalc) %>%
  summarise(val_mean = mean(select_right, na.rm = T), val_median = median(select_right, na.rm = T),
            val_sd = sd(select_right, na.rm = T), val_se = sd(select_right, na.rm = T)/sqrt(length(select_right)),
            Q_mean = mean(Q_difference, na.rm = T), Q_median = median(Q_difference, na.rm = T),
            Q_sd = sd(Q_difference, na.rm = T), Q_se = sd(Q_difference, na.rm = T)/sqrt(length(Q_difference))
  )

sum_data_plot_bar <- sum_data_plot_bar %>%
  ungroup() %>%
  mutate(row = row_number()) %>%  # Create a new column to store row numbers
  mutate(group = (row - 1) %/% 2 + 1) %>%   # Group every two rows together
  mutate(color_group = ifelse(row %% 2 == 0, "FF", "NULL")) %>%  # Assign groups based on whether the row number is even or odd
  select(-row)  # del the raw tag

custom_labels1 <- c("150", "153", "210", "213")  
custom_labels2 <- c("310", "317","450", "457","590", "597","730", "737")

sum_data_plot_bar$group <- as.factor(sum_data_plot_bar$group) 
sum_data_plot_bar1<-sum_data_plot_bar %>%
  dplyr::filter(total_tri %in% c(230, 233, 290, 293))

sum_data_plot_bar$group <- as.factor(sum_data_plot_bar$group) 
sum_data_plot_bar2<-sum_data_plot_bar %>%
  dplyr::filter(total_tri %in% c(390, 397, 530, 537, 670, 677, 810, 817))

#################################################################################
#################################################################################

f_labels1 <- c("148", "151", "208", "211")  
f_labels2 <- c("308", "315","448", "455","588", "595","728", "735")
sum_data_plot_vf_bar1<-sum_data_plot_vf_blk5 %>%
  dplyr::filter(total_tri %in% c(228, 231, 288, 291))
sum_data_plot_vf_bar1 <- sum_data_plot_vf_bar1 %>%
  ungroup() %>%
  mutate(row = row_number()) %>%  
  mutate(group = (row - 1) %/% 2 + 1) %>%   
  mutate(color_group = ifelse(row %% 2 == 0, "FF", "NULL")) %>%  
  select(-row)  
sum_data_plot_vf_bar1$group <- as.factor(sum_data_plot_vf_bar1$group) 

sum_data_plot_vf_bar2<-sum_data_plot_vf_blk68 %>%
  dplyr::filter(total_tri %in% c(388, 395, 528, 535, 668, 675, 808, 815))
sum_data_plot_vf_bar2 <- sum_data_plot_vf_bar2 %>%
  ungroup() %>%
  mutate(row = row_number()) %>%  
  mutate(group = (row - 1) %/% 2 + 1) %>%   
  mutate(color_group = ifelse(row %% 2 == 0, "FF", "NULL")) %>%  
  select(-row)  
sum_data_plot_vf_bar2$group <- as.factor(sum_data_plot_vf_bar2$group) 
