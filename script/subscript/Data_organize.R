
source("script/subscript/set_basic_params.R")

##### Load packages #####

library("dplyr")
library("purrr")
library("ggplot2")
library(sjPlot)

############# Read in data #############
data_ind = vector(mode = "list", length = length(all_subs))  # clean data all for blocks
data_ind_blk24 = vector(mode = "list", length = length(all_subs))  # clean data for block2~4
data_ind_blk5 = vector(mode = "list", length = length(all_subs))   # clean data for block5
data_ind_blk68 = vector(mode = "list", length = length(all_subs))  # clean data for block6~8
data_for_choice = vector(mode = "list", length = length(all_subs)) # clean selection data all for blocks
data_for_choice_blk2_4 = vector(mode = "list", length = length(all_subs)) # clean selection data all for block2~4
data_for_choice_blk5 = vector(mode = "list", length = length(all_subs))  # clean selection data all for block5
data_for_choice_blk6_8 = vector(mode = "list", length = length(all_subs))  # clean selection data all for block6~8
csub = 0 # count subjects 

for (soi in all_subs){
  
  csub = csub + 1 # Update counter
  # Convert to string with prefix identifier
  if (soi < 10){
    sid_str = sprintf("S0%d",soi)
  } else {
    sid_str = sprintf("S%d",soi) 
  }
  
  # Read data from csv files
  data_fpath = sprintf("data/processed/%s",sid_str)
  data_force = read.csv(sprintf("%s/data_force.csv",data_fpath))
  data_tgt = read.csv(sprintf("%s/data_tgt.csv",data_fpath))
  data_tri_clean = read.csv(sprintf("%s/data_tri_clean.csv",data_fpath))
  
  # some cleaning
  data_ind_blk24[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 |select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",1))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(sub = csub) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(blk == 2|blk == 3|blk == 4) %>%
    dplyr::rename(val = peak_dev) %>% # This decides what will be plotted
    mutate(sub = soi)
  
  data_ind_blk5[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 |select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",1))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(sub = csub) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(blk == 5) %>%
    dplyr::rename(val = peak_dev) %>% # This decides what will be plotted
    mutate(sub = soi)
  
  data_ind_blk68[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 |select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",1))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(sub = csub) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(blk == 6|blk == 7|blk == 8) %>%
    dplyr::rename(val = peak_dev) %>% # This decides what will be plotted
    mutate(sub = soi)
  
  data_ind[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 |select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",1))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(sub = csub) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::rename(val = peak_dev) %>% # This decides what will be plotted
    mutate(sub = soi)
  
  data_for_choice[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 | select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",bvalc))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(ws == "L"|ws == "R") %>%
    dplyr::rename(val1 = peak_dev) %>% # This decides what will be plotted
    mutate(val = gain) %>%
    mutate(sub = soi)
  
  data_for_choice_blk2_4[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 | select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",bvalc))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(blk == 2|blk == 3|blk == 4) %>%
    dplyr::filter(ws == "L"|ws == "R") %>%
    dplyr::rename(val1 = peak_dev) %>% # This decides what will be plotted
    mutate(val = gain) %>%
    mutate(sub = soi)
  
  data_for_choice_blk5[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 | select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",bvalc))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(blk == 5) %>%
    dplyr::filter(ws == "L"|ws == "R") %>%
    dplyr::rename(val1 = peak_dev) %>% # This decides what will be plotted
    mutate(val = gain) %>%
    mutate(sub = soi)
  
  data_for_choice_blk6_8[[csub]] = data_tri_clean %>%
    dplyr::select(total_tri,blk_tri,blk,gain,select_right,select_left,largest_dev,bvalc,prb,fchoice,prechoice) %>% # Remove unused variables
    mutate(field = data_tgt$field, peak_f = data_force$peak_f, f_pvel = data_force$f_pvel , k = data_force$k, pvel = data_force$pvel/1000) %>% # Copy from other variables
    mutate(peak_dev = data_force$peak_dev) %>%
    mutate(ws = ifelse(fchoice == 1 | select_right == 1, "R", 
                       ifelse(fchoice == 2 | select_left == 1, "L",
                              ifelse(field == 3, "CC",sprintf("C%d",bvalc))))) %>%
    mutate(ws = factor(ws, levels = ff_level)) %>% # Convert to factor for coloring
    mutate(prechoice = factor(prechoice)) %>% # Convert to factor for markering
    dplyr::filter(blk == 6|blk == 7|blk == 8) %>%
    dplyr::filter(ws == "L"|ws == "R") %>%
    dplyr::rename(val1 = peak_dev) %>% # This decides what will be plotted
    mutate(val = gain) %>%
    mutate(sub = soi)

}

Cleandata_allblk <- data_ind        # clean data all for blocks
Cleandata_blk2_4 <- data_ind_blk24  # clean data for block2~4
Cleandata_blk5 <- data_ind_blk5     # clean data for block5
Cleandata_blk6_8 <- data_ind_blk68  # clean data for block6~8

save(Cleandata_allblk, file = "data/Cleandata_allblk.RData")  # clean data all for blocks
save(Cleandata_blk2_4, file = "data/Cleandata_blk2_4.RData")  # clean data for block2~4
save(Cleandata_blk5, file = "data/Cleandata_blk5.RData")      # clean data for block5
save(Cleandata_blk6_8, file = "data/Cleandata_blk6_8.RData")  # clean data for block6~8
save(data_for_choice, file = "data/data_for_choice.RData")    # clean selection data all for blocks
save(data_for_choice_blk2_4, file = "data/data_for_choice_blk2_4.RData")  # clean selection data all for block2~4
save(data_for_choice_blk5, file = "data/data_for_choice_blk5.RData")      # clean selection data all for block5
save(data_for_choice_blk6_8, file = "data/data_for_choice_blk6_8.RData")  # clean selection data all for block6~8
