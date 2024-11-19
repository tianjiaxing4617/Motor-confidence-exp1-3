
library("purrr")
library("egg")
library("rjson")
library("loo")
library('nloptr')
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2) 
library(dplyr)
library(patchwork)
library(tidyverse)
library(broom)
library("reshape2")
library("purrr")
library(lme4)
library(effects)
library(lmerTest)


####################################################################
#################  mcnemar test for P value ########################
####################################################################

# select the index of the trials in the figure f~k (selection trial).
# organize data of trial 810 VS trial 817 
data_plot_pvalue1_blk68<-data_plot_analyze %>%
  dplyr::filter(total_tri %in% c(810))
data_plot_pvalue2_blk68<-data_plot_analyze %>%
  dplyr::filter(total_tri %in% c(817))
# organize data of trial 290 VS trial 293 
data_plot_pvalue1_blk5<-data_plot_analyze %>%
  dplyr::filter(total_tri %in% c(290))
data_plot_pvalue2_blk5<-data_plot_analyze %>%
  dplyr::filter(total_tri %in% c(293))

### mcnemar test ####
# select right of trial 810 VS trial 817

contingency_table1 <- matrix(c(2, 8, 0, 0), nrow = 2, byrow = TRUE,
                            dimnames = list("data_plot_pvalue1" = c("0", "1"),
                                            "data_plot_pvalue2" = c("0", "1")))
# select right of trial 290 VS trial 293 

contingency_table2 <- matrix(c(8, 0, 1, 1), nrow = 2, byrow = TRUE,
                            dimnames = list("data_plot_pvalue1" = c("0", "1"),
                                            "data_plot_pvalue2" = c("0", "1")))
print(contingency_table1)
print(contingency_table2)

#print the result of mcnemar test
mcnemar_result <- mcnemar.test(contingency_table1)
mcnemar_result <- mcnemar.test(contingency_table2)
print(mcnemar_result1)
print(mcnemar_result2)

#print the result of wilcox test
wilcox.test(data_plot_pvalue1$Q_difference, data_plot_pvalue2$Q_difference, paired = TRUE)
wilcox.test(data_plot_pvalue1_blk5$Q_difference, data_plot_pvalue2_blk5$Q_difference, paired = TRUE)

# select the index of the trials in the figure f~k (selection trial).
# datas of channel trial 808 VS trial 815 
data_plot_cc_pvalue1_blk68<-data_plot_cc_blk68 %>%
  dplyr::filter(total_tri %in% c(808))
data_plot_cc_pvalue2_blk68<-data_plot_cc_blk68 %>%
  dplyr::filter(total_tri %in% c(815))
wilcox.test(data_plot_cc_pvalue1_blk68$f_pvel, data_plot_cc_pvalue2_blk68$f_pvel, paired = TRUE)

# datas of channel trial 288 VS trial 291
data_plot_cc_pvalue1_blk5<-data_plot_cc_blk5 %>%
  dplyr::filter(total_tri %in% c(288))
data_plot_cc_pvalue2_blk5<-data_plot_cc_blk5 %>%
  dplyr::filter(total_tri %in% c(291))
wilcox.test(data_plot_cc_pvalue1_blk5$f_pvel, data_plot_cc_pvalue2_blk5$f_pvel, paired = TRUE)

