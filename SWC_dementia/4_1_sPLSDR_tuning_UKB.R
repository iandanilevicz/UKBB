# ---------------------------------------------------------------------------- #
# Script : 4_1_sPLSDR_tuning_UKB.R
# Author : I M Danilevicz 
# Remark : Part of the work available in:
#          C Cavailles, IM Danilevicz, S Vidil et al (2026). 
#          "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults", (under review)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Set local language as english to be able to read dates properly
# Clear the Environment 
#rm(list=ls())
#Sys.setlocale("LC_ALL","English")
#the_seed = 2020
# ---------------------------------------------------------------------------- #
# Packages 
library(tidyverse)
library(survival)
library(smoothHR)
library(compareC)
library(plsRcox)
library(patchwork)
# ---------------------------------------------------------------------------- #
# Folders

dir_load = "D:/..."
dir_fun  = "D:/..."
dir_cv_boot   = "D:/..."

# ---------------------------------------------------------------------------- #
# Functions
setwd(dir_fun)
source("0-0_aux_functions_v2.R")


# ---------------------------------------------------------------------------- #
# Data
setwd(dir_load)
dat0 = read.csv2("data_UKB_clean.csv", header = TRUE, sep=",", dec=".")
X <- dat0 %>% 
  select(starts_with("cov"), starts_with("y"), starts_with("rar_"), 
         starts_with("mb_"), starts_with("ct_"), starts_with("spt_"))

X_acc = dat0 %>% select(starts_with("rar_"), starts_with("mb_"), starts_with("ct_"), starts_with("spt_"))
S_acc                  = as.data.frame(scale(X_acc))
S_acc$mb_dur_lipa_sq   = scale(S_acc$mb_dur_lipa^2)
S_acc$mb_mdb_lipa_sq   = scale(S_acc$mb_mdb_lipa^2)
S_acc$mb_mdb_mvpa_sq   = scale(S_acc$mb_mdb_mvpa^2)
S_acc$spt_dur_sleep_sq = scale(S_acc$spt_dur_sleep^2)
R_acc = rescale_mat(S_acc, sigma=6, scale=FALSE)
n = dim(R_acc)[1]
Cas = dat0$y 
T   = dat0$y_time # delay
Y = cbind(X$y_time,X$y); colnames(Y) = c("time", "event")
X$y = dat0$y
X$y_time = dat0$y_time
X$cov1_sex     = as.factor(X$cov1_sex) 
X$cov1_edu     = as.factor(X$cov1_edu)
X$cov1_marcoh  = as.factor(X$cov1_marcoh) 
X$cov1_work    = as.factor(X$cov1_work) 
X$cov1_smoke   = as.factor(X$cov1_smoke) #never, ex(>5y), current
X$cov1_veg     = as.factor(X$cov1_veg) 
X$cov1_alcohol = as.factor(X$cov1_alcohol) 
X$cov1_bmic    = factor(ifelse(X$cov1_bmi<25,1,ifelse(X$cov1_bmi>=30,3,2))) 
X$cov1_hypt    = as.factor(X$cov1_hypt) 
X$cov1_diab    = as.factor(X$cov1_diab) 
X$cov1_hypl    = as.factor(X$cov1_hypl) 
X$cov1_ncdc    = factor(ifelse(X$cov1_Nchronic==0,0, ifelse(X$cov1_Nchronic==1,1,2)))
X$cov1_cns     = as.factor(X$cov1_cns) 

# ---------------------------------------------------------------------------- #
# sPLS procedure tuning

cov0_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"


# ---------------------------------------------------------------------------- #
# U-statistics Part 1: 
# ---------------------------------------------------------------------------- #

eta1 = 0:19/20
C1 = compare_c_eta(data=X, R_acc=R_acc, y_str="y", time_str="y_time", cov_str1=cov0_acc0, eta=eta1, D=1, round=3)

setwd(dir_cv_boot)
save.image(file="u_tuning1_ukb.RData")
save(C1, file="u_tuning1_ukb.rda")
C2 = compare_c_eta(data=X,R_acc=R_acc, y_str="y", time_str="y_time", cov_str1=cov0_acc0, eta=eta1, D=2, round=3)
setwd(dir_cv_boot)
save.image(file="u_tuning2_ukb.RData")
save(C2, file="u_tuning2_ukb.rda")
C3 = compare_c_eta(data=X,R_acc=R_acc, y_str="y", time_str="y_time", cov_str1=cov0_acc0, eta=eta1, D=2, round=3)
setwd(dir_cv_boot)
save.image(file="u_tuning3_ukb.RData")
save(C3, file="u_tuning3_ukb.rda")



# ---------------------------------------------------------------------------- #
# load 
setwd(dir_cv_boot)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
load("u_tuning3_ukb.RData")

# ---------------------------------------------------------------------------- #
# Plot



colnames(C1) = c("Eta", "C", "low", "up", "Delta", "dlow", "dup", "Sign")  
colnames(C2) = c("Eta", "C", "low", "up", "Delta", "dlow", "dup", "Sign")  
colnames(C3) = c("Eta", "C", "low", "up", "Delta", "dlow", "dup", "Sign")  

C1 = as.data.frame(C1)
C2 = as.data.frame(C2)
C3 = as.data.frame(C3)
C1 = C1[1:20,]
C2 = C2[1:20,]
C3 = C3[1:20,]

C1$bar_color[1:20] <- "gray80"
C3$bar_color[1:20] <- "gray80"
C2$bar_color <- "black"                       # default color
C2$bar_color[(nrow(C2)-1):nrow(C2)] <- "gray80"  # last two bars
C2$bar_color[(nrow(C2)-2)] <- "red"  # optimal


x_ticks_small  <- C1[1:20,1]
tick_height_small  <- 0.001  # small line from y=0 to y=tick_height
x_ticks_large <- c(0, 0.25, 0.5, 0.75)
tick_height_large <- 0.002 

p1 <- ggplot(C1, aes(x = Eta)) +
  geom_segment(aes(y = low, yend = up, xend = Eta), linewidth = 1, color = "gray40") +
  geom_point(aes(y = C), size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "black") +  # x-axis at y=0
  coord_cartesian(ylim = c(0.74, 0.79)) +
  geom_segment(data = data.frame(x = x_ticks_small),
               aes(x = x, xend = x, y = 0.7385, yend = -tick_height_small),  # negative to go down
               color = "black", linewidth = 0.5) +
  
  # Large ticks below
  geom_segment(data = data.frame(x = x_ticks_large),
               aes(x = x, xend = x, y = 0.74, yend = -tick_height_large),  # negative to go down
               color = "black", linewidth = 0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                                  # remove default grids
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # add box
  ) +
  labs(x = expression(eta), y = "C-index")

p2 <- ggplot(C2, aes(x = Eta)) +
  geom_segment(aes(y = low, yend = up, xend = Eta), linewidth = 1, color = "gray40") +
  geom_point(aes(y = C), size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "black") +  # x-axis at y=0
  coord_cartesian(ylim = c(0.74, 0.79)) +
  geom_segment(data = data.frame(x = x_ticks_small),
               aes(x = x, xend = x, y = 0.7385, yend = -tick_height_small),  # negative to go down
               color = "black", linewidth = 0.5) +
  
  # Large ticks below
  geom_segment(data = data.frame(x = x_ticks_large),
               aes(x = x, xend = x, y = 0.74, yend = -tick_height_large),  # negative to go down
               color = "black", linewidth = 0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                                  # remove default grids
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # add box
  ) +
  labs(x = expression(eta), y = "C-index")

p3 <- ggplot(C3, aes(x = Eta)) +
  geom_segment(aes(y = low, yend = up, xend = Eta), linewidth = 1, color = "gray40") +
  geom_point(aes(y = C), size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "black") +  # x-axis at y=0
  coord_cartesian(ylim = c(0.74, 0.79)) +
  geom_segment(data = data.frame(x = x_ticks_small),
               aes(x = x, xend = x, y = 0.7385, yend = -tick_height_small),  # negative to go down
               color = "black", linewidth = 0.5) +
  
  # Large ticks below
  geom_segment(data = data.frame(x = x_ticks_large),
               aes(x = x, xend = x, y = 0.74, yend = -tick_height_large),  # negative to go down
               color = "black", linewidth = 0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                                  # remove default grids
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # add box
  ) +
  labs(x = expression(eta), y = "C-index")


p4 <- ggplot(C1, aes(x = Eta)) +
  geom_segment(aes(y = dlow, yend = dup, xend = Eta, color = bar_color), linewidth = 1) +
  geom_point(aes(y = Delta, color = bar_color), size = 3) +
  geom_hline(yintercept = 0.01, linetype = "dotted", color = "gray40") +
  #geom_hline(yintercept = 0, color = "black") +
  # Small ticks above
  geom_segment(data = data.frame(x = x_ticks_small),
               aes(x = x, xend = x, y = -0.1, yend = -tick_height_small),  # negative to go down
               color = "black", linewidth = 0.5) +
  
  # Large ticks below
  geom_segment(data = data.frame(x = x_ticks_large),
               aes(x = x, xend = x, y = 0, yend = -tick_height_large),  # negative to go down
               color = "black", linewidth = 0.7) +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  labs(x = expression(eta), y = expression(Delta~"C-index"))


p5 <- ggplot(C2, aes(x = Eta)) +
  geom_segment(aes(y = dlow, yend = dup, xend = Eta, color = bar_color), linewidth = 1) +
  geom_point(aes(y = Delta, color = bar_color), size = 3) +
  geom_hline(yintercept = 0.01, linetype = "dotted", color = "gray40") +
 # geom_hline(yintercept = 0, color = "black") +   # keep x-axis at y=0
  # Small ticks above
  geom_segment(data = data.frame(x = x_ticks_small),
               aes(x = x, xend = x, y = -0.1, yend = -tick_height_small),  # negative to go down
               color = "black", linewidth = 0.5) +
  
  # Large ticks below
  geom_segment(data = data.frame(x = x_ticks_large),
               aes(x = x, xend = x, y = 0, yend = -tick_height_large),  # negative to go down
               color = "black", linewidth = 0.7) +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_color_identity() +      # ensures colors are used as-is, no legend
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # add box
  ) +
  labs(x = expression(eta), y = expression(Delta~"C-index"))

p6 <- ggplot(C3, aes(x = Eta)) +
  geom_segment(aes(y = dlow, yend = dup, xend = Eta, color = bar_color), linewidth = 1) +
  geom_point(aes(y = Delta, color = bar_color), size = 3) +
  geom_hline(yintercept = 0.01, linetype = "dotted", color = "gray40") +
  #geom_hline(yintercept = 0, color = "black") +
  # Small ticks above
  geom_segment(data = data.frame(x = x_ticks_small),
               aes(x = x, xend = x, y = -0.1, yend = -tick_height_small),  # negative to go down
               color = "black", linewidth = 0.5) +
  
  # Large ticks below
  geom_segment(data = data.frame(x = x_ticks_large),
               aes(x = x, xend = x, y = 0, yend = -tick_height_large),  # negative to go down
               color = "black", linewidth = 0.7) +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  labs(x = expression(eta), y = expression(Delta~"C-index"))


# Create small text-only plots for headers
header1 <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Number of components = 1", size = 5) + 
  theme_void()

header2 <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Number of components = 2", size = 5) + 
  theme_void()

header3 <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Number of components = 3", size = 5) + 
  theme_void()

# Combine headers and plots
final_plot <- (
  header1 | header2 | header3
) /
  (p1 | p2 | p3) /
  (p4 | p5 | p6) +
  plot_layout(heights = c(0.15, 1, 1)) 

final_plot
# ---------------------------------------------------------------------------- #



 

