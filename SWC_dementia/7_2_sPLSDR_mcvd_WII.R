# ---------------------------------------------------------------------------- #
# Script : 7_2_sPLSDR_mcvd_WII.R
# Author : I M Danilevicz 
# Remark : Part of the work available in:
#          C Cavailles, IM Danilevicz, S Vidil et al (2026). 
#          "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults", (under review)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Set local language as english to be able to read dates properly
# Clear the Environment 
rm(list=ls())
Sys.setlocale("LC_ALL","English")

# ---------------------------------------------------------------------------- #
# Packages 
library(compareC)
library(tidyverse)
library(survival)
# ---------------------------------------------------------------------------- #
# Folders
dir_load = "D:/..."
dir_fun  = "D:/..."
dir_cv   = "D:/..."
dir_res  = "D:/..."

# ---------------------------------------------------------------------------- #
# Functions
setwd(dir_fun)
source("0-0_aux_functions.R")
# ---------------------------------------------------------------------------- #
# Data
setwd(dir_load)
dat0 = read.csv2("data_WII_clean.csv", header = TRUE, sep=",", dec=".")
X <- dat0 %>% 
  select(starts_with("cov"), starts_with("y"), starts_with("rar_"), 
         starts_with("mb_"), starts_with("ct_"), starts_with("spt_"), "fptau")

X_acc = dat0 %>% select(starts_with("rar_"), starts_with("mb_"), starts_with("ct_"), starts_with("spt_"))
S_acc                  = as.data.frame(scale(X_acc))
S_acc$spt_dur_sleep_sq = scale(S_acc$spt_dur_sleep^2)
R_acc = rescale_mat(S_acc, sigma=6, scale=FALSE)
n = dim(R_acc)[1]
Cas = dat0$y 
T   = dat0$y_time 
y = dat0$y
y_time = dat0$y_time
Y = cbind(X$y_time,X$y); colnames(Y) = c("time", "event")
X$y = dat0$y
X$y_time = dat0$y_time
X$cov1_sex     = as.factor(X$cov1_sex) 
X$cov1_edu     = as.factor(X$cov1_edu)
X$cov1_marcoh  = as.factor(X$cov1_marcoh) 
X$cov1_work    = as.factor(X$cov1_work) 
X$cov1_smoke   = as.factor(X$cov1_smoke) 
X$cov1_veg     = as.factor(X$cov1_veg) 
X$cov1_alcohol = as.factor(X$cov1_alcohol) 
X$cov1_bmic    = factor(ifelse(X$cov1_bmi<25,1,ifelse(X$cov1_bmi>=30,3,2))) 
X$cov1_hypt    = as.factor(X$cov1_hypt) 
X$cov1_diab    = as.factor(X$cov1_diab) 
X$cov1_hypl    = as.factor(X$cov1_hypl) 
X$cov1_ncdc    = factor(ifelse(X$cov1_Nchronic==0,0, ifelse(X$cov1_Nchronic==1,1,2))) 
X$cov1_cns     = as.factor(X$cov1_cns) 
X$cov2_adl     = as.factor(X$cov1_adl) 
X$cov2_dep     = as.factor(X$cov1_ghq) 
X$cov3_ptau     = log(X$fptau) 

# ---------------------------------------------------------------------------- #
# Reference model 
cov0_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

cov0_acc1 = "sdt_score1 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

cov0_acc2 = "sdt_score1 + sdt_score2 + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

cov0_Acc0 = c("cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
"cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

cov0_Acc1 = c("sdt_score1", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

cov0_Acc2 = c("sdt_score1", "sdt_score2", "cov1_age", "cov1_sex",  "cov1_edu", "cov1_marcoh", "cov1_smoke", 
              "cov1_veg", "cov1_alcohol", "cov1_bmic", "cov1_hypt", "cov1_diab", "cov1_hypl", "cov1_ncdc", "cov1_cns")

# ---------------------------------------------------------------------------- #
# Model fit
D = 2
eta = 0.85
setwd(dir_res)
load(file="W_star_ukb85.rda")
full_list = colnames(R_acc)
Wtemp     = build_matrix(full_list, W_star)
Xscores   =  as.matrix(R_acc) %*% Wtemp
X$score1 = Xscores[,1]
X$score2 = Xscores[,2]
X$sdt_score1 = scale(X$score1)
X$sdt_score2 = scale(X$score2)

# remove prevalence CVD 
W = X 
W = W %>% filter(W$y_cvd_time > 0)
# remove NA mortality 
W1 = X 
W1 = W1 %>% filter(W1$y_m_time > 0)
W2 = X %>% filter(is.na(X$y_m_time))

mod_acc0_cvd = coxph(as.formula(paste0("Surv(y_cvd_time, y_cvd) ~ ", cov0_acc0)), data = W)
mod_acc1_cvd = coxph(as.formula(paste0("Surv(y_cvd_time, y_cvd) ~ ", cov0_acc1)), data = W)
mod_acc2_cvd = coxph(as.formula(paste0("Surv(y_cvd_time, y_cvd) ~ ", cov0_acc2)), data = W)

mod_acc0_mort = coxph(as.formula(paste0("Surv(y_m_time, y_m) ~ ", cov0_acc0)), data = W1)
mod_acc1_mort = coxph(as.formula(paste0("Surv(y_m_time, y_m) ~ ", cov0_acc1)), data = W1)
mod_acc2_mort = coxph(as.formula(paste0("Surv(y_m_time, y_m) ~ ", cov0_acc2)), data = W1)

# ---------------------------------------------------------------------------- #
# follow up 
mean(W$y_cvd_time/365.25, na.rm=TRUE)
mean(W1$y_m_time/365.25, na.rm=TRUE)

max(W$y_cvd_time/365.25, na.rm=TRUE)
max(W1$y_m_time/365.25, na.rm=TRUE)

# ---------------------------------------------------------------------------- #
# Model summary
H0 = compare_HR_1_12(mod1=mod_acc1_cvd, mod2=mod_acc2_cvd, alpha=0.05, round=3)
H1 = compare_HR_1_12(mod1=mod_acc1_mort, mod2=mod_acc2_mort, alpha=0.05, round=3)


# H0 same covariates as UK Biobank: CVD
H0
mod_acc1_cvd$n
mod_acc1_cvd$nevent
age0 = age_at_event(W$cov1_age, W$y_cvd_time, W$y, W$cov1_bmi)
age0  

# same covariates as UK Biobank: mortality
H1
mod_acc1_mort$n
mod_acc1_mort$nevent
age1 = age_at_event(W1$cov1_age, W1$y_m_time, W1$y, cbind(W1$cov1_sex))
age1
# ---------------------------------------------------------------------------- #
# Statistics 
# M0 same covariates as UK Biobank: CVD
M0 = compare_models_123(data=W, y_str="y_cvd", time_str="y_cvd_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                        cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                        folloup=quantile(W$y_cvd_time,probs=0.9), precision = 0.01,  do_sens=TRUE, round=4)

# M1 same covariates as UK Biobank: mortality 
M1 = compare_models_123(data=W1, y_str="y_m", time_str="y_m_time", cov_str1=cov0_acc0, cov_str2=cov0_acc1, cov_str3=cov0_acc2, 
                        cov_list1=cov0_Acc0, cov_list2=cov0_Acc1, cov_list3=cov0_Acc2, 
                        folloup=quantile(W1$y_m_time,probs=0.9), precision = 0.01,  do_sens=TRUE, round=4)


# M0 same covariates as UK Biobank: CVD
round(M0,3)
# M1 same covariates as UK Biobank: mortality 
round(M1,3)


setwd(dir = dir_res)
write.csv(M0, file = "m0_wii.csv", row.names = TRUE)
write.csv(M1, file = "m1_wii.csv", row.names = TRUE)
write.csv(M2, file = "m2_wii.csv", row.names = TRUE)
write.csv(M3, file = "m3_wii.csv", row.names = TRUE)






