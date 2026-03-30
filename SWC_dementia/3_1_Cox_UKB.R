# ---------------------------------------------------------------------------- #
# Script : 3_1_Cox_UKB.R
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
T   = dat0$y_time 
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

cov0_acc0 = "cov1_age + cov1_sex +  cov1_edu + cov1_marcoh +
cov1_work + cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmic + cov1_hypt + cov1_diab +
cov1_hypl + cov1_ncdc + cov1_cns"

mod_acc1_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(ct_acrotime)+", cov0_acc0)), data = X)
mod_acc2_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(ct_l5time)+", cov0_acc0)), data = X)
mod_acc3_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(ct_m10time)+", cov0_acc0)), data = X)
mod_acc4_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(ct_onset)+", cov0_acc0)), data = X)
mod_acc5_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(ct_wakeup)+", cov0_acc0)), data = X)
mod_acc6_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_acc_day)+", cov0_acc0)), data = X)
mod_acc7_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_dur_sb)+", cov0_acc0)), data = X)
mod_acc8_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_dur_lipa)+", cov0_acc0)), data = X)
mod_acc9_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_dur_mvpa)+", cov0_acc0)), data = X)
mod_acc10_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_nb_sb)+", cov0_acc0)), data = X)
mod_acc11_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_nb_lipa)+", cov0_acc0)), data = X)
mod_acc12_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_nb_mvpa)+", cov0_acc0)), data = X)
mod_acc13_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_mdb_sb)+", cov0_acc0)), data = X)
mod_acc14_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_mdb_lipa)+", cov0_acc0)), data = X)
mod_acc15_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_mdb_mvpa)+", cov0_acc0)), data = X)
mod_acc16_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_ig_inter)+", cov0_acc0)), data = X)
mod_acc17_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_ig_slope)+", cov0_acc0)), data = X)
mod_acc18_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_m10value)+", cov0_acc0)), data = X)
mod_acc19_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_TP_ar)+", cov0_acc0)), data = X)
mod_acc20_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(mb_TP_ra)+", cov0_acc0)), data = X)
mod_acc21_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_acc_night)+", cov0_acc0)), data = X)
mod_acc22_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_dur_sleep)+", cov0_acc0)), data = X)
mod_acc23_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(scale(spt_dur_sleep)^2)+", cov0_acc0)), data = X)
mod_acc24_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_nb_sleep)+", cov0_acc0)), data = X)
mod_acc25_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_mdb_sleep)+", cov0_acc0)), data = X)
mod_acc26_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_mdb_wake)+", cov0_acc0)), data = X)
mod_acc27_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_waso)+", cov0_acc0)), data = X)
mod_acc28_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_eff)+", cov0_acc0)), data = X)
mod_acc29_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_l5value)+", cov0_acc0)), data = X)
mod_acc30_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_TP_sw)+", cov0_acc0)), data = X)
mod_acc31_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(spt_TP_ws)+", cov0_acc0)), data = X)
mod_acc32_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(rar_amp)+", cov0_acc0)), data = X)
mod_acc33_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(rar_is)+", cov0_acc0)), data = X)
mod_acc34_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(rar_iv)+", cov0_acc0)), data = X)
mod_acc35_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(rar_mesor)+", cov0_acc0)), data = X)
mod_acc36_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(rar_r2)+", cov0_acc0)), data = X)
mod_acc37_cov0 = coxph(as.formula(paste0("Surv(y_time, y) ~ scale(rar_ra)+", cov0_acc0)), data = X)

m1 = extract_HR_1(mod_acc1_cov0, cov="Acrotime") 
m2 = extract_HR_1(mod_acc2_cov0, cov="L5 (time)") 
m3 = extract_HR_1(mod_acc3_cov0, cov="M10 (time)")
m4 = extract_HR_1(mod_acc4_cov0, cov="Onset")
m5 = extract_HR_1(mod_acc5_cov0, cov="Wake-up") 
m6 = extract_HR_1(mod_acc6_cov0, cov="Daily acceleration") 
m7 = extract_HR_1(mod_acc7_cov0, cov="SB (duration)") 
m8 = extract_HR_1(mod_acc8_cov0, cov="LIPA (duration)") 
m9 = extract_HR_1(mod_acc9_cov0, cov="MVPA (duration)") 
m10 = extract_HR_1(mod_acc10_cov0, cov="SB (n bouts)") 
m11 = extract_HR_1(mod_acc11_cov0, cov="LIPA (n bouts)") 
m12 = extract_HR_1(mod_acc12_cov0, cov="MVPA (n bouts)") 
m13 = extract_HR_1(mod_acc13_cov0, cov="SB (mean duration of bouts)") 
m14 = extract_HR_1(mod_acc14_cov0, cov="LIPA (mean duration of bouts)") 
m15 = extract_HR_1(mod_acc15_cov0, cov="MVPA (mean duration of bouts)") 
m16 = extract_HR_1(mod_acc16_cov0, cov="IG (intercept)") 
m17 = extract_HR_1(mod_acc17_cov0, cov="IG (slope)") 
m18 = extract_HR_1(mod_acc18_cov0, cov="M10 (value)") 
m19 = extract_HR_1(mod_acc19_cov0, cov="TP (activity to rest)") 
m20 = extract_HR_1(mod_acc20_cov0, cov="TP (rest to activity)") 
m21 = extract_HR_1(mod_acc21_cov0, cov="Nigth acceleration") 
m22 = extract_HR_1(mod_acc22_cov0, cov="Sleep (duration)")
m23 = extract_HR_1(mod_acc23_cov0, cov="Sleep (squared duration)")
m24 = extract_HR_1(mod_acc24_cov0, cov="Sleep (n bouts)") 
m25 = extract_HR_1(mod_acc25_cov0, cov="Sleep (mean duration of bouts)") 
m26 = extract_HR_1(mod_acc26_cov0, cov="Wake (mean duration of bouts)") 
m27 = extract_HR_1(mod_acc27_cov0, cov="WASO") 
m28 = extract_HR_1(mod_acc28_cov0, cov="Sleep efficiency") 
m29 = extract_HR_1(mod_acc29_cov0, cov="L5 (value)") 
m30 = extract_HR_1(mod_acc30_cov0, cov="TP (sleep to wake)") 
m31 = extract_HR_1(mod_acc31_cov0, cov="TP (wake to sleep)") 
m32 = extract_HR_1(mod_acc32_cov0, cov="Amplitude") 
m33 = extract_HR_1(mod_acc33_cov0, cov="IS") 
m34 = extract_HR_1(mod_acc34_cov0, cov="IV") 
m35 = extract_HR_1(mod_acc35_cov0, cov="Mesor") 
m36 = extract_HR_1(mod_acc36_cov0, cov="R2") 
m37 = extract_HR_1(mod_acc37_cov0, cov="Relative amplitude") 


# ---------------------------------------------------------------------------- #
# Individual association (check U-shape)
mi_1 = coxph(Surv(y_time, y) ~ pspline(rar_amp), data = X,x=TRUE)
mi_2 = coxph(Surv(y_time, y) ~ pspline(rar_is), data = X,x=TRUE)
mi_3 = coxph(Surv(y_time, y) ~ pspline(rar_iv), data = X ,x=TRUE)
mi_4 = coxph(Surv(y_time, y) ~ pspline(rar_mesor), data = X ,x=TRUE)
mi_5 = coxph(Surv(y_time, y) ~ pspline(rar_r2), data = X ,x=TRUE)
mi_6 = coxph(Surv(y_time, y) ~ pspline(rar_ra), data = X ,x=TRUE)
mi_7 = coxph(Surv(y_time, y) ~ pspline(mb_acc_day), data = X,x=TRUE)
mi_8 = coxph(Surv(y_time, y) ~ pspline(mb_dur_lipa), data = X ,x=TRUE)
mi_9 = coxph(Surv(y_time, y) ~ pspline(mb_dur_mvpa), data = X ,x=TRUE)
mi_10 = coxph(Surv(y_time, y) ~ pspline(mb_dur_sb), data = X ,x=TRUE)
mi_11 = coxph(Surv(y_time, y) ~ pspline(mb_ig_inter), data = X ,x=TRUE)
mi_12 = coxph(Surv(y_time, y) ~ pspline(mb_ig_slope), data = X ,x=TRUE)
mi_13 = coxph(Surv(y_time, y) ~ pspline(mb_m10value), data = X ,x=TRUE)
mi_14 = coxph(Surv(y_time, y) ~ pspline(mb_mdb_lipa), data = X ,x=TRUE)
mi_15 = coxph(Surv(y_time, y) ~ pspline(mb_mdb_mvpa), data = X ,x=TRUE)
mi_16 = coxph(Surv(y_time, y) ~ pspline(mb_mdb_sb), data = X ,x=TRUE) 
mi_17 = coxph(Surv(y_time, y) ~ pspline(mb_nb_lipa), data = X ,x=TRUE)
mi_18 = coxph(Surv(y_time, y) ~ pspline(mb_nb_mvpa), data = X ,x=TRUE)
mi_19 = coxph(Surv(y_time, y) ~ pspline(mb_nb_sb), data = X ,x=TRUE)
mi_20 = coxph(Surv(y_time, y) ~ pspline(mb_TP_ar), data = X ,x=TRUE)
mi_21 = coxph(Surv(y_time, y) ~ pspline(mb_TP_ra), data = X ,x=TRUE)
mi_22 = coxph(Surv(y_time, y) ~ pspline(ct_acrotime), data = X ,x=TRUE)
mi_23 = coxph(Surv(y_time, y) ~ pspline(ct_l5time), data = X ,x=TRUE)
mi_24 = coxph(Surv(y_time, y) ~ pspline(ct_m10time), data = X ,x=TRUE)
mi_25 = coxph(Surv(y_time, y) ~ pspline(ct_onset), data = X ,x=TRUE)
mi_26 = coxph(Surv(y_time, y) ~ pspline(ct_wakeup), data = X ,x=TRUE)
mi_27 = coxph(Surv(y_time, y) ~ pspline(spt_acc_night), data = X,x=TRUE)
mi_28 = coxph(Surv(y_time, y) ~ pspline(spt_dur_sleep), data = X,x=TRUE)
mi_29 = coxph(Surv(y_time, y) ~ pspline(spt_eff), data = X,x=TRUE)
mi_30 = coxph(Surv(y_time, y) ~ pspline(spt_l5value), data = X,x=TRUE)
mi_31 = coxph(Surv(y_time, y) ~ pspline(spt_mdb_sleep), data = X,x=TRUE)
mi_32 = coxph(Surv(y_time, y) ~ pspline(spt_mdb_wake), data = X,x=TRUE)
mi_33 = coxph(Surv(y_time, y) ~ pspline(spt_nb_sleep), data = X,x=TRUE)
mi_34 = coxph(Surv(y_time, y) ~ pspline(spt_TP_sw), data = X,x=TRUE)
mi_35 = coxph(Surv(y_time, y) ~ pspline(spt_TP_ws), data = X,x=TRUE)
mi_36 = coxph(Surv(y_time, y) ~ pspline(spt_waso), data = X,x=TRUE)

hr_mi_1 = smoothHR(data = X, coxfit = mi_1) 
hr_mi_2 = smoothHR(data = X, coxfit = mi_2) 
hr_mi_3 = smoothHR(data = X, coxfit = mi_3) 
hr_mi_4 = smoothHR(data = X, coxfit = mi_4) 
hr_mi_5 = smoothHR(data = X, coxfit = mi_5) 
hr_mi_6 = smoothHR(data = X, coxfit = mi_6) 
hr_mi_7 = smoothHR(data = X, coxfit = mi_7) 
hr_mi_8 = smoothHR(data = X, coxfit = mi_8) 
hr_mi_9 = smoothHR(data = X, coxfit = mi_9) 
hr_mi_10 = smoothHR(data = X, coxfit = mi_10) 
hr_mi_11 = smoothHR(data = X, coxfit = mi_11) 
hr_mi_12 = smoothHR(data = X, coxfit = mi_12) 
hr_mi_13 = smoothHR(data = X, coxfit = mi_13) 
hr_mi_14 = smoothHR(data = X, coxfit = mi_14) 
hr_mi_15 = smoothHR(data = X, coxfit = mi_15) 
hr_mi_16 = smoothHR(data = X, coxfit = mi_16) 
hr_mi_17 = smoothHR(data = X, coxfit = mi_17) 
hr_mi_18 = smoothHR(data = X, coxfit = mi_18) 
hr_mi_19 = smoothHR(data = X, coxfit = mi_19) 
hr_mi_20 = smoothHR(data = X, coxfit = mi_20) 
hr_mi_21 = smoothHR(data = X, coxfit = mi_21) 
hr_mi_22 = smoothHR(data = X, coxfit = mi_22) 
hr_mi_23 = smoothHR(data = X, coxfit = mi_23) 
hr_mi_24 = smoothHR(data = X, coxfit = mi_24) 
hr_mi_25 = smoothHR(data = X, coxfit = mi_25) 
hr_mi_26 = smoothHR(data = X, coxfit = mi_26) 
hr_mi_27 = smoothHR(data = X, coxfit = mi_27) 
hr_mi_28 = smoothHR(data = X, coxfit = mi_28) 
hr_mi_29 = smoothHR(data = X, coxfit = mi_29) 
hr_mi_30 = smoothHR(data = X, coxfit = mi_30) 
hr_mi_31 = smoothHR(data = X, coxfit = mi_31) 
hr_mi_32 = smoothHR(data = X, coxfit = mi_32) 
hr_mi_33 = smoothHR(data = X, coxfit = mi_33) 
hr_mi_34 = smoothHR(data = X, coxfit = mi_34) 
hr_mi_35 = smoothHR(data = X, coxfit = mi_35) 
hr_mi_36 = smoothHR(data = X, coxfit = mi_36) 


par(mfrow = c(1, 1))

# RAR
plot_hr(hr_model = hr_mi_1, model = mi_1, predictor = "rar_amp",
        xlab = "Cosinor amplitude", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_2, model = mi_2, predictor = "rar_is",
        xlab = "Interdaily stability (IS)", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_3, model = mi_3, predictor = "rar_iv",
        xlab = "Intradaily variability (IV)", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_4, model = mi_4, predictor = "rar_mesor",
        xlab = "Cosinor mesor", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_5, model = mi_5, predictor = "rar_r2",
        xlab = "Cosinor R2", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_6, model = mi_6, predictor = "rar_ra",
        xlab = "Relative Amplitude", main = "Smooth HR", xlim = "core")

# activity day
plot_hr(hr_model = hr_mi_7,  model = mi_7,  predictor = "mb_acc_day",
        xlab = "Acceleration during waking", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_8,  model = mi_8,  predictor = "mb_dur_lipa",
        xlab = "LIPA duration", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_9,  model = mi_9,  predictor = "mb_dur_mvpa",
        xlab = "MVPA duration", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_10, model = mi_10, predictor = "mb_dur_sb",
        xlab = "SB duration", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_11, model = mi_11, predictor = "mb_ig_inter",
        xlab = "Intensity gradient (intercept)", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_12, model = mi_12, predictor = "mb_ig_slope",
        xlab = "Intensity gradient (slope)", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_13, model = mi_13, predictor = "mb_m10value",
        xlab = "M10 mean acceleration", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_14, model = mi_14, predictor = "mb_mdb_lipa",
        xlab = "Mean duration of LIPA bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_15, model = mi_15, predictor = "mb_mdb_mvpa",
        xlab = "Mean duration of MVPA bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_16, model = mi_16, predictor = "mb_mdb_sb",
        xlab = "Mean duration of SB bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_17, model = mi_17, predictor = "mb_nb_lipa",
        xlab = "Number of LIPA bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_18, model = mi_18, predictor = "mb_nb_mvpa",
        xlab = "Number of MVPA bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_19, model = mi_19, predictor = "mb_nb_sb",
        xlab = "Number of SB bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_20, model = mi_20, predictor = "mb_TP_ar",
        xlab = "Transition probability from activity to rest during the day (TP ar,d)",
        main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_21, model = mi_21, predictor = "mb_TP_ra",
        xlab = "Transition probability rest to activity during the day (TP ra,d)",
        main = "Smooth HR", xlim = "core")
# chronotype
plot_hr(hr_model = hr_mi_22, model = mi_22, predictor = "ct_acrotime",
        xlab = "Cosinor acrotime", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_23, model = mi_23, predictor = "ct_l5time",
        xlab = "L5 start", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_24, model = mi_24, predictor = "ct_m10time",
        xlab = "M10 start", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_25, model = mi_25, predictor = "ct_onset",
        xlab = "Sleep onset", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_26, model = mi_26, predictor = "ct_wakeup",
        xlab = "Waking time", main = "Smooth HR", xlim = "core")

# sleep
plot_hr(hr_model = hr_mi_27, model = mi_27, predictor = "spt_acc_night",
        xlab = "Mean acceleration during sleep period", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_28, model = mi_28, predictor = "spt_dur_sleep",
        xlab = "Sleep duration", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_29, model = mi_29, predictor = "spt_eff",
        xlab = "Sleep efficiency", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_30, model = mi_30, predictor = "spt_l5value",
        xlab = "L5 mean acceleration", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_31, model = mi_31, predictor = "spt_mdb_sleep",
        xlab = "Mean duration of sleep bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_32, model = mi_32, predictor = "spt_mdb_wake",
        xlab = "Mean duration of wake bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_33, model = mi_33, predictor = "spt_nb_sleep",
        xlab = "Number of sleep bouts", main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_34, model = mi_34, predictor = "spt_TP_sw",
        xlab = "Transition probabilities from sleep to wake during the night (TP sw,n)",
        main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_35, model = mi_35, predictor = "spt_TP_ws",
        xlab = "Transition probabilities from wake to sleep during the night (TP ws,n)",
        main = "Smooth HR", xlim = "core")
plot_hr(hr_model = hr_mi_36, model = mi_36, predictor = "spt_waso",
        xlab = "WASO", main = "Smooth HR", xlim = "core")

# squared effect 
par(mfrow = c(2, 2))
plot_hr(hr_model=hr_mi_8, model=mi_8, predictor="mb_dur_lipa", xlab="LIPA duration, min", main = "A.", xlim = "core", adj=0)
plot_hr(hr_model=hr_mi_14, model=mi_14, predictor="mb_mdb_lipa", xlab="Mean duration of LIPA bouts, min", main = "B.", xlim = "core", adj=0)
plot_hr(hr_model=hr_mi_15, model=mi_15, predictor="mb_mdb_mvpa", xlab="Mean duration of MVPA bouts, min", main = "C.", xlim = "core", adj=0)
plot_hr(hr_model=hr_mi_28, model=mi_28, predictor="spt_dur_sleep", xlab="Sleep duration, min", main = "D.", xlim = "core", adj=0)




