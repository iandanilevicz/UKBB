# ---------------------------------------------------------------------------- #
# Script : 2_2_EDA_WII.R
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
library(arsenal) 

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
# Folders
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
X$cov2_dep     = as.factor(X$cov1_ghq) 
X$cov3_ptau     = log(X$fptau) 
cutpoint1 = log(0.4) # Ashton NJ, Brum WS, Di Molfetta G, et al. Diagnostic Accuracy of a Plasma Phosphorylated Tau 217 Immunoassay for Alzheimer Disease Pathology. JAMA Neurol. 2024;81(3):255â263. doi:10.1001/jamaneurol.2023.5319
cutpoint2 = log(0.63)
X$cov3_ptau_cat = ifelse(X$cov3_ptau  < cutpoint1, 0, 1)
X$cov3_ptau_cat = as.factor(X$cov3_ptau_cat) 
X$y_time = X$y_time/365.25
X$cov4_apoec = as.factor(ifelse(X$cov4_apoe>0,1,0))

vars2 = c("cov2_dep", "cov2_mmse", "fptau")
X_clean2 = X[complete.cases(X[, vars2]), ]

# ---------------------------------------------------------------------------- #
# average and SD

round(table(X$cov1_sex)/(length(X$cov1_sex) ),3)
mean(X$cov1_age)
sd(X$cov1_age)
round(mean(X$y_time),2)
round(sd(X$y_time),2)
round(median(X$y_time),2)
round(IQR(X$y_time),2)
# ---------------------------------------------------------------------------- #
# Model 1  
control_ttest <- tableby.control(
  total = FALSE,
  digits = 1, 
  digits.p = 3,
  numeric.simplify = TRUE,
  cat.simplify = TRUE,
  numeric.stats = c("Nmiss", "meansd"),
  test = TRUE,
  test.numeric = "t.test"   
)

table0 = tableby(y ~ y_time, 
                 data=X, control=tableby.control(total=FALSE,
                                                 digits=1, digits.p = 3,
                                                 numeric.simplify = TRUE,
                                                 cat.simplify = TRUE,
                                                 numeric.stats = c("Nmiss", "medianq1q3")))


table1 = tableby(y ~ y_time + cov1_age + cov1_sex +  cov1_edu + cov1_marcoh + cov1_work + 
                           cov1_smoke + cov1_veg + cov1_alcohol + cov1_bmi + cov1_hypt +
                           cov1_diab + cov1_hypl + cov1_Nchronic + cov1_cns, 
                         data=X, control=control_ttest)



table2 = tableby(y ~ cov2_dep + cov2_mmse + fptau, 
                 data=X, control=control_ttest)

table2_clean = tableby(y ~ cov2_dep + cov2_mmse + fptau, 
                 data=X_clean2, control=control_ttest)


table3 = tableby(y ~ cov4_apoec, 
                 data=X, control=control_ttest)

table4 = tableby(y ~ cov1_sex + cov3_ptau_cat, 
                 data=X, control=control_ttest)

table4_clean = tableby(y ~ cov1_sex + cov3_ptau_cat, 
                 data=X_clean2, control=control_ttest)


labels_cov0 = c(y_time = "Follow-up (years), median (IQR)")

labels_cov1 = c(y_time = "Follow-up (years), mean (SD)",
               cov1_age = "Age (years), mean (SD)", 
               cov1_sex = "Sex (women)", 
               cov1_edu  = "Education",
               cov1_marcoh = "Cohabitation status(married/cohab.)", 
               cov1_work = "Work status (working)", 
               cov1_smoke = "Smoking status", 
               cov1_veg = "Fruit and vegetable intake", 
               cov1_alcohol = "Alcohol intake",  
               cov1_bmic = "Body mass index (BMI)", 
               cov1_hypt = "Prevalent hypertension",  
               cov1_diab = "Prevalent diabetes",
               cov1_hypl = "Prevalent hyperlipidemia",
               cov1_ncdc = "Number of chronic diseases, mean (SD)",
               cov1_cns= "CNS drug use") 

labels_cov2 = c(cov2_dep  = "Depressive symptoms",
                cov2_mmse = "MMSE",
                fptau     = "P-tau 217" ) 

labels_cov3 = c( cov4_apoec = "APOE E4")

labels_cov4 = c(cov1_sex = "Sex (women)", cov3_ptau_cat = "p-tau 217 (medium/high )")
table(X$cov3_ptau_cat)

# ---------------------------------------------------------------------------- #
# Tables 
summary(table0, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov0)

summary(table1, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov1)

summary(table2, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov2)

# only for p-tau progressive clean 
summary(table2_clean, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov2)

summary(table3, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov3)
summary(table4, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov4)
summary(table4_clean, text = TRUE, pfootnote = TRUE, 
        title = "Covariates by dementia status in WII accelerometer sub-study", labelTranslations = labels_cov4)
# ---------------------------------------------------------------------------- #
# follow up
quantile(X$y_time, 0.90)
quantile(X$y_time, 0.75)
quantile(X$y_time, 0.5)

time_case = ifelse(X$y == 1, X$y_time, NA)
quantile(time_case, 0.90, na.rm=TRUE)
quantile(time_case, 0.75, na.rm=TRUE)
quantile(time_case, 0.5, na.rm=TRUE)
# ---------------------------------------------------------------------------- #
# End 
# Close with security 
setwd("C:/Users/i_danilevicz/Documents")
rm(list=ls())
# ---------------------------------------------------------------------------- #





