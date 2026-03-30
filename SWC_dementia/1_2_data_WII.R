# ---------------------------------------------------------------------------- #
# Script : 1_2_data_WII.R
# Author : I M Danilevicz 
# Remark : Part of the work available in:
#          C Cavailles, IM Danilevicz, S Vidil et al (2026). 
#          "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults", (under review)
# ---------------------------------------------------------------------------- #

# Set local language as english to be able to read dates properly
# Clear the Environment 
rm(list=ls())
Sys.setlocale("LC_ALL","English")

# ---------------------------------------------------------------------------- #
# Packages 

# ---------------------------------------------------------------------------- #
# Folders

dir_3wii   = "D:/..."
dir_w_NW   = "D:/..."
dir_wo_NW  = "D:/..."
dir_azt1   = "D:/..."
dir_save   = "D:/..."
dir_ad     = "D:/..."
dir_2wii   = "D:/..."
dir_1wii   = "D:/..."
# ---------------------------------------------------------------------------- #
# Data
# Data Accelerometer 
# Set of participants with valid accelerometer data

setwd(dir = dir_3wii)
pop_target_ok  = read_dta("pop_target_ok.dta")
sample_stno = pop_target_ok %>% select(stno)

# ---------------------------------------------------------------------------- #
# Data
# Data Accelerometer part 5
# ---------------------------------------------------------------------------- #
# Include accelerometer features IAN
setwd(dir = dir_azt1)

tab_p5 = read.csv("part5_personsummary_WW_L40M100V400_T5A5.csv")
tab_p5$stno = as.numeric(gsub("[^0-9.-]", "", tab_p5$ID))
n = dim(tab_p5)[1]

# ---------------------------------------------------------------------------- #
# Data
# Data screen and quest 
# Phase 11 
setwd(dir = dir_3wii)

tab_s11 = read_dta("s11quest.dta") %>%
  left_join(read_dta("S11screen.dta"),
            by = "stno") %>% 
  filter(stno %in% unique(sample_stno$stno))


tab_s11$Hyperlipid = ifelse(tab_s11$flipdrg == 1 | tab_s11$fldl > 4.1, 1,0)

# ---------------------------------------------------------------------------- #
# Phase 9
setwd(dir = dir_2wii)

tab_s9 = read_dta("s9quest.dta") %>% left_join(read_dta("s9screen.dta"),
          by = "stno") %>% filter(stno %in% unique(sample_stno$stno))


tab_s9$jHyperlipid = ifelse(tab_s9$jlipdrg == 1 | tab_s9$jldl > 4.1, 1,0)

# ---------------------------------------------------------------------------- #
# Phase 7
setwd(dir = dir_2wii)
tab_s7 = read_dta("s7quest.dta") %>% left_join(read_dta("s7screen.dta"),
                                               by = "stno") %>% filter(stno %in% unique(sample_stno$stno))
tab_s7$mHyperlipid = ifelse(tab_s7$mlipdrg == 1 | tab_s7$mldl > 4.1, 1,0)


# ---------------------------------------------------------------------------- #
# Phase 5
setwd(dir = dir_2wii)
tab_s5 = read_dta("s5quest.dta") %>% left_join(read_dta("s5screen.dta"),
                                               by = "stno") %>% filter(stno %in% unique(sample_stno$stno))
tab_s5$tHyperlipid = ifelse(tab_s5$tlipdrg == 1 | tab_s5$tldl > 4.1, 1,0)

# ---------------------------------------------------------------------------- #
# Phase 3
setwd(dir = dir_2wii)
tab_s3 = read_dta("s3quest.dta") %>%
  filter(stno %in% unique(sample_stno$stno))

# ---------------------------------------------------------------------------- #
# Phase 8
setwd(dir = dir_2wii)
tab_s8 = read_dta("s8quest.dta") %>% filter(stno %in% unique(sample_stno$stno))

# ---------------------------------------------------------------------------- #
# Data
# Data chronic disease
setwd(dir = dir_1wii)
tab_cd <- read_dta(file = "chronic_disease.dta") %>% 
  filter(stno %in% unique(sample_stno$stno)) 
tab_cd$Nchronic = tab_cd$CHD_p + tab_cd$STROKE_p + tab_cd$PARK_p + tab_cd$COPD_p + 
  tab_cd$DEPRESSION_p + tab_cd$HF_p + tab_cd$ARTH_p + tab_cd$MD_p + tab_cd$CANCER_p + tab_cd$LIVER_p  
tab_cd$CVD_p = tab_cd$CHD_p + tab_cd$STROKE_p + tab_cd$AF_p 


# ---------------------------------------------------------------------------- #
# Data
# Data Basic covariates 
setwd(dir_2wii)

tab_basics <- read_dta(file = "basics.dta") %>% 
  filter(stno %in% unique(sample_stno$stno))

tab_edu <- read_dta(file = "education_new.dta") %>% 
  filter(stno %in% unique(sample_stno$stno))


setwd(dir_2wii)

tab_cov_imp <- read_dta(file = "s11_covariates_imputation.dta") %>% 
  filter(stno %in% unique(sample_stno$stno))


# ---------------------------------------------------------------------------- #
# Data 
# Data APOE 4  
setwd(dir = dir_2wii)
tab_apoe = read_dta(file = "genetics.dta") %>% 
  filter(stno %in% unique(sample_stno$stno))
tab_apoe$apoe = as.numeric(tab_apoe$apoe)
tab_apoe$apoe44 = ifelse(is.na(tab_apoe$apoe),NA,  
                         ifelse(tab_apoe$apoe %in% c(24,34,42,43), 1,
                                ifelse(tab_apoe$apoe ==44, 2, 0))) 

# ---------------------------------------------------------------------------- #
# Data
# Outcome: Dementia
setwd(dir = dir_3wii)

data_rayane = read_dta("rayane_whii_vars.dta")
tab_demence = data_rayane %>% filter(stno %in% unique(sample_stno$stno)) 
tab_demence = tab_demence %>% select(stno, demence, demence_date)
tab_demence = tab_demence %>%
  left_join(tab_s11, by = "stno") %>% 
  filter(stno %in% unique(sample_stno$stno)) %>%
  select(stno, demence, demence_date, fdatscrn) 
tab_demence$y      = tab_demence$demence 
tab_demence$y_date = tab_demence$demence_date 
tab_demence$y_time = as.numeric(difftime(strptime(tab_demence$y_date, format = "%Y-%m-%d"),
         strptime(tab_demence$fdatscrn, format = "%Y-%m-%d"),units="days"))
tab_demence$y_prev = ifelse(tab_demence$y_time<=0,1,0) 

# ---------------------------------------------------------------------------- #
# Data
# Outcome: mortality 

tab_mort = data_rayane %>% select(stno, stat0824_c, fup0824)
tab_mort$stat0824_c = ifelse(tab_mort$stat0824_c == 1, 0, ifelse(tab_mort$stat0824_c == 2, 1, 3))
tab_mort = tab_mort %>%
  left_join(tab_s11, by = "stno") %>% 
  filter(stno %in% unique(sample_stno$stno)) %>%
  select(stno, stat0824_c, fup0824, fdatscrn) 
tab_demence_temp = tab_demence %>% select(stno, demence_date)
tab_mort = merge(tab_mort, tab_demence_temp, by="stno")
tab_mort$y_m      = tab_mort$stat0824_c 
tab_mort$y_m_date = ifelse(tab_mort$stat0824_c==3, as.Date(tab_mort$demence_date,"%Y-%m-%d"), as.Date(tab_mort$fup0824,"%Y-%m-%d"))  
tab_mort$y_m      = ifelse(tab_mort$stat0824_c==3, 0, tab_mort$stat0824_c)  
tab_mort$y_m_time = as.numeric(
                      difftime(as.Date(tab_mort$y_m_date, origin = "1970-01-01"),
                               as.Date(tab_mort$fdatscrn, format = "%Y-%m-%d"),
                               units= "days"))
tab_mort$y_m_prev = ifelse(tab_mort$y_m_time<=0,1,0) 
tab_mort = tab_mort %>% select(stno, y_m, y_m_date, y_m_time, y_m_prev)
# ---------------------------------------------------------------------------- #
# Data
# Outcome: CVD 
followup_end = as.Date("2024-03-01", format = "%Y-%m-%d")

setwd(dir = dir_3wii)
tab_cvd = read_dta(file = "cvd_2024.dta")%>%
  select(stno, b_123y, b_456x, chd, stroke, hfailure, d_chd, d_stroke, d_hfailure)

tab_cvd = tab_cvd %>%
  left_join(tab_s11, by = "stno") %>% 
  filter(stno %in% unique(sample_stno$stno)) %>%
  select(stno, b_123y, b_456x, chd, stroke, hfailure, d_chd, d_stroke, d_hfailure, fdatscrn) 
tab_cvd$cvd = ifelse(tab_cvd$chd>=1 | tab_cvd$stroke>=1 | tab_cvd$hfailure>=1 | 
                       tab_cvd$b_123y==1 | tab_cvd$b_456x==1, 1, 0)

vector = tab_cvd %>% 
  transmute(earliest_date = pmin(d_chd, d_stroke, d_hfailure, na.rm = TRUE))
tab_cvd$fue        = rep(followup_end, dim(tab_cvd)[1]) 
tab_cvd$d_cvd      = vector$earliest_date
tab_cvd$y_cvd      = ifelse(is.na(tab_cvd$cvd),0,tab_cvd$cvd)
tab_cvd$y_cvd_date = as.Date(ifelse(is.na(tab_cvd$d_cvd), tab_cvd$fue, as.Date(tab_cvd$d_cvd, format = "%Y-%m-%d")) , format = "%Y-%m-%d")

tab_cvd$y_cvd_time = as.numeric(difftime(strptime(tab_cvd$y_cvd_date, format = "%Y-%m-%d"),
                                        strptime(tab_cvd$fdatscrn, format = "%Y-%m-%d"),units="days"))
tab_cvd$y_cvd_prev = ifelse(tab_cvd$y_cvd_time<=0 | tab_cvd$b_123y==1 | tab_cvd$b_456x==1,1,0)
tab_cvd$y_cvd_prev = ifelse(is.na(tab_cvd$y_cvd_prev),0, tab_cvd$y_cvd_prev )
tab_cvd = tab_cvd %>% select(stno, y_cvd, y_cvd_date, y_cvd_time, y_cvd_prev)
# ---------------------------------------------------------------------------- #
# Build Main Dataset

tab_cov               = tab_s11 %>% select(stno, fage_s, fmarcoh, flabstat, ffruitvg, funitwk0, fbmi, Hyperlipid, fcnsdrg, fghqgp, fmm_scor)
tab_temp              = tab_basics %>% select(stno, sex, ethn_ds)
tab_cov               = merge(tab_cov, tab_temp, by.x = "stno", by.y = "stno")
tab_temp              = tab_edu %>% select(stno, edu_imp) 
tab_cov               = merge(tab_cov, tab_temp, by.x = "stno", by.y = "stno")
tab_temp              = tab_cov_imp %>% select(stno, fesmoke_i)
tab_cov               = merge(tab_cov, tab_temp, by.x = "stno", by.y = "stno")
tab_temp              = tab_cd %>% select(stno, HYPTEN_p, DIABETES_p, Nchronic, DEPRESSION_p, CVD_p)
tab_cov               = merge(tab_cov, tab_temp, by.x = "stno", by.y = "stno")
tab_temp              = tab_apoe %>% select(stno, apoe44)
tab_cov               = merge(tab_cov, tab_temp, by.x = "stno", by.y = "stno")
colnames(tab_cov) = c("stno", 
                      "cov1_age", "cov1_marcoh", "cov1_work", "cov1_veg", "cov1_alcohol", "cov1_bmi", 
                      "cov1_hypl", "cov1_cns", "cov1_ghq", "cov2_mmse", 
                      "cov1_sex", "cov1_ethn", "cov1_edu", "cov1_smoke", 
                      "cov1_hypt", "cov1_diab", "cov1_Nchronic", 
                      "cov3_dep", "cov3_cvd", "cov4_apoe")

# ---------------------------------------------------------------------------- #
# Imputation: screen 9, 7, ... 

tab_temp1 = tab_cov %>% select(stno, cov1_marcoh)
tab_temp2 = tab_s9 %>% select(stno, jmarcoh)
tab_temp3 = tab_s7 %>% select(stno, mmarcoh)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
tab_cov$cov1_marcoh = ifelse(is.na(tab_cov$cov1_marcoh), gathering$jmarcoh, tab_cov$cov1_marcoh)  
tab_cov$cov1_marcoh = ifelse(is.na(tab_cov$cov1_marcoh), gathering$mmarcoh, tab_cov$cov1_marcoh)  

tab_temp1 = tab_cov %>% select(stno, cov1_work)
tab_temp2 = tab_s9 %>% select(stno, jlabstat)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
tab_cov$cov1_work = ifelse(is.na(tab_cov$cov1_work), gathering$jlabstat, tab_cov$cov1_work)  

tab_temp1 = tab_cov %>% select(stno, cov1_veg)
tab_temp2 = tab_s9 %>% select(stno, jfruitvg)
tab_temp3 = tab_s7 %>% select(stno, mfruitvg)
tab_temp4 = tab_s5 %>% select(stno, tfruitvg)
tab_temp5 = tab_s3 %>% select(stno, xfruitvg)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp4, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp5, by.x = "stno", by.y = "stno")
tab_cov$cov1_veg = ifelse(is.na(tab_cov$cov1_veg), gathering$jfruitvg, tab_cov$cov1_veg)  
tab_cov$cov1_veg = ifelse(is.na(tab_cov$cov1_veg), gathering$mfruitvg, tab_cov$cov1_veg)  
tab_cov$cov1_veg = ifelse(is.na(tab_cov$cov1_veg), gathering$tfruitvg, tab_cov$cov1_veg)  
tab_cov$cov1_veg = ifelse(is.na(tab_cov$cov1_veg), gathering$xfruitvg, tab_cov$cov1_veg)  

tab_temp1 = tab_cov %>% select(stno, cov1_alcohol)
tab_temp2 = tab_s9 %>% select(stno, junitwk0)
tab_temp3 = tab_s7 %>% select(stno, munitwk0)
tab_temp4 = tab_s5 %>% select(stno, tunitwk0)
tab_temp5 = tab_s3 %>% select(stno, xunitwk0)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp4, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp5, by.x = "stno", by.y = "stno")
tab_cov$cov1_alcohol = ifelse(is.na(tab_cov$cov1_alcohol), gathering$junitwk0, tab_cov$cov1_alcohol)  
tab_cov$cov1_alcohol = ifelse(is.na(tab_cov$cov1_alcohol), gathering$munitwk0, tab_cov$cov1_alcohol)  
tab_cov$cov1_alcohol = ifelse(is.na(tab_cov$cov1_alcohol), gathering$tunitwk0, tab_cov$cov1_alcohol)  
tab_cov$cov1_alcohol = ifelse(is.na(tab_cov$cov1_alcohol), gathering$xunitwk0, tab_cov$cov1_alcohol)  

tab_temp1 = tab_cov %>% select(stno, cov1_bmi)
tab_temp2 = tab_s9 %>% select(stno, jbmi)
tab_temp3 = tab_s7 %>% select(stno, mbmi)
tab_temp4 = tab_s5 %>% select(stno, tbmi)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp4, by.x = "stno", by.y = "stno")
tab_cov$cov1_bmi = ifelse(is.na(tab_cov$cov1_bmi), gathering$jbmi, tab_cov$cov1_bmi)  
tab_cov$cov1_bmi = ifelse(is.na(tab_cov$cov1_bmi), gathering$mbmi, tab_cov$cov1_bmi)  
tab_cov$cov1_bmi = ifelse(is.na(tab_cov$cov1_bmi), gathering$tbmi, tab_cov$cov1_bmi)  
  
tab_temp1 = tab_cov %>% select(stno, cov1_hypl)
tab_temp2 = tab_s9 %>% select(stno, jHyperlipid)
tab_temp3 = tab_s7 %>% select(stno, mHyperlipid)
tab_temp4 = tab_s5 %>% select(stno, tHyperlipid)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp4, by.x = "stno", by.y = "stno")
tab_cov$cov1_hypl = ifelse(is.na(tab_cov$cov1_hypl), gathering$jHyperlipid, tab_cov$cov1_hypl)  
tab_cov$cov1_hypl = ifelse(is.na(tab_cov$cov1_hypl), gathering$mHyperlipid, tab_cov$cov1_hypl)  
tab_cov$cov1_hypl = ifelse(is.na(tab_cov$cov1_hypl), gathering$tHyperlipid, tab_cov$cov1_hypl)  


gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")


tab_temp1 = tab_cov %>% select(stno, cov1_ghq)
tab_temp2 = tab_s9 %>% select(stno, jghqgp)
tab_temp3 = tab_s7 %>% select(stno, mghqgp)
tab_temp4 = tab_s5 %>% select(stno, tghqgp)
tab_temp5 = tab_s3 %>% select(stno, xghqgp)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp4, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp5, by.x = "stno", by.y = "stno")
tab_cov$cov1_ghq = ifelse(is.na(tab_cov$cov1_ghq), gathering$jghqgp, tab_cov$cov1_ghq)  
tab_cov$cov1_ghq = ifelse(is.na(tab_cov$cov1_ghq), gathering$mghqgp, tab_cov$cov1_ghq)  
tab_cov$cov1_ghq = ifelse(is.na(tab_cov$cov1_ghq), gathering$tghqgp, tab_cov$cov1_ghq)  
tab_cov$cov1_ghq = ifelse(is.na(tab_cov$cov1_ghq), gathering$xghqgp, tab_cov$cov1_ghq) 

tab_temp1 = tab_cov %>% select(stno, cov2_mmse)
tab_temp2 = tab_s9 %>% select(stno, jmm_scor)
tab_temp3 = tab_s7 %>% select(stno, mmm_scor)
tab_temp4 = tab_s5 %>% select(stno, tmm_scor)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp3, by.x = "stno", by.y = "stno")
gathering = merge(gathering, tab_temp4, by.x = "stno", by.y = "stno")
tab_cov$cov2_mmse = ifelse(is.na(tab_cov$cov2_mmse), gathering$jmm_scor, tab_cov$cov2_mmse)  
tab_cov$cov2_mmse = ifelse(is.na(tab_cov$cov2_mmse), gathering$mmm_scor, tab_cov$cov2_mmse)  
tab_cov$cov2_mmse = ifelse(is.na(tab_cov$cov2_mmse), gathering$tmm_scor, tab_cov$cov2_mmse)  

# Imputation: screen 5 
tab_temp1 = tab_cov %>% select(stno, cov1_ethn)
tab_temp2 = tab_s5 %>% select(stno, tethgp)
gathering = merge(tab_temp1, tab_temp2, by.x = "stno", by.y = "stno")

# ---------------------------------------------------------------------------- #
# Covariates proper format 
tab_cov$cov1_sex    = factor(ifelse(tab_cov$cov1_sex ==1,0,1))     
tab_cov$cov1_edu    = factor(ifelse(tab_cov$cov1_edu <=2, 0, ifelse(tab_cov$cov1_edu ==3, 1,2))) 
tab_cov$cov1_marcoh = factor(ifelse(tab_cov$cov1_marcoh ==1, 1,0)) 
tab_cov$cov1_work   = factor(ifelse(tab_cov$cov1_work %in% c(1,2), 1,0)) 
tab_cov$cov1_smoke  = factor(ifelse(tab_cov$cov1_smoke ==1, 0, ifelse(tab_cov$cov1_smoke ==2, 1,2))) 
tab_cov$cov1_veg    = factor(ifelse(tab_cov$cov1_veg %in% c(1:6), 0, ifelse(tab_cov$cov1_veg ==7, 1, 2))) 
tab_cov$cov1_alcohol = factor(ifelse(tab_cov$cov1_alcohol ==0, 0, ifelse(tab_cov$cov1_alcohol <=14, 1, 2)))

# ---------------------------------------------------------------------------- #
# Include accelerometer features IAN
setwd(dir = dir_azt1)
part2      = read.csv("part2_summary.csv")
part2$stno = as.numeric(gsub("[^0-9.-]", "", part2$ID))
P2         = part2 %>% select(stno, calib_err,	calib_status)

setwd(dir = dir_3wii)
ppart2      = read.csv(file= "sample_part2_ok.csv", header = TRUE, sep = ";", quote = "\"'")
P2 = rbind(P2, ppart2)

setwd(dir = dir_azt1) 
part5 = read.csv("part5_personsummary_WW_L40M100V400_T5A5.csv")
part5$stno = as.numeric(gsub("[^0-9.-]", "", part5$ID))

part6 = read.csv("part6_summary.csv")
part6$stno = as.numeric(gsub("[^0-9.-]", "", part6$ID))

P5 = part5 %>% select(stno, Nvaliddays, dur_day_total_IN_min_pla, dur_day_total_LIG_min_pla, dur_day_total_MOD_min_pla,
                      dur_day_total_VIG_min_pla, FRAG_Nfrag_IN_day_pla, FRAG_Nfrag_LIPA_day_pla, FRAG_Nfrag_MVPA_day_pla,
                      ig_day_intercept_pla, ig_day_gradient_pla, ACC_day_mg_pla, FRAG_TP_IN2PA_day_pla, FRAG_TP_PA2IN_day_pla, 
                      sleeponset_pla, wakeup_pla, dur_spt_min_pla, Nblocks_spt_sleep_pla, sleep_efficiency_after_onset_pla, 
                      ACC_spt_sleep_mg_pla, dur_spt_sleep_min_pla) %>% filter(stno %in% unique(sample_stno$stno))

P5$mb_dur_sb     = P5$dur_day_total_IN_min_pla
P5$mb_dur_lipa   = P5$dur_day_total_LIG_min_pla
P5$mb_dur_mvpa   = P5$dur_day_total_MOD_min_pla + P5$dur_day_total_VIG_min_pla
P5$mb_nb_sb      = P5$FRAG_Nfrag_IN_day_pla 
P5$mb_nb_lipa    = P5$FRAG_Nfrag_LIPA_day_pla 
P5$mb_nb_mvpa    = P5$FRAG_Nfrag_MVPA_day_pla
P5$mb_mdb_sb     = ifelse(P5$mb_nb_sb <=0, 0, P5$mb_dur_sb / P5$mb_nb_sb)
P5$mb_mdb_lipa   = ifelse(P5$mb_nb_lipa <=0, 0, P5$mb_dur_lipa / P5$mb_nb_lipa)
P5$mb_mdb_mvpa   = ifelse(P5$mb_nb_mvpa <=0, 0, P5$mb_dur_mvpa / P5$mb_nb_mvpa)
P5$mb_ig_inter   = P5$ig_day_intercept_pla
P5$mb_ig_slope   = P5$ig_day_gradient_pla 
P5$mb_acc_day    = P5$ACC_day_mg_pla
P5$ct_onset      = P5$sleeponset_pla
P5$ct_wakeup     = P5$wakeup_pla - 24
P5$spt_dur_sleep = P5$dur_spt_sleep_min_pla 
P5$spt_nb_sleep  = P5$Nblocks_spt_sleep_pla
P5$spt_mdb_sleep = ifelse(P5$spt_nb_sleep <=0, 0, P5$spt_dur_sleep/P5$spt_nb_sleep)
P5$spt_eff       = P5$sleep_efficiency_after_onset_pla
P5$spt_acc_night = P5$ACC_spt_sleep_mg_pla
P5$spt_mdb_wake  = (P5$dur_spt_min_pla - P5$spt_dur_sleep) / (P5$spt_nb_sleep - 1) 
P5$spt_waso      = P5$dur_spt_min_pla - P5$spt_dur_sleep 

P6 = part6 %>% select(stno, M10VALUE, L5VALUE, M10TIME_num, L5TIME_num, IV, IS, 
                      cosinor_mes, cosinor_amp, cosinor_R2, cosinor_acrotime,
                      FRAG_TP_IN2PA_day, FRAG_TP_PA2IN_day,
                      FRAG_TP_sleep2wake_spt, FRAG_TP_wake2sleep_spt)%>% 
  filter(stno %in% unique(sample_stno$stno))


P6$mb_m10value   = P6$M10VALUE
P6$mb_TP_ra      = P6$FRAG_TP_IN2PA_day 
P6$mb_TP_ar      = P6$FRAG_TP_PA2IN_day 
P6$spt_l5value   = P6$L5VALUE
P6$spt_TP_sw     = P6$FRAG_TP_sleep2wake_spt 
P6$spt_TP_ws     = P6$FRAG_TP_wake2sleep_spt 
P6$rar_iv        = P6$IV
P6$rar_is        = P6$IS
P6$rar_mesor     = P6$cosinor_mes
P6$rar_amp       = P6$cosinor_amp
P6$rar_ra        = (P6$mb_m10value - P6$spt_l5value) / (P6$mb_m10value + P6$spt_l5value)
P6$rar_r2        = P6$cosinor_R2
P6$ct_m10time    = P6$M10TIME_num
P6$ct_l5time     = P6$L5TIME_num
P6$ct_acrotime   = P6$cosinor_acrotime
P_merge = merge(P5, P6, by = "stno")
P_merge = merge(P_merge, P2, by = "stno")
dim(P_merge)


# ---------------------------------------------------------------------------- #
# Merge covariates
# ---------------------------------------------------------------------------- #
tab_cov = merge(tab_cov, P_merge, by = "stno")

tab_cov = tab_cov %>% select(stno, Nvaliddays, calib_err, calib_status, 
                             cov1_age, cov1_alcohol,
                             cov1_bmi, cov1_cns, cov1_diab, cov1_edu, cov1_ethn, cov1_ghq, 
                             cov1_hypl, cov1_hypt, cov1_marcoh, cov1_Nchronic, 
                             cov1_sex, cov1_smoke, cov1_veg, cov1_work, cov2_mmse, 
                             cov3_cvd, cov3_dep,
                             cov4_apoe, mb_dur_sb, mb_dur_lipa, mb_dur_mvpa, mb_nb_sb, 
                             mb_nb_lipa, mb_nb_mvpa, mb_mdb_sb, mb_mdb_lipa, mb_mdb_mvpa, 
                             mb_ig_inter, mb_ig_slope, mb_acc_day, mb_m10value, mb_TP_ra,
                             mb_TP_ar, spt_acc_night, spt_dur_sleep, spt_eff, spt_l5value, 
                             spt_mdb_sleep, spt_mdb_wake, spt_nb_sleep, spt_TP_sw, spt_TP_ws,
                             spt_waso, rar_iv, rar_is, rar_mesor, rar_amp, rar_ra, rar_r2, 
                             ct_onset, ct_wakeup, ct_m10time, ct_l5time, ct_acrotime)


# ---------------------------------------------------------------------------- #
# Merge outcomes
# ---------------------------------------------------------------------------- #
tab_demence = tab_demence %>% select(stno, y, y_date, y_time, y_prev)
tab_mort = tab_mort %>% select(stno, y_m, y_m_date, y_m_time, y_m_prev)
tab_cvd = tab_cvd %>% select(stno, y_cvd, y_cvd_date, y_cvd_time, y_cvd_prev)
tab_cov = merge(tab_cov, tab_demence, by = "stno")
tab_cov = merge(tab_cov, tab_mort, by = "stno")
tab_cov = merge(tab_cov, tab_cvd, by = "stno")

# ---------------------------------------------------------------------------- #
# exclusion criteria 
# ---------------------------------------------------------------------------- #

setwd(dir = dir_3wii)
pop_target_ok  = read_dta("pop_target_ok.dta")
pop_target_ok1 = pop_target_ok %>% select(stno,nvd_reason)
dim(pop_target_ok1)
table(pop_target_ok1$nvd_reason)
tab_cov = merge(tab_cov, pop_target_ok1, by.x = "stno", by.y = "stno")

tab_cov$exclusion_calib0 = ifelse(is.na(tab_cov$nvd_reason) | tab_cov$nvd_reason == 1, 1, 0)
tab_cov$exclusion_calib1 = ifelse(is.na(tab_cov$calib_err) | tab_cov$calib_err > 0.010, 1, 0)
tab_cov$exclusion_calib2 = ifelse(is.na(tab_cov$calib_status)==FALSE | tab_cov$calib_status =="recalibration done, no problems detected" |
                                    tab_cov$calib_status == "recalibration done, but temperature values not used" |
                                    tab_cov$calib_status =="recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this", 
                                  0, 1)
tab_cov$exclusion_calib3 = ifelse(is.na(tab_cov$Nvaliddays) | tab_cov$Nvaliddays < 5, 1, 0)


tab_cov$exclusion_y   = ifelse(is.na(tab_cov$y) | is.na(tab_cov$y_time), 1, 0)
tab_cov$exclusion_cov = ifelse(is.na(tab_cov$cov1_age) |
                             is.na(tab_cov$cov1_sex) |  
                             is.na(tab_cov$cov1_edu) | is.na(tab_cov$cov1_marcoh) |
                               is.na(tab_cov$cov1_smoke) |
                               is.na(tab_cov$cov1_veg) | is.na(tab_cov$cov1_alcohol) |
                               is.na(tab_cov$cov1_bmi) | is.na(tab_cov$cov1_hypl) |
                               is.na(tab_cov$cov1_Nchronic) | is.na(tab_cov$cov1_cns) , 1,0)
tab_cov$exclusion_apoe = ifelse(is.na(tab_cov$cov4_apoe), 1, 0)


# ---------------------------------------------------------------------------- #
# 4.	Individual participant exclusion criteria (flowchart): 
tab_cov$exclusion_age60      = ifelse(tab_cov$cov1_age < 60, 1, 0)
tab_cov$exclusion_y_prev     = tab_cov$y_prev 
tab_cov$age_dementia         = ifelse(tab_cov$y==1, tab_cov$cov1_age + tab_cov$y_time/365, 500)
tab_cov$age_end_followup     = tab_cov$cov1_age + (tab_cov$y_time/365)
tab_cov$exclusion_dementia65 = ifelse(tab_cov$age_dementia < 65, 1, 0) 
tab_cov$exclusion_follow65   = ifelse(tab_cov$age_end_followup < 65, 1, 0) 

# ---------------------------------------------------------------------------- #
# 5. Removing duplicates
tab_cov = tab_cov %>% distinct(stno, .keep_all = TRUE)


# ---------------------------------------------------------------------------- #
# Plasma Data tau 217
# ---------------------------------------------------------------------------- #

setwd(dir_ad)
dat_plasma = read_dta("AD_biomarkers_p11p12.dta")
wave11_12 = dat_plasma %>% select(stno, fptau, dptau)
tab_cov_T = merge(tab_cov, wave11_12, 
      by = "stno", 
      all.x = TRUE)
tab_cov = tab_cov_T


# ---------------------------------------------------------------------------- #
# Build datasets

tab_cov_exc0  = tab_cov %>% filter(exclusion_calib0 == 0)
tab_cov_exc1  = tab_cov %>% filter(exclusion_calib1 == 0)
tab_cov_exc2  = tab_cov_exc1 %>% filter(exclusion_calib2 == 0)
tab_cov_exc3  = tab_cov_exc2 %>% filter(exclusion_calib3 == 0)
tab_cov_exc5  = tab_cov_exc3 %>% filter(exclusion_y_prev == 0)
tab_cov_exc6  = tab_cov_exc5 %>% filter(exclusion_dementia65 == 0)
tab_cov_exc7  = tab_cov_exc6 %>% filter(exclusion_follow65 == 0)
tab_cov_exc8  = tab_cov_exc7 %>% filter(spt_dur_sleep >= 3*60)
tab_cov_exc9  = tab_cov_exc8 %>% filter(exclusion_cov == 0)
tab_cov_main  = tab_cov_exc8 
tab_cov_clean = tab_cov_exc9
tab_cov_A4    = tab_cov_clean %>% filter(exclusion_apoe == 0)
tab_cov_ghq    = tab_cov_clean %>% filter(!is.na(cov1_ghq))
tab_cov_mmse   = tab_cov_clean %>% filter(!is.na(cov2_mmse))
tab_cov_fptau  = tab_cov_clean %>% filter(!is.na(fptau))

dim(tab_cov_main)
dim(tab_cov_clean)
dim(tab_cov_A4)
dim(tab_cov_ghq)
dim(tab_cov_mmse)
dim(tab_cov_fptau)



# ---------------------------------------------------------------------------- #
# Save dataset

setwd(dir = dir_save)
write.csv(x = tab_cov_exc3,  file = "data_WII_exc3.csv", row.names = F)
write.csv(x = tab_cov_main,  file = "data_WII_main.csv", row.names = F)
write.csv(x = tab_cov_clean,  file = "data_WII_clean.csv", row.names = F)
write.csv(x = tab_cov_A4,    file = "data_WII_A4.csv", row.names = F)

# ---------------------------------------------------------------------------- #
# End 




