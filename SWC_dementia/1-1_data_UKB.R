# ---------------------------------------------------------------------------- #
# Script : 1_1_data_UKB.R
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
dir_save = "D:/..."
dir_azt1 = "D:/..."
dir_ian  = "D:/..."
dir_sam  = "D:/..."
dir_sam2 = "D:/..."
dir_sam3 = "D:/..."
dir_nat  = "D:/..."

# ---------------------------------------------------------------------------- #
# Data
# Data Accelerometer 

setwd(dir = dir_azt1)
part2      = read.csv2("part2_summary.csv", header = TRUE, sep=";", dec=",", quote = "\"'")
part2$stno = as.numeric(gsub("[^0-9.-]", "", part2$ID))
sample_stno2 = part2 %>% select(stno)


part5      = read.csv2("part5_personsummary_OO_L40M100V400_T5A5.csv", header = TRUE, sep=";", dec=",", quote = "\"'")
part5$stno = as.numeric(gsub("[^0-9.-]", "", part5$ID))
sample_stno5 = part5 %>% select(stno)


setwd(dir = dir_azt1)
part6      = read.csv2("part6_summary.csv", header = TRUE, sep=";", dec=",", quote = "\"'")
part6$stno = as.numeric(gsub("[^0-9.-]", "", part6$ID))
sample_stno6 = part6 %>% select(stno)


sample_stno = rbind(sample_stno2, sample_stno5, sample_stno6)
sample_stno = sample_stno %>% distinct(stno, .keep_all = TRUE)

P2 = part2 %>% select(stno, calib_err,	calib_status) 
P5 = part5 %>% select(stno, Nvaliddays, dur_day_total_IN_min_pla, dur_day_total_LIG_min_pla, dur_day_total_MOD_min_pla,
                      dur_day_total_VIG_min_pla, FRAG_Nfrag_IN_day_pla, FRAG_Nfrag_LIPA_day_pla, FRAG_Nfrag_MVPA_day_pla,
                      ig_day_intercept_pla, ig_day_gradient_pla, ACC_day_mg_pla, FRAG_TP_IN2PA_day_pla, FRAG_TP_PA2IN_day_pla, 
                      sleeponset_pla, wakeup_pla, dur_spt_min_pla, Nblocks_spt_sleep_pla, sleep_efficiency_after_onset_pla, 
                      ACC_spt_sleep_mg_pla, dur_spt_sleep_min_pla) %>% filter(stno %in% unique(sample_stno$stno))

P6 = part6 %>% select(stno, M10VALUE, L5VALUE, M10TIME_num, L5TIME_num, IV, IS, 
                      cosinor_mes, cosinor_amp, cosinor_R2, cosinor_acrotime,
                      FRAG_TP_IN2PA_day, FRAG_TP_PA2IN_day,
                      FRAG_TP_sleep2wake_spt, FRAG_TP_wake2sleep_spt)

P5$mb_dur_sb     = as.numeric(P5$dur_day_total_IN_min_pla)
P5$mb_dur_lipa   = as.numeric(P5$dur_day_total_LIG_min_pla)
P5$mb_dur_mvpa   = as.numeric(P5$dur_day_total_MOD_min_pla) + as.numeric(P5$dur_day_total_VIG_min_pla)
P5$mb_nb_sb      = as.numeric(P5$FRAG_Nfrag_IN_day_pla) 
P5$mb_nb_lipa    = as.numeric(P5$FRAG_Nfrag_LIPA_day_pla) 
P5$mb_nb_mvpa    = as.numeric(P5$FRAG_Nfrag_MVPA_day_pla)
P5$mb_mdb_sb     = ifelse(as.numeric(P5$mb_nb_sb) <=0, 0, as.numeric(P5$mb_dur_sb) / as.numeric(P5$mb_nb_sb))
P5$mb_mdb_lipa   = ifelse(as.numeric(P5$mb_nb_lipa) <=0, 0, as.numeric(P5$mb_dur_lipa) / as.numeric(P5$mb_nb_lipa))
P5$mb_mdb_mvpa   = ifelse(as.numeric(P5$mb_nb_mvpa) <=0, 0, as.numeric(P5$mb_dur_mvpa) / as.numeric(P5$mb_nb_mvpa))
P5$mb_ig_inter   = as.numeric(P5$ig_day_intercept_pla)
P5$mb_ig_slope   = as.numeric(P5$ig_day_gradient_pla )
P5$mb_acc_day    = as.numeric(P5$ACC_day_mg_pla)
P5$ct_onset      = as.numeric(P5$sleeponset_pla)
P5$ct_wakeup     = as.numeric(P5$wakeup_pla) - 24
P5$spt_dur_sleep = as.numeric(P5$dur_spt_sleep_min_pla) 
P5$spt_nb_sleep  = as.numeric(P5$Nblocks_spt_sleep_pla)
P5$spt_mdb_sleep = ifelse(as.numeric(P5$spt_nb_sleep) <=0, 0, as.numeric(P5$spt_dur_sleep)/as.numeric(P5$spt_nb_sleep))
P5$spt_eff       = as.numeric(P5$sleep_efficiency_after_onset_pla)
P5$spt_acc_night = as.numeric(P5$ACC_spt_sleep_mg_pla)
P5$spt_mdb_wake  = (as.numeric(P5$dur_spt_min_pla) - as.numeric(P5$spt_dur_sleep)) / (as.numeric(P5$spt_nb_sleep) - 1) 
P5$spt_waso      = as.numeric(P5$dur_spt_min_pla) - as.numeric(P5$spt_dur_sleep) 
P6$mb_m10value   = as.numeric(P6$M10VALUE)
P6$mb_TP_ra      = as.numeric(P6$FRAG_TP_IN2PA_day) # ?
P6$mb_TP_ar      = as.numeric(P6$FRAG_TP_PA2IN_day) # ?
P6$spt_l5value   = as.numeric(P6$L5VALUE)
P6$spt_TP_sw     = as.numeric(P6$FRAG_TP_sleep2wake_spt) # ?
P6$spt_TP_ws     = as.numeric(P6$FRAG_TP_wake2sleep_spt) # ?
P6$rar_iv        = as.numeric(P6$IV)
P6$rar_is        = as.numeric(P6$IS)
P6$rar_mesor     = as.numeric(P6$cosinor_mes)
P6$rar_amp       = as.numeric(P6$cosinor_amp)
P6$rar_ra        = (as.numeric(P6$mb_m10value) - as.numeric(P6$spt_l5value)) / (as.numeric(P6$mb_m10value) + as.numeric(P6$spt_l5value))
P6$rar_r2        = as.numeric(P6$cosinor_R2)
P6$ct_m10time    = as.numeric(P6$M10TIME_num)
P6$ct_l5time     = as.numeric(P6$L5TIME_num)
P6$ct_acrotime   = as.numeric(P6$cosinor_acrotime)

DF_ghost2 = data.frame(matrix(ncol = dim(P2)[2], nrow = dim(sample_stno)[1]))
colnames(DF_ghost2) = colnames(P2)
DF_ghost2$stno = sample_stno$stno
P2 = rbind(P2, DF_ghost2)
P2 = P2 %>% distinct(stno, .keep_all = TRUE)
DF_ghost5 = data.frame(matrix(ncol = dim(P5)[2], nrow = dim(sample_stno)[1]))
colnames(DF_ghost5) = colnames(P5)
DF_ghost5$stno = sample_stno$stno
P5 = rbind(P5, DF_ghost5)
P5 = P5 %>% distinct(stno, .keep_all = TRUE)
DF_ghost6 = data.frame(matrix(ncol = dim(P6)[2], nrow = dim(sample_stno)[1]))
colnames(DF_ghost6) = colnames(P6)
DF_ghost6$stno = sample_stno$stno
P6 = rbind(P6, DF_ghost6)
P6 = P6 %>% distinct(stno, .keep_all = TRUE)
P_merge = merge(P5, P6, by = "stno", all = TRUE)
P_merge = merge(P_merge, P2, by = "stno", all = TRUE)
P_merge$exclusion_calib1 = ifelse(is.na(P_merge$calib_err) | P_merge$calib_err <= 0.010, 0, 1) # NA opposite by bug
P_merge$exclusion_calib2 = ifelse(is.na(P_merge$calib_status) | P_merge$calib_status =="recalibration done, no problems detected" |
                                    P_merge$calib_status == "recalibration done, but temperature values not used" |
                                    P_merge$calib_status =="recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this", 
                                  0, 1)  # NA opposite by bug
P_merge$exclusion_calib3 = ifelse(is.na(P_merge$Nvaliddays) | P_merge$Nvaliddays < 5, 1, 0)


# ---------------------------------------------------------------------------- #
# Covariates 
tab_cov = data.frame(matrix(ncol = 1, nrow = dim(sample_stno)[1]))
colnames(tab_cov) = "stno"
tab_cov$stno = sample_stno$stno
setwd(dir = dir_ian)
Data_cov0 = read_dta(file = "Final dataset_251601.dta") 


Data_cov0$age = round((as.Date(Data_cov0$calendar_date) - Data_cov0$dob) / 365.25, 2)

setwd(dir = dir_ian)
Data_cov1 = read_dta(file = "Data_ukb_cov1_270424.dta")
Data_cov2 = read_dta(file = "Data_ukb_cov2_270424.dta")
Data_cov3 = read_dta(file = "Data_ukb_cov3_270424.dta")

Data_cov_merge = merge(Data_cov1, Data_cov2, by="n_eid", all = TRUE)
Data_cov_merge = merge(Data_cov_merge, Data_cov3, by="n_eid", all = TRUE)
Data_cov_merge = merge(Data_cov_merge, Data_cov0, by="n_eid", all = TRUE)

dim(tab_cov)
dim(Data_cov0)
dim(Data_cov1)
dim(Data_cov2)
dim(Data_cov3)
dim(Data_cov_merge)

Data_cov_merge$ALC = Data_cov_merge$alcohol # 0 1 2 
Data_cov_merge$smoking = ifelse(Data_cov_merge$smoking==-3, NA, Data_cov_merge$smoking)
Data_cov_merge$doacc = as.Date(Data_cov_merge$calendar_date)
Data_cov_merge$DIABETES_p = ifelse(Data_cov_merge$diabetes_out_sro == 1 & Data_cov_merge$diabetes_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$DIABETES_p = ifelse(is.na(Data_cov_merge$DIABETES_p), 0, Data_cov_merge$DIABETES_p)
Data_cov_merge$HYPTEN_p = ifelse(Data_cov_merge$hypertension_out_sro == 1 & Data_cov_merge$hypertension_out_sro <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$HYPTEN_p = ifelse(is.na(Data_cov_merge$HYPTEN_p), 0, Data_cov_merge$HYPTEN_p)
Data_cov_merge$COPD_p = ifelse(Data_cov_merge$copd_out_sro & Data_cov_merge$copd_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$COPD_p = ifelse(is.na(Data_cov_merge$COPD_p), 0, Data_cov_merge$COPD_p)
Data_cov_merge$STROKE_p = ifelse(Data_cov_merge$stroke_out_sro & Data_cov_merge$stroke_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$STROKE_p = ifelse(is.na(Data_cov_merge$STROKE_p), 0, Data_cov_merge$STROKE_p) 
Data_cov_merge$CHD_p = ifelse(Data_cov_merge$chd_out_sro & Data_cov_merge$chd_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$CHD_p = ifelse(is.na(Data_cov_merge$CHD_p), 0, Data_cov_merge$CHD_p)
Data_cov_merge$DEPR_p = ifelse(Data_cov_merge$depression_out_sro & Data_cov_merge$depression_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$DEPR_p = ifelse(is.na(Data_cov_merge$DEPR_p), 0, Data_cov_merge$DEPR_p)
Data_cov_merge$ARTH1_p = ifelse(Data_cov_merge$osteoarthritis_out_sro & Data_cov_merge$osteoarthritis_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$ARTH1_p = ifelse(is.na(Data_cov_merge$ARTH1_p), 0, Data_cov_merge$ARTH1_p)
Data_cov_merge$ARTH2_p = ifelse(Data_cov_merge$rheumarthritis_out_sro & Data_cov_merge$rheumarthritis_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$ARTH2_p = ifelse(is.na(Data_cov_merge$ARTH2_p), 0, Data_cov_merge$ARTH2_p)
Data_cov_merge$PARK_p = ifelse(Data_cov_merge$parkinson_out_sro & Data_cov_merge$parkinson_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$PARK_p = ifelse(is.na(Data_cov_merge$PARK_p), 0, Data_cov_merge$PARK_p)
Data_cov_merge$HF_p = ifelse(Data_cov_merge$hfailure_out_sro & Data_cov_merge$hfailure_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$HF_p = ifelse(is.na(Data_cov_merge$HF_p), 0, Data_cov_merge$HF_p)
Data_cov_merge$LIVER_p = ifelse(Data_cov_merge$liver_out_sro & Data_cov_merge$liver_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$LIVER_p = ifelse(is.na(Data_cov_merge$LIVER_p), 0, Data_cov_merge$LIVER_p)
Data_cov_merge$CANCER_p = ifelse(Data_cov_merge$cancer_out_sro & Data_cov_merge$cancer_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$CANCER_p = ifelse(is.na(Data_cov_merge$CANCER_p), 0, Data_cov_merge$CANCER_p)
Data_cov_merge$MD_p = ifelse(Data_cov_merge$mentaldisorders_out_sro & Data_cov_merge$mentaldisorders_out_sro_date <= Data_cov_merge$doacc, 1, 0)
Data_cov_merge$MD_p = ifelse(is.na(Data_cov_merge$MD_p), 0, Data_cov_merge$MD_p)

Data_cov_merge$NBCHDI = Data_cov_merge$MD_p + Data_cov_merge$CANCER_p + Data_cov_merge$LIVER_p + Data_cov_merge$HF_p + 
  Data_cov_merge$PARK_p + (Data_cov_merge$ARTH1_p + Data_cov_merge$ARTH2_p >= 1) + Data_cov_merge$DEPR_p + Data_cov_merge$CHD_p + 
  Data_cov_merge$STROKE_p + Data_cov_merge$COPD_p

setwd(dir = dir_ian)
UK_data1 = read_dta(file = "Sam_ukb_dataset3.dta")
UK_data2 = read_dta(file = "Sam_ukb_dataset4.dta")
setwd(dir = dir_ian)
UK_data3 = read_dta(file = "data_final_ukbb_250116.dta")
UK_merge = merge(UK_data1, UK_data2, by="n_eid", all = TRUE)
UK_merge = merge(UK_merge, UK_data3, by="n_eid", all = TRUE)
UK_merge$WORK = ifelse(UK_merge$n_6142_0_0 == 1,1, ifelse(UK_merge$n_6142_0_0 < 0,NA, 0))   
UK_merge$LIPDRG = ifelse(UK_merge$n_6153_0_0  == 1, 1, 0)
UK_merge$LIPDRG = ifelse(is.na(UK_merge$LIPDRG), 0, UK_merge$LIPDRG)
UK_merge$LDL = UK_merge$LDL1
UK_merge$LDL = ifelse(is.na(UK_merge$LDL), UK_merge$LDL2, UK_merge$LDL)
UK_merge$HYPLIP = ifelse(UK_merge$LDL > 4.1 | UK_merge$LIPDRG == 1, 1, 0)
UK_merge$HYPLIP = ifelse(is.na(UK_merge$HYPLIP), 0, UK_merge$HYPLIP)
setwd(dir = dir_ian)
Data_cns = read_dta(file = "cns_meds.dta")
Data_cns$cns = ifelse(Data_cns$antidep_0_0 == 1 | Data_cns$anxio_0_0 == 1 | Data_cns$antipsych_0_0 == 1 | Data_cns$sed_hypn_0_0 == 1, 1, 0)
Data_cns$cns = ifelse(is.na(Data_cns$cns), 0, Data_cns$cns)

setwd(dir = dir_ian)
Data_apoe = read_dta(file = "apoe.dta")
 

# ---------------------------------------------------------------------------- #
# Merge Covariates   
 
Data_cov_merge = Data_cov_merge %>% select(n_eid, bmi,age, sex.x, edu.x, livingalone, smoking, fruitveg, ALC, 
                                           DIABETES_p, HYPTEN_p, NBCHDI )
UK_merge = UK_merge %>% select(n_eid, WORK, HYPLIP)
Data_cns = Data_cns %>% select(n_eid, cns)
Data_apoe = Data_apoe %>% select(n_eid, apoe4)


Data_final = merge(Data_cov_merge, UK_merge, by="n_eid", all = TRUE)
Data_final = merge(Data_final, Data_cns, by="n_eid", all = TRUE)
Data_final = merge(Data_final, Data_apoe, by="n_eid", all = TRUE)
Data_final$stno = Data_final$n_eid
Data_final$sex = ifelse(Data_final$sex.x ==2, 1, 0) 
Data_final$edu = Data_final$edu.x 
tab_cov = merge(tab_cov, Data_final, by="stno", all = TRUE)
tab_cov$livingalone[1:10] 
tab_cov$cov1_marcoh = ifelse(tab_cov$livingalone ==1,0,1)                           
tab_cov$cov1_smoke = tab_cov$smoking
tab_cov$cov1_age     = round(as.numeric(tab_cov$age),2)
tab_cov$cov1_alcohol = tab_cov$ALC
tab_cov$cov1_edu    = tab_cov$edu
tab_cov$cov1_sex    = tab_cov$sex
tab_cov$cov1_bmi     = tab_cov$bmi 
tab_cov$cov1_cns     = tab_cov$cns
tab_cov$cov1_diab    = tab_cov$DIABETES_p
tab_cov$cov1_hypl     = tab_cov$HYPLIP
tab_cov$cov1_hypt     = tab_cov$HYPTEN_p
tab_cov$cov1_Nchronic = tab_cov$NBCHDI
tab_cov$cov1_veg     = tab_cov$fruitveg
tab_cov$cov1_work     = tab_cov$WORK
tab_cov$cov4_apoe     = tab_cov$apoe4
tab_cov = tab_cov %>% select(stno, cov1_age, cov1_alcohol,
                             cov1_bmi, cov1_cns, cov1_diab, cov1_edu,   
                             cov1_hypl, cov1_hypt, cov1_marcoh, cov1_Nchronic, 
                             cov1_sex, cov1_smoke, cov1_veg, cov1_work, cov4_apoe)


# ---------------------------------------------------------------------------- #
# Do accelerometer 
reference_date = as.Date("2022-11-30", format = "%Y-%m-%d") 

setwd(dir = dir_ian)
Data_cov0 = read_dta(file = "Final dataset_251601.dta") 
Data_cov0$doacc = as.Date(Data_cov0$calendar_date)
Data_cov0 = Data_cov0 %>% select(n_eid, doacc)

# ---------------------------------------------------------------------------- #
# Outcome: mortality 
setwd(dir = dir_ian)

tab_y1 = read_dta(file = "mortality_UKB_sam.dta") 
tab_y1$y_m = ifelse(is.na(tab_y1$ts_40000_0_0), 0, 1)
tab_y1$y_m_date = as.Date(reference_date, format = "%Y-%m-%d")
tab_y1$y_m_date = as.Date(ifelse(is.na(tab_y1$ts_40000_0_0)==TRUE,tab_y1$y_m_date, tab_y1$ts_40000_0_0))
tab_y1 = merge(tab_y1, Data_cov0, by = "n_eid", all = FALSE, all.y=TRUE)
tab_y1$y_m_time = as.numeric(difftime(strptime(tab_y1$y_m_date, format = "%Y-%m-%d"),
                                      strptime(tab_y1$doacc, format = "%Y-%m-%d"),units="days"))
tab_y1$y_m_prev = ifelse(tab_y1$y_m_time <= 0, 1, 0)
table(tab_y1$y_m_prev)
table(tab_y1$y_m)
tab_y1 = tab_y1 %>% select(n_eid, y_m, y_m_date, y_m_time)
tab_y1 = tab_y1 %>% filter(is.na(n_eid)==FALSE)

# ---------------------------------------------------------------------------- #
# Outcome: CVD
setwd(dir = dir_nat)

tab_y2 = read_dta(file = "ukb_cvd_nathalia.dta") 
tab_y2$CVD = ifelse(tab_y2$stroke_out_sro == 1 | tab_y2$chd_out_sro == 1 | tab_y2$hfailure_out_sro == 1, 1, 0)
tab_y2$CVD = ifelse(is.na(tab_y2$CVD) | tab_y2$CVD==0,0,1)
tab_y2$d1  = as.Date(tab_y2$stroke_out_sro_date, format = "%Y-%m-%d") 
tab_y2$d2  = as.Date(tab_y2$chd_out_sro_date, format = "%Y-%m-%d") 
tab_y2$d3  = as.Date(tab_y2$hfailure_out_sro_date, format = "%Y-%m-%d") 
tab_y2$CVD_date = as.Date(apply(tab_y2[, c("d1", "d2", "d3")], 1, min, na.rm = TRUE))
tab_y2$y_cvd_date = as.Date(reference_date, format = "%Y-%m-%d")
tab_y2$y_cvd_date = as.Date(ifelse(is.na(tab_y2$CVD_date)==TRUE, tab_y2$y_cvd_date, tab_y2$CVD_date))
tab_y2$y_cvd = tab_y2$CVD
tab_y2 = tab_y2 %>% select(n_eid, y_cvd, y_cvd_date)
tab_y2 = merge(tab_y2, Data_cov0, by = "n_eid", all = FALSE, all.y=TRUE)
tab_y2 = merge(tab_y2, tab_y1, by = "n_eid", all = FALSE, all.x=TRUE)
tab_y2$y_cvd_date = as.Date(ifelse(tab_y2$y_cvd_date < tab_y2$y_m_date, tab_y2$y_cvd_date, tab_y2$y_m_date))
tab_y2$y_cvd_time = as.numeric(difftime(strptime(tab_y2$y_cvd_date, format = "%Y-%m-%d"),
                                   strptime(tab_y2$doacc, format = "%Y-%m-%d"),units="days"))
tab_y2$y_cvd_prev = ifelse(tab_y2$y_cvd_time <= 0, 1, 0)
table(tab_y2$y_cvd_prev)
tab_y2 = tab_y2 %>% filter(is.na(n_eid)==FALSE)
tab_y2 = tab_y2 %>% select(n_eid, y_cvd, y_cvd_date, y_cvd_time)
# ---------------------------------------------------------------------------- #
# Outcome: Dementia
setwd(dir = dir_ian)
tab_y3   = read_dta(file = "dataset_diseasesanddementianew.dta") 
tab_y3   = tab_y3 %>%  select(n_eid, dementia, dementia_date, dementia_source, AD, VD, FD, OtherD, PD)
tab_y3$y = ifelse(tab_y3$dementia ==1 | tab_y3$AD ==1 | tab_y3$VD ==1 | tab_y3$FD ==1 | tab_y3$OtherD ==1 | tab_y3$PD ==1, 1, 0) 
tab_y3$y = ifelse(is.na(tab_y3$y)==TRUE, 0, tab_y3$y) 
tab_y3$y_date = as.Date(reference_date, format = "%Y-%m-%d")
tab_y3$y_date = as.Date(ifelse(is.na(tab_y3$dementia_date)==TRUE,tab_y3$y_date, tab_y3$dementia_date))
tab_y3 = merge(tab_y3, Data_cov0, by = "n_eid", all = FALSE, all.y=TRUE)
tab_y3 = merge(tab_y3, tab_y1, by = "n_eid", all = FALSE, all.x=TRUE)
tab_y3$y_date = as.Date(ifelse(tab_y3$y_date < tab_y3$y_m_date, tab_y3$y_date, tab_y3$y_m_date))
tab_y3$y_time = as.numeric(difftime(strptime(tab_y3$y_date, format = "%Y-%m-%d"),
                                      strptime(tab_y3$doacc, format = "%Y-%m-%d"),units="days"))
tab_y3$y_prev = ifelse(tab_y3$y_time <= 0, 1, 0) # Prevalence demence
tab_y3$y_sa   = ifelse(tab_y3$y_prev==0 & tab_y3$y_time>2*365, 1, 0) 
table(tab_y3$y_prev)
table(tab_y3$y)
tab_y3 = tab_y3 %>% select(n_eid, y, y_date, y_time, y_prev, y_sa)
tab_y3 = tab_y3 %>% filter(is.na(n_eid)==FALSE)
tab_y1$n_eid = as.numeric(tab_y1$n_eid)
tab_y2$n_eid = as.numeric(tab_y2$n_eid)
tab_y3$n_eid = as.numeric(tab_y3$n_eid)
tab_y4 = merge(tab_y3, tab_y2, by = "n_eid", all = FALSE, all.x = TRUE)
tab_y = merge(tab_y4, tab_y1, by = "n_eid", all = FALSE, all.x = TRUE)
tab_y$stno = tab_y$n_eid 

# ---------------------------------------------------------------------------- #
# 5. Removing non acc stno
P_merge1 = P_merge %>% filter(stno %in% sample_stno$stno)
tab_cov1 = tab_cov %>% filter(stno %in% sample_stno$stno)
tab_y1   = tab_y %>% filter(stno %in% sample_stno$stno)


# ---------------------------------------------------------------------------- #
# 5. Removing duplicates
tab_acc = P_merge1 %>% distinct(stno, .keep_all = TRUE)
tab_cov = tab_cov1 %>% distinct(stno, .keep_all = TRUE)
tab_y   = tab_y1   %>% distinct(stno, .keep_all = TRUE)

# ---------------------------------------------------------------------------- #
# Build Main Dataset

tab_acc = tab_acc %>% select(stno, Nvaliddays, calib_err, calib_status,   
                             mb_dur_sb, mb_dur_lipa, mb_dur_mvpa, mb_nb_sb, 
                             mb_nb_lipa, mb_nb_mvpa, mb_mdb_sb, mb_mdb_lipa, mb_mdb_mvpa, 
                             mb_ig_inter, mb_ig_slope, mb_acc_day, mb_m10value, mb_TP_ra,
                             mb_TP_ar, spt_acc_night, spt_dur_sleep, spt_eff, spt_l5value, 
                             spt_mdb_sleep, spt_mdb_wake, spt_nb_sleep, spt_TP_sw, spt_TP_ws,
                             spt_waso, rar_iv, rar_is, rar_mesor, rar_amp, rar_ra, rar_r2, 
                             ct_onset, ct_wakeup, ct_m10time, ct_l5time, ct_acrotime, 
                             exclusion_calib1, exclusion_calib2, exclusion_calib3)
tab_cov_acc = merge(tab_cov, tab_acc, by = "stno", all = TRUE)
tab_cov_acc = tab_cov_acc %>% select(stno, Nvaliddays, calib_err, calib_status,   cov1_age, cov1_alcohol,
                             cov1_bmi, cov1_cns, cov1_diab, cov1_edu,  
                             cov1_hypl, cov1_hypt, cov1_marcoh, cov1_Nchronic, 
                             cov1_sex, cov1_smoke, cov1_veg, cov1_work, cov4_apoe,
                             mb_dur_sb, mb_dur_lipa, mb_dur_mvpa, mb_nb_sb, 
                             mb_nb_lipa, mb_nb_mvpa, mb_mdb_sb, mb_mdb_lipa, mb_mdb_mvpa, 
                             mb_ig_inter, mb_ig_slope, mb_acc_day, mb_m10value, mb_TP_ra,
                             mb_TP_ar, spt_acc_night, spt_dur_sleep, spt_eff, spt_l5value, 
                             spt_mdb_sleep, spt_mdb_wake, spt_nb_sleep, spt_TP_sw, spt_TP_ws,
                             spt_waso, rar_iv, rar_is, rar_mesor, rar_amp, rar_ra, rar_r2, 
                             ct_onset, ct_wakeup, ct_m10time, ct_l5time, ct_acrotime, 
                             exclusion_calib1, exclusion_calib2, exclusion_calib3)
tab_y = tab_y %>% select(stno, y, y_date, y_time, y_prev, y_sa, y_m, y_m_date, y_m_time, y_cvd, y_cvd_date, y_cvd_time)
tab_cov_acc_y = merge(tab_cov_acc, tab_y, by.x = "stno", by.y = "stno", all = TRUE)


# ---------------------------------------------------------------------------- #
# View more 
# 4.	Individual participant exclusion criteria (flowchart): 
tab_cov_acc_y$exclusion_calib1 = ifelse(is.na(tab_cov_acc_y$calib_err)==FALSE & tab_cov_acc_y$calib_err > 0.010, 1, 0)
tab_cov_acc_y$exclusion_calib2 = ifelse(is.na(tab_cov_acc_y$calib_status)==TRUE | tab_cov_acc_y$calib_status =="recalibration done, no problems detected" |
                                          tab_cov_acc_y$calib_status == "recalibration done, but temperature values not used" |
                                          tab_cov_acc_y$calib_status =="recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this", 
                                  0, 1) 
tab_cov_acc_y$exclusion_calib3 = ifelse(is.na(tab_cov_acc_y$Nvaliddays) | tab_cov_acc_y$Nvaliddays < 5, 1, 0)
tab_cov_acc_y$exclusion_no_age     = ifelse(is.na(tab_cov_acc_y$cov1_age)==TRUE, 1, 0)
tab_cov_acc_y$exclusion_age60      = ifelse(tab_cov_acc_y$cov1_age < 60 | is.na(tab_cov_acc_y$cov1_age)==TRUE , 1, 0)
tab_cov_acc_y$exclusion_y_prev     = tab_cov_acc_y$y_prev 
tab_cov_acc_y$age_dementia         = ifelse(tab_cov_acc_y$y==1, tab_cov_acc_y$cov1_age + tab_cov_acc_y$y_time/365, 500)
tab_cov_acc_y$age_end_followup     = tab_cov_acc_y$cov1_age + (tab_cov_acc_y$y_time/365)
tab_cov_acc_y$exclusion_dementia65 = ifelse(tab_cov_acc_y$age_dementia < 65, 1, 0) 
tab_cov_acc_y$exclusion_follow65   = ifelse(tab_cov_acc_y$age_end_followup < 65, 1, 0) 
tab_cov$exclusion_apoe       = ifelse(is.na(tab_cov$cov4_apoe),1,0)
tab_cov_acc_y$exclusion_y   = ifelse(is.na(tab_cov_acc_y$y) | is.na(tab_cov_acc_y$y_time), 1, 0)
tab_cov_acc_y$exclusion_cov = ifelse( is.na(tab_cov_acc_y$cov1_age) |
                                 is.na(tab_cov_acc_y$cov1_sex) |      
                                 is.na(tab_cov_acc_y$cov1_edu) | is.na(tab_cov_acc_y$cov1_marcoh) |
                                 is.na(tab_cov_acc_y$cov1_smoke) |
                                 is.na(tab_cov_acc_y$cov1_veg) | is.na(tab_cov_acc_y$cov1_alcohol) |
                                 is.na(tab_cov_acc_y$cov1_bmi) | is.na(tab_cov_acc_y$cov1_hypl) |
                                 is.na(tab_cov_acc_y$cov1_Nchronic) | is.na(tab_cov_acc_y$cov1_cns), 1,0)
tab_cov_acc_y$exclusion_apoe = ifelse(is.na(tab_cov$cov4_apoe), 1, 0)


# ---------------------------------------------------------------------------- #
# 6. Checking sample size accelerometer features
tab_acc_exc1  = tab_acc %>% filter(exclusion_calib1 == 0)
tab_acc_exc2  = tab_acc_exc1 %>% filter(exclusion_calib2 == 0)
tab_acc_exc3  = tab_acc_exc2 %>% filter(exclusion_calib3 == 0)

# ---------------------------------------------------------------------------- #
# Build datasets

tab_cov_exc1  = tab_cov_acc_y %>% filter(exclusion_no_age == 0)
tab_cov_exc2  = tab_cov_exc1 %>% filter(exclusion_age60 == 0)
tab_cov_exc3  = tab_cov_exc2 %>% filter(exclusion_calib1 == 0)
tab_cov_exc4  = tab_cov_exc3 %>% filter(exclusion_calib2 == 0)
tab_cov_exc5  = tab_cov_exc4 %>% filter(exclusion_calib3 == 0)
tab_cov_exc6  = tab_cov_exc5 %>% filter(exclusion_y_prev == 0)
tab_cov_exc7  = tab_cov_exc6 %>% filter(exclusion_dementia65 == 0)
tab_cov_exc8  = tab_cov_exc7 %>% filter(exclusion_follow65 == 0)
tab_cov_exc9  = tab_cov_exc8 %>% filter(spt_dur_sleep >= 3*60)
tab_cov_exc10 = tab_cov_exc9 %>% filter(exclusion_cov == 0)
tab_cov_main  = tab_cov_exc8
tab_cov_3hours = tab_cov_exc9
tab_cov_clean = tab_cov_exc10
tab_cov_A4    = tab_cov_clean %>% filter(exclusion_apoe == 0)

# ---------------------------------------------------------------------------- #
# Save dataset
setwd(dir = dir_save)
write.csv(x = tab_cov_3hours,  file = "data_UKB_3hours.csv", row.names = F)
write.csv(x = tab_cov_clean,  file = "data_UKB_clean.csv", row.names = F)
write.csv(x = tab_cov_A4,    file = "data_UKB_A4.csv", row.names = F)

# ---------------------------------------------------------------------------- #
# End 



