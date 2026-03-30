# ---------------------------------------------------------------------------- #
# Script : 0_0_aux_functions.R
# Author : I M Danilevicz 
# Remark : Part of the work available in:
#          C Cavailles, IM Danilevicz, S Vidil et al (2026). 
#          "Contribution of Digital Sleep-Wake Cycle Metrics to Dementia Prediction in Older Adults", (under review)
# ---------------------------------------------------------------------------- #

# Winsorizing function 
rescale_mat = function(X, sigma=10, scale=TRUE){
  if(scale==TRUE){
    X  = as.data.frame(scale(X))
  }
  p = dim(X)[2]
  for(j in 1:p){
    X[,j] = ifelse(X[,j] <= -sigma,-sigma, ifelse(X[,j]>=sigma, sigma, X[,j]))
  }
  return(X)
}

extract_HR_1 = function(mod1, cov="cov name", alpha=0.05, round=3){
  H = matrix(NA, ncol = 4, nrow = 1)
  summary1 = summary(mod1)
  summary1 = summary1$coefficients
  H[1,1] = exp(summary1[1,1])
  ig = qnorm(1-alpha/2)
  ci1 = c(summary1[1,1] - summary1[1,3]*ig, summary1[1,1] + summary1[1,3]*ig)
  ci1 = sort(exp(ci1))
  H[1,2:3] = ci1
  H[1,4] = summary1[1,5]
  H = round(H, round)
  colnames(H) = c("HR", "low CI", "up CI", "p-value")
  rownames(H) = cov
  return(H)
}

plot_hr = function(hr_model, model, predictor, main="Smooth HR for demence by x", 
                   leg=NULL, pos="bottom", xlab=NULL, xlim = NULL, adj = 0.5){
    if(xlim=="core"){
      if(!is.null(xlab)){
        xx <- hr_model$dataset[[predictor]]
        a = quantile(xx,0.01)
        b = quantile(xx,0.99)
        xticks <- seq(a, b, length.out = 5)
        par(xpd = FALSE)
        if(b-a>1){
          plot(hr_model, predictor=predictor, prob=0.5, conf.level=0.95, round.x=1, 
               ref.label="Median", main="", xlab=xlab, ylab="Log hazard ratio", xlim=c(a,b), xaxt = "n")
          title(main=main, adj=adj)
          axis(1, at = xticks, labels = format(round(xticks, 1), nsmall = 1))        
        }else{
          plot(hr_model, predictor=predictor, prob=0.5, conf.level=0.95, round.x=2, 
               ref.label="Median", main="", xlab=xlab, ylab="Log hazard ratio", xlim=c(a,b), xaxt = "n")
          title(main=main, adj=adj)
          axis(1, at = xticks, labels = format(round(xticks, 2), nsmall = 1))        
        }
        par(xpd = TRUE)
      }else{
        plot(hr_model, predictor=predictor, prob=0.5, conf.level=0.95, round.x=1,  
             ref.label="Median", main=main, ylab="Log hazard ratio", xlim=xlim)
      }
      if(!is.null(leg)){
        legend(pos, legend = leg, text.col=2, bty = "n")
      }
    } else{
      if(!is.null(xlab)){
        plot(hr_model, predictor=predictor, prob=0.5, conf.level=0.95, round.x=1, 
             ref.label="Median", main=main, xlab=xlab, ylab="Log hazard ratio")    
      }else{
        plot(hr_model, predictor=predictor, prob=0.5, conf.level=0.95, round.x=1, 
             ref.label="Median", main=main, ylab="Log hazard ratio")
      }
      if(!is.null(leg)){
        legend(pos, legend = leg, text.col=2, bty = "n")
      }
  }
}

compare_c_eta = function(data, R_acc, y_str, time_str, cov_str1,  eta=0:19/20, D=2,
                         alpha=0.05, round=4){
  p = length(eta)
  c_mat = matrix(NA, ncol = 8, nrow = (p+1))
  c_mat[1:p, 1] = eta
  mod1 = coxph(as.formula(paste0("Surv(",time_str ,",", y_str,") ~ ", cov_str1)), data = data)
  yhat1 = predict(mod1, type = "risk") 
  c_mat[(p+1),2:4] = cindex_ci(y = data$y, time = data$y_time, x=-yhat1, alpha=alpha)
  if(D==1) cov_str2 = "+ score1"
  if(D==2) cov_str2 = "+ score1 + score2"
  if(D==3) cov_str2 = "+ score1 + score2 + score3"
  if(D==4) cov_str2 = "+ score1 + score2 + score3 + score4"
  #Y = cbind(data$y_time,data$y); colnames(Y) = c("time", "event")
  for(i in 1:p){
    mod_acc = coxsplsDR(R_acc, time = data$y_time, event = data$y, ncomp = D, eta = eta[i], 
                                 scaleX = FALSE, scaleY = FALSE, allres = TRUE) 
    if(D==1){
      data$score1 = mod_acc$tt_splsDR[,1]
    } else if(D==2){
      data$score1 = mod_acc$tt_splsDR[,1]
      data$score2 = mod_acc$tt_splsDR[,2]
    } else if(D==3){
      data$score1 = mod_acc$tt_splsDR[,1]
      data$score2 = mod_acc$tt_splsDR[,2]
      data$score3 = mod_acc$tt_splsDR[,3]
    } else if(D==4){
      data$score1 = mod_acc$tt_splsDR[,1]
      data$score2 = mod_acc$tt_splsDR[,2]
      data$score3 = mod_acc$tt_splsDR[,3]
      data$score4 = mod_acc$tt_splsDR[,4]
    } 
    mod2 = coxph(as.formula(paste0("Surv(",time_str ,",", y_str,") ~ ", cov_str1,cov_str2)), data = data)
    yhat2 = predict(mod2, type = "risk")
    c_mat[i,2:4] = cindex_ci(y = data$y, time = data$y_time, x=-yhat2, alpha=alpha)
    c_mat[i,5:7] = cindex_delta(y = data$y, time = data$y_time, x=-yhat1, z=-yhat2, alpha=alpha)
    c_mat[i,8] = ifelse(c_mat[i,6]>0, 1, 0)
  }
  colnames(c_mat) = c("Eta", "C-index", "low CI", "up CI", "Delta C", "low CI", "up CI", "Sign")
  c_mat = round(c_mat, round)
  return(c_mat)
}

cindex_ci = function(y, time, x, alpha=0.05, data=NULL){
  n = length(y)
  z = rnorm(n)
  if(is.vector(X)==TRUE){
    c1   = compareC::compareC(timeX = time, statusX = y, scoreY = X, scoreZ = z) 
    m1   = c1$est.c[1]
    sd1  = sqrt(c1$est.varCxy) 
    low1 = m1 - qnorm(1-alpha/2)*sd1
    up1  = m1 + qnorm(1-alpha/2)*sd1
  }else{
    fit1 = coxph(Surv(time, y) ~ x)
    c1   = compareC(timeX = time, statusX = y, scoreY = -fit1$linear.predictors, scoreZ = z) 
    m1   = c1$est.c[1]
    sd1  = sqrt(c1$est.varCxy) 
    low1 = m1 - qnorm(1-alpha/2)*sd1
    up1  = m1 + qnorm(1-alpha/2)*sd1
  }
  R = c(m1, low1, up1)
  names(R) = c("C-index", "lower CI", "upper CI")
  return(R) 
}

cindex_delta = function(y, time, x, z, alpha=0.05){
  if(is.vector(x) & is.vector(z)){
    c1    = compareC::compareC(timeX = time, statusX = y, scoreY = x, scoreZ = z) 
    m1    = c1$est.c[1]
    m2    = c1$est.c[2]
    var1  = c1$est.varCxy
    var2  = c1$est.varCxz
    cov1  = c1$est.cov
    sd1   = sqrt(var1 + var2 - 2*cov1)
  }else{
    fit1 = survival::coxph(Surv(time, y) ~ x)
    fit2 = survival::coxph(Surv(time, y) ~ z)
    c1   = compareC(timeX = time, statusX = y, scoreY = -fit1$linear.predictors, scoreZ = -fit2$linear.predictors) 
    m1   = c1$est.c[1]
    m2   = c1$est.c[2]
    var1  = c1$est.varCxy
    var2  = c1$est.varCxz
    cov1  = c1$est.cov
    sd1  = sqrt(var1 + var2 - 2*cov1) 
  }
  delt  = m2 - m1 
  low1 = delt - qnorm(1-alpha/2)*sd1
  up1  = delt + qnorm(1-alpha/2)*sd1
  R = c(delt, low1, up1)
  names(R) = c("C-index", "lower CI", "upper CI")
  return(R) 
}

# ---------------------------------------------------------------------------- #
# Function to build a matrix using a core matrix  
build_matrix = function(list, mat){
  n = length(list)
  n1 = dim(mat)[1]
  p = dim(mat)[2]
  M = matrix(0, ncol = p, nrow = n)
  rownames(M) = list
  shortlist = rownames(mat)
  for(i in 1:n){
    if(list[i] %in% shortlist){
      for(j in 1:n1){
        if(shortlist[j]==list[i]) M[i,] = mat[j,]
      }
    }
  }
  return(as.matrix(M))
}

compare_HR_1_12 = function(mod1, mod2, alpha=0.05, round=3){
  H = matrix(NA, ncol = 4, nrow = 3)
  summary1 = summary(mod1)
  summary2 = summary(mod2)
  summary1 = summary1$coefficients
  summary2 = summary2$coefficients
  H[1,1] = exp(summary1[1,1])
  H[2,1] = exp(summary2[1,1])
  H[3,1] = exp(summary2[2,1])
  ig = qnorm(1-alpha/2)
  ci1 = c(summary1[1,1] - summary1[1,3]*ig, summary1[1,1] + summary1[1,3]*ig)
  ci2 = c(summary2[1,1] - summary2[1,3]*ig, summary2[1,1] + summary2[1,3]*ig)
  ci3 = c(summary2[2,1] - summary2[2,3]*ig, summary2[2,1] + summary2[2,3]*ig)
  ci1 = sort(exp(ci1))
  ci2 = sort(exp(ci2))
  ci3 = sort(exp(ci3))
  H[1,2:3] = ci1
  H[2,2:3] = ci2
  H[3,2:3] = ci3
  H[1,4] = summary1[1,5]
  H[2,4] = summary2[1,5]
  H[3,4] = summary2[2,5]
  H = round(H, round)
  colnames(H) = c("HR", "low CI", "up CI", "p-value")
  rownames(H) = c("M1 Score 1", "M2 Score 1", "M2 Score 2")
  return(H)
}

compare_models_123 = function(data, y_str, time_str, cov_str1, cov_str2, cov_str3, cov_list1, cov_list2, cov_list3, 
                              folloup=10*365, precision = 0.01, alpha=0.05, round=3, do_sens=TRUE){
  aic = rep(0, 3)
  se_sp_mat = matrix(0, ncol = 2, nrow = 3)
  c_mat = matrix(0, ncol = 3, nrow = 3) 
  c_delta1 = matrix(NA, ncol = 3, nrow = 3) 
  c_delta2 = matrix(NA, ncol = 3, nrow = 3) 
  mod1 = coxph(as.formula(paste0("Surv(",time_str ,",", y_str,") ~ ", cov_str1)), data = data)
  mod2 = coxph(as.formula(paste0("Surv(",time_str ,",", y_str,") ~ ", cov_str2)), data = data)
  mod3 = coxph(as.formula(paste0("Surv(",time_str ,",", y_str,") ~ ", cov_str3)), data = data)
  yhat1 = predict(mod1, type = "risk")
  yhat2 = predict(mod2, type = "risk")
  yhat3 = predict(mod3, type = "risk")
  aic[1] = extractAIC(mod1)[2]
  aic[2] = extractAIC(mod2)[2]
  aic[3] = extractAIC(mod3)[2]
  p1 = length(cov_list1) 
  p2 = length(cov_list2) 
  p3 = length(cov_list3) 
  n1  = length(yhat1) 
  n2  = length(yhat2) 
  n3  = length(yhat3)
  vec_y = (data %>% select(all_of(y_str)))[[1]]
  vec_t = (data %>% select(all_of(time_str)))[[1]]
  n = length(vec_y)
  covs1 = as.matrix(data %>% select(all_of(cov_list1)))
  covs1 = matrix(as.numeric(covs1), ncol = p1, nrow=n, byrow = FALSE)
  covs2 = as.matrix(data %>% select(all_of(cov_list2)))
  covs2 = matrix(as.numeric(covs2), ncol = p2, nrow=n, byrow = FALSE)
  covs3 = as.matrix(data %>% select(all_of(cov_list3)))
  covs3 = matrix(as.numeric(covs3), ncol = p3, nrow=n, byrow = FALSE)
  x1 = cbind(vec_t, vec_y, covs1) 
  x1 = na.omit(x1)
  x2 = cbind(vec_t, vec_y, covs2) 
  x2 = na.omit(x2)
  x3 = cbind(vec_t, vec_y, covs3) 
  x3 = na.omit(x3)
  vec_t1 = x1[,1]
  vec_y1 = x1[,2]
  covs1 = x1[,3:(2+p1)]
  vec_t2 = x2[,1]
  vec_y2 = x2[,2]
  covs2 = x2[,3:(2+p2)]
  vec_t3 = x3[,1]
  vec_y3 = x3[,2]
  covs3 = x3[,3:(2+p3)]
  if(do_sens == TRUE){
    se_sp_mat[1,] = f_youden(time=vec_t1, delta = vec_y1, marker = yhat1, folloup=folloup, precision = precision)
    se_sp_mat[2,] = f_youden(time=vec_t2, delta = vec_y2, marker = yhat2, folloup=folloup, precision = precision)
    se_sp_mat[3,] = f_youden(time=vec_t3, delta = vec_y3, marker = yhat3, folloup=folloup, precision = precision)    
  }
  c_mat[1,] = cindex_ci(y = vec_y1, time = vec_t1, x=covs1, alpha=alpha)
  c_mat[2,] = cindex_ci(y = vec_y2, time = vec_t2, x=covs2, alpha=alpha)
  c_mat[3,] = cindex_ci(y = vec_y3, time = vec_t3, x=covs3, alpha=alpha)
  c_delta1[2,] =cindex_delta(y = vec_y1, time = vec_t1, x=covs1, z=covs2, alpha=alpha)
  c_delta1[3,] =cindex_delta(y = vec_y2, time = vec_t2, x=covs1, z=covs3, alpha=alpha)
  c_delta2[3,] =cindex_delta(y = vec_y3, time = vec_t3, x=covs2, z=covs3, alpha=alpha)
  M = cbind(aic, se_sp_mat, c_mat, c_delta1, c_delta2)
  colnames(M) = c("AIC", "Sens.", "Spec.", "C-index", "low CI", "up CI", "Delta C", "low CI", "up CI", "Delta C", "low CI", "up CI")
  rownames(M) = c("Model 1", "Model 2", "Model 3")
  M = round(M, round)
  M[,1] = round(M[,1], 1)
  return(M)
}

f_youden = function(time, delta, marker, folloup, precision=0.001, cause=1, weighting = "marginal"){
  # Hassanzad, M., Hajian-Tilaki, K. Methods of determining optimal cut-point of diagnostic biomarkers with application of clinical data in ROC analysis: an update review. BMC Med Res Methodol 24, 84 (2024). https://doi.org/10.1186/s12874-024-02198-2
  # Youden WJ. An index for rating diagnostic tests. Cancer. 1950;3:32-35.
  
  # Blanche P, Dartigues JF, Jacqmin-Gadda H. Estimating and comparing time-dependent areas under receiver operating characteristic curves for censored event times with competing risks. Stat Med. 2013;32(30):5381-5397. doi:10.1002/sim.5958
  n = 1/precision
  min_c = quantile(marker, 0.05)
  max_c = quantile(marker, 0.95)
  cutpoints = seq(from=min_c, to=max_c, length.out=n) #1:(n-1)/n
  youden = rep(NA, n)
  for(i in 1:n){
    time_ROC = timeROC::SeSpPPVNPV(cutpoint=cutpoints[i], 
                                   T = time, 
                                   delta = delta, 
                                   marker = marker, 
                                   other_markers = NULL, 
                                   cause = cause,
                                   weighting = weighting, 
                                   times = folloup, 
                                   iid = FALSE)
    youden[i] = time_ROC$TP[2] + (1- time_ROC$FP)[2] - 1 # youden = sensitivity + specificity - 1
    se = time_ROC$TP[2]     # Youden's sensitivity
    sp = (1 - time_ROC$FP)[2] # Youden's specificity
  }
  max_youden = max(youden)
  max_c = cutpoints[which.max(youden)]
  
  final_ROC = timeROC::SeSpPPVNPV(max_c, 
                                  T = time, 
                                  delta = delta, 
                                  marker = marker, 
                                  other_markers = NULL, 
                                  cause = cause,
                                  weighting = weighting, 
                                  times = folloup, 
                                  iid = FALSE)
  se = final_ROC$TP[2]     # Youden's sensitivity
  sp = (1 - final_ROC$FP)[2] # Youden's specificity
  x = c(se,sp)
  colnames(c("se","sp"))
  return(x)
}

age_at_event = function(age, time, y, cov_restriction, unit=365.25){
  age_e = age +time/unit
  x = cbind(y,age_e, cov_restriction)
  final = x[complete.cases(x), ]
  n = dim(final)[1]
  final = final[final[,1] == 1,]
  m = round(mean(final[,2]),2)
  s = round(sd(final[,2]),2)
  return(list(n=n, mean=m, sd=s))
}
