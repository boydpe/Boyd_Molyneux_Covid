# Peter Boyd
# Covid trump analysis
# Functions



# Libraries ---------------------------------------------------------------

library(readr)
library(tidyverse)
library(Rcpp)
library(knitr)
library(patchwork)
library(gridExtra)
library(stringr)


# ----------
# Data --------------------------------------------------------------------

state_sub_rally = readRDS("Data/state_data.Rds")
all_data = readRDS("Data/county_data.Rds")
rally = readRDS("Data/rally_edit.Rds")



# ----------
# rcpp files --------------------------------------------------------------

# function files
sourceCpp("functions/0_time_bins.cpp")
sourceCpp("functions/1_p0.cpp")
sourceCpp("functions/2_br.cpp")
sourceCpp("functions/4_update.cpp")
sourceCpp("functions/5_check.cpp")
sourceCpp("functions/get_rt.cpp")
sourceCpp("functions/denominator_p.cpp")
sourceCpp("functions/numerator_p.cpp")
sourceCpp("functions/p_divide.cpp")
sourceCpp("functions/get_prod.cpp")
sourceCpp("functions/2_br_ee.cpp")
sourceCpp("functions/3_trig_time_ee.cpp")
# simulation files
sourceCpp("functions/sim_k.cpp")
sourceCpp("functions/0_bins.cpp")
sourceCpp("functions/sim_kids.cpp")
sourceCpp("functions/sim_add_data.cpp")



# ----------
# MISD extraneous event ---------------------------------------------------

misd_ee = function(counties, rallies, time_bins = NULL, 
                   cut_time = 14, stopwhen = 1e-3,
                   loess_smooth = 0.20,
                   print_message = FALSE) {
  
  # GROUP DATA BY COUNTY
  nc = length(counties) #number of counties
  nrows = lapply(counties, nrow) # number of rows in each county
  for(i in 1:nc) {
    nrows[[i]] = nrows[[i]] - 1
  }
  
  ndays = list()
  for(i in 1:nc) {
    ndays[[i]] = as.numeric(
      difftime(counties[[i]]$date[length(counties[[i]]$date)],
               counties[[i]]$date[1], units = "days"))
  }
  
  # CREATE VARIABLE FOR DAILY NEW CASE NUMBERS
  # negative case changes set to 0
  for(i in 1:nc) {
    counties[[i]]$newcases = c(counties[[i]]$cases[1],
                               diff(counties[[i]]$cases))
  }
  counties = lapply(counties, function(x)
    transform(x, newcases = c(cases[1], diff(cases))))
  counties = lapply(counties, function(x)
    transform(x, newcases = ifelse(newcases<0, 0, newcases)))
  
  # use second day as start to accurately 
  # capture first new case value
  for(i in 1:nc){
    counties[[i]] = counties[[i]][-1,]
  }
  
  # CREATE DAY NUMBER SINCE FIRST CASE
  for(i in 1:nc) {
    counties[[i]]$time = c(1:length(counties[[i]]$date))
  }
  
  # KEEP ONLY THE DAYS(ROWS) WITH NEW CASES
  n_size = list()
  for(i in 1:nc) {
    counties[[i]] = counties[[i]][which(counties[[i]]$newcases > 0),]
    n_size[[i]] = nrow(counties[[i]])
  }
  
  # INITIALIZE P MATRIX FOR EACH COUNTY
  p0 = lapply(counties, function(x)
    init_p0(x$newcases))
  
  # DEFINE TIME BINS FOR EACH COUNTY
  if(is.null(time_bins) == TRUE){
    time_bins = lapply(ndays, function(x)
      c(0:x))
  }
  
  
  # PUT EACH EVENT PAIR INTO TIME DIFFERENCE BINS
  time_pairs = list()
  for(i in 1:nc) {
    time_pairs[[i]] = get_time_bins(times = counties[[i]]$time, 
                                    time_breaks = time_bins[[i]])
  }
  
  ### INITIALIZE LISTS
  
  # ee time is depending on assumption that each day has case.
  # ee time is just getting index, rather than matching number of days since start
  counties_pre = list()
  ee_time = list()
  n_pre = list()
  
  max_diff = list()
  n_iterations = list()
  br = list()
  gt = list()
  k_pre = list()
  k_2 = list()
  k_post = list()
  k_during = list()
  opt_length = list()
  k_all = list()
  pc = list()
  p_num = list()
  p_den = list()
  k_fit = list()
  increase_check = list()
  attempt = as.list(rep(0, nc))
  g_integral = list()
  g_frac = list()
  g_diff = list()
  slope_test = list()
  
  for (i in 1:nc) {
    # initialize max diff and n iterations
    max_diff[[i]] = 1
    n_iterations[[i]] = 0
    
    # get day number that ee occurs
    ee_time[[i]] = as.numeric(difftime(rallies$Date[i], 
                                       counties[[i]]$date[1], 
                                       units = "days"))
    counties_pre[[i]] = counties[[i]][which(
      counties[[i]]$time < ee_time[[i]]),]
    # find location (row number in p0) of ee date
    n_pre[[i]] = nrow(counties_pre[[i]])
  }
  
  # ITERATE
  for(i in 1:nc) {
    if(print_message == TRUE){
      print(paste("working on ", i))
    }
    md = c()
    kd = c()
    max_at = c()
    # i = 1
    while(max_diff[[i]] > stopwhen) { #} & n_iterations[[i]] < 500){
      
      # ATTEMPT TO WORK AROUND NONCONVERGING PROCESSES
      if(n_iterations[[i]] == 500) {
        counties[[i]] = counties[[i]][-1,]
        counties[[i]]$time = counties[[i]]$time - 1
        n_iterations[[i]] = 1
        attempt[[i]] = attempt[[i]] + 1
        p0[[i]] = init_p0(counties[[i]]$newcases)
      }
      # CALC BR OF EACH COUNTY, OVER ENTIRE PERIOD 
      br[[i]] = calc_br(p0[[i]], 
                        counties[[i]]$newcases,
                        ndays[[i]])#, ee_date[[i]])
      
      # CALCULATE G TRIGGERING FUNC EACH COUNTY, OVER ENTIRE PERIOD 
      # g is matrix of num, den
      g_temp = get_g_ee(p0 = p0[[i]], 
                        time_breaks = time_bins[[i]], 
                        times = counties[[i]]$time, 
                        cases = counties[[i]]$newcases)
      
      # get g in vector form and round
      g_frac[[i]] = g_temp[,1] / g_temp[,2]
      g_diff[[i]] = diff(time_bins[[i]])
      g_integral[[i]] = sum(g_diff[[i]] * g_frac[[i]])
      gt[[i]] = g_frac[[i]] / g_integral[[i]]
      # print(n_iterations[[i]])
      # print(g_frac[[i]])
      # print(gt[[i]])
      # print(g_integral[[i]])
      
      # CALC AVG COLSUMS PRE EE 
      # ks = offspring per row
      ks = (colSums(p0[[i]]) - diag(p0[[i]]))*
        (counties[[i]]$newcases * g_integral[[i]])
      
      # CALC K PRE EE 
      k_pre[[i]] = mean(ks[1:n_pre[[i]]]) #/
      
      # CHECK THAT CASES ACTUALLY INCREASE
      # changing from month after to two weeks after (then changed back to 2)
      post_data = counties[[i]][which((counties[[i]]$time > ee_time[[i]]) &
                                        (counties[[i]]$time < 2*ee_time[[i]])),]
      # James edit
      # increase_check[[i]] = t.test(counties[[i]]$newcases[1:n_pre[[i]]],
      #                              post_data$newcases,
      #                              alternative = "less")$p.value
      increase_check[[i]] = t.test(counties[[i]]$newcases[1:n_pre[[i]]],
                                   post_data$newcases,
                                   alternative = "two.sided")$p.value
      
      pre_slope = lm(counties[[i]]$newcases[1:n_pre[[i]]] ~ c(1:n_pre[[i]]))
      post_slope = lm(post_data$newcases ~ c(1:nrow(post_data)))
      
      t_val = (pre_slope$coefficients[2] - post_slope$coefficients[2]) /
        sqrt(
          (summary(pre_slope)$coef[4]^2 )/n_pre[[i]] + 
            (summary(post_slope)$coef[4]^2)/nrow(post_data))
      t_crit = qt(p = 0.05, df = nrow(post_data) -1)
      
      slope_test[[i]] = ifelse(t_val > t_crit, 1, 0)
      
      # may run into issues if empty days (no cases)
      
      
      # CALC AVG COLSUMS FOR VARIABLE DURING/POST LENGTHS
      # m = number of rows in during/post
      m = n_size[[i]] - n_pre[[i]] - cut_time # subtracted cut time 7/26
      k_both = matrix(nrow = m, ncol = 3, data = NA)
      # k_both[1,] = c(0,#during
      #                mean(ks[(n_pre[[i]]):(n_pre[[i]] + m)]), #post
      #                0) #m
      
      # avg colsums for during and post
      # dividing by number of new cases during period
      for(j in 1:m) {
        k_both[j,1] = mean(ks[(n_pre[[i]] + 1):(n_pre[[i]] + j)]) 
        k_both[j,2] = mean(ks[(n_pre[[i]] + 1 + j):(n_size[[i]] - cut_time)])
        k_both[j,3] = j
      }
      k_2[[i]] = k_both #k_both[1:(m),]
      
      # fit loess curve, find minimum of difference(min first derivative,
      # estimate of peak)
      k_fit[[i]] = loess(k_2[[i]][,1] ~ k_2[[i]][,3], 
                         span = loess_smooth)$fitted
      dk = diff(k_fit[[i]])
      opt_length[[i]] = which(diff(sign(dk)) == -2)[1]
      
      # opt is ideal number of days for ee to last
      if(is.na(opt_length[[i]]) == TRUE) {
        opt_length[[i]] = 0
      }
      
      if(increase_check[[i]] > 0.05) {
        opt_length[[i]] = 0
      }
      # print(opt_length[[i]])
      
      if(opt_length[[i]] == 0) {
        k_post[[i]] = mean(ks[(n_pre[[i]]):(n_pre[[i]] + m)])
        k_during[[i]] = 0
      } else {
        k_post[[i]] = k_2[[i]][opt_length[[i]] + 1,2]
        k_during[[i]] = k_2[[i]][opt_length[[i]] + 1,1]
      }
      
      k_all[[i]] = c(rep(k_pre[[i]], n_pre[[i]] - attempt[[i]]),
                     rep(k_during[[i]], opt_length[[i]]),
                     rep(k_post[[i]], n_size[[i]] -
                           n_pre[[i]] - opt_length[[i]]))
      
      
      p_num[[i]] = numerator_p(p0 = p0[[i]], g = gt[[i]], 
                               cases = counties[[i]]$newcases, 
                               br = br[[i]],
                               time_breaks = time_bins[[i]],
                               time_pairs = time_pairs[[i]], 
                               k = k_all[[i]])
      p_den[[i]] = denominator_p(p0 = p0[[i]], g = gt[[i]], 
                                 cases = counties[[i]]$newcases, 
                                 br = br[[i]],
                                 time_breaks = time_bins[[i]],
                                 time_pairs = time_pairs[[i]], 
                                 k = k_all[[i]])
      
      pc[[i]] = p_divide(p_num[[i]], p_den[[i]])
      
      max_diff[[i]] = check_p(p0 = p0[[i]], p = pc[[i]])
      ma = which.max(abs(p0[[i]] - pc[[i]]))
      n_iterations[[i]] = n_iterations[[i]] + 1
      # if(i == 2){
      # print(paste("n :", n_iterations[[i]]))
      # print(paste("diff: ", max_diff[[i]]))
      # print(paste("loc: ", as.numeric(which(p0[[i]] == max(p0[[i]], na.rm = TRUE), arr.ind = TRUE))))
      # }
      p0[[i]] = pc[[i]]
      md = c(md, max_diff[[i]])
      kd = c(kd, k_during[[i]])
      max_at = c(max_at, ma)
      
      if(attempt[[i]] == 6) {
        max_diff[[i]] = 0
        n_iterations[[i]] = 501
      }
    }
  }
  
  
  # OUTPUT
  perc_diag = list()
  for(i in 1:nc) {
    perc_diag[[i]] = sum(diag(p0[[i]])) / nrow(p0[[i]])
  }
  
  fail = c()
  for(i in 1:nc){
    fail[i] = ifelse(n_iterations[[i]] < 500, 0, 1)
  }
  
  # for(i in 1:nc) {
  #   if(increase_check[[i]] > 0.05) {
  #     opt_length[[i]] = 0
  #   }
  # }
  
  out = list(p0 = p0, g = gt, n_iterations = n_iterations,
             time_bins = time_bins, 
             perc_diag = perc_diag, k = k_all,
             opt_length = opt_length, br = br, ndays = ndays,
             data = counties, 
             ee_time = ee_time, k_2 = k_2, k_fit = k_fit, fail = fail, 
             increase_check = increase_check, attempt = attempt, 
             g_integral = g_integral, slope_test = slope_test)
  return(out)
  
}


# ----------
# MISD extraneous event state level ---------------------------------------


misd_ee_state = function(counties, rallies, time_bins = NULL, 
                         cut_time = 14, stopwhen = 1e-3, set_length,
                         loess_smooth = 0.20) {
  
  # GROUP DATA BY COUNTY
  nc = length(counties) #number of counties
  nrows = lapply(counties, nrow) # number of rows in each county
  for(i in 1:nc) {
    nrows[[i]] = nrows[[i]] - 1
  }
  
  ndays = list()
  for(i in 1:nc) {
    ndays[[i]] = as.numeric(
      difftime(counties[[i]]$date[length(counties[[i]]$date)],
               counties[[i]]$date[1], units = "days"))
  }
  
  
  # CREATE VARIABLE FOR DAILY NEW CASE NUMBERS
  # negative case changes set to 0
  for(i in 1:nc) {
    counties[[i]]$newcases = c(counties[[i]]$cases_tot[1],
                               diff(counties[[i]]$cases_tot))
  }
  # counties = lapply(counties, function(x)
  #   transform(x, newcases = c(cases[1], diff(cases))))
  counties = lapply(counties, function(x)
    transform(x, newcases = ifelse(newcases<0, 0, newcases)))
  
  # use second day as start to accurately 
  # capture first new case value
  for(i in 1:nc){
    counties[[i]] = counties[[i]][-1,]
  }
  
  # CREATE DAY NUMBER SINCE FIRST CASE
  for(i in 1:nc) {
    counties[[i]]$time = c(1:length(counties[[i]]$date))
  }
  
  # KEEP ONLY THE DAYS(ROWS) WITH NEW CASES
  n_size = list()
  for(i in 1:nc) {
    counties[[i]] = counties[[i]][which(counties[[i]]$newcases > 0),]
    n_size[[i]] = nrow(counties[[i]])
  }
  
  # INITIALIZE P MATRIX FOR EACH COUNTY
  p0 = lapply(counties, function(x)
    init_p0(x$newcases))
  
  # DEFINE TIME BINS FOR EACH COUNTY
  if(is.null(time_bins) == TRUE){
    time_bins = lapply(ndays, function(x)
      c(0:x))
  }
  
  
  # PUT EACH EVENT PAIR INTO TIME DIFFERENCE BINS
  time_pairs = list()
  for(i in 1:nc) {
    time_pairs[[i]] = get_time_bins(times = counties[[i]]$time, 
                                    time_breaks = time_bins[[i]])
  }
  
  ### INITIALIZE LISTS
  
  # ee time is depending on assumption that each day has case.
  # ee time is just getting index, rather than matching number of days since start
  counties_pre = list()
  ee_time = list()
  n_pre = list()
  
  max_diff = list()
  n_iterations = list()
  br = list()
  gt = list()
  k_pre = list()
  k_2 = list()
  k_post = list()
  k_during = list()
  opt_length = list()
  k_all = list()
  pc = list()
  p_num = list()
  p_den = list()
  k_fit = list()
  increase_check = list()
  g_frac = list()
  g_diff = list()
  g_integral = list()
  attempt = as.list(rep(0, nc))
  
  
  for (i in 1:nc) {
    # initialize max diff and n iterations
    max_diff[[i]] = 1
    n_iterations[[i]] = 0
    
    # get day number that ee occurs
    ee_time[[i]] = as.numeric(difftime(rallies$Date[i], 
                                       counties[[i]]$date[1], 
                                       units = "days"))
    counties_pre[[i]] = counties[[i]][which(
      counties[[i]]$time < ee_time[[i]]),]
    # find location (row number in p0) of ee date
    n_pre[[i]] = nrow(counties_pre[[i]])
  }
  
  # ITERATE
  for(i in 1:nc) {
    while(max_diff[[i]] > stopwhen) { #} & n_iterations[[i]] < 500){
      
      # ATTEMPT TO WORK AROUND NONCONVERGING PROCESSES
      if(n_iterations[[i]] == 500) {
        counties[[i]] = counties[[i]][-1,]
        counties[[i]]$time = counties[[i]]$time - 1
        n_iterations[[i]] = 1
        attempt[[i]] = attempt[[i]] + 1
        p0[[i]] = init_p0(counties[[i]]$newcases)
      }
      
      # CALC BR OF EACH COUNTY, OVER ENTIRE PERIOD 
      br[[i]] = calc_br(p0[[i]], 
                        counties[[i]]$newcases,
                        ndays[[i]])#, ee_date[[i]])
      
      # CALCULATE G TRIGGERING FUNC EACH COUNTY, OVER ENTIRE PERIOD 
      # g is matrix of num, den
      g_temp = get_g_ee(p0 = p0[[i]], time_breaks = time_bins[[i]], 
                        times = counties[[i]]$time, 
                        cases = counties[[i]]$newcases)
      
      # get g in vector form and round
      g_frac[[i]] = g_temp[,1] / g_temp[,2]
      g_diff[[i]] = diff(time_bins[[i]])
      g_integral[[i]] = sum(g_diff[[i]] * g_frac[[i]])
      gt[[i]] = g_frac[[i]] / g_integral[[i]]
      
      # CALC AVG COLSUMS PRE EE 
      # ks = offspring per row
      ks = (colSums(p0[[i]]) - diag(p0[[i]]))*
        (counties[[i]]$newcases * g_integral[[i]])      
      # CALC K PRE EE  
      k_pre[[i]] = mean(ks[1:n_pre[[i]]]) #/
      
      # CHECK THAT CASES ACTUALLY INCREASE
      # changing from month after to two weeks after
      
      # zzq: don't need for state
      # post_data = counties[[i]][which((counties[[i]]$time > ee_time[[i]]) &
      #                                   (counties[[i]]$time < 1.5*ee_time[[i]])),]
      # increase_check[[i]] = t.test(counties[[i]]$newcases[1:n_pre[[i]]],
      #                              post_data$newcases,
      #                              alternative = "less")$p.value
      
      # CALC AVG COLSUMS FOR VARIABLE DURING/POST LENGTHS
      # m = number of rows in during/post
      m = n_size[[i]] - n_pre[[i]] - cut_time # subtracted cut time 7/26
      k_both = matrix(nrow = m, ncol = 3, data = NA)
      # k_both[1,] = c(0,#during
      #                mean(ks[(n_pre[[i]]):(n_pre[[i]] + m)]), #post
      #                0) #m
      
      # avg colsums for during and post
      # dividing by number of new cases during period
      for(j in 1:m) {
        k_both[j,1] = mean(ks[(n_pre[[i]] + 1):(n_pre[[i]] + j)]) 
        k_both[j,2] = mean(ks[(n_pre[[i]] + 1 + j):(n_size[[i]] - cut_time)])
        k_both[j,3] = j
      }
      k_2[[i]] = k_both #k_both[1:(m),]
      
      # fit loess curve, find minimum of difference(min first derivative,
      # estimate of peak)
      k_fit[[i]] = loess(k_2[[i]][,1] ~ k_2[[i]][,3], 
                         span = loess_smooth)$fitted
      dk = diff(k_fit[[i]])
      # zzq: opt_length[[i]] = which(diff(sign(dk)) == -2)[1]
      opt_length[[i]] = set_length[[i]]
      
      # opt is ideal number of days for ee to last
      if(is.na(opt_length[[i]]) == TRUE) {
        opt_length[[i]] = 0
      }
      
      # zzq: if(increase_check[[i]] > 0.05) {
      #   opt_length[[i]] = 1
      # }
      
      if(opt_length[[i]] == 0) {
        k_post[[i]] = mean(ks[(n_pre[[i]]):(n_pre[[i]] + m)])
        k_during[[i]] = 0
      } else {
        k_post[[i]] = k_2[[i]][opt_length[[i]] + 1,2]
        k_during[[i]] = k_2[[i]][opt_length[[i]] + 1,1]
      }
      
      # k_all[[i]] = c(rep(k_pre[[i]], n_pre[[i]]),
      #                rep(k_during[[i]], opt_length[[i]]),
      #                rep(k_post[[i]], n_size[[i]] -
      #                      n_pre[[i]] - opt_length[[i]]))
      # k_all below is with the attempt update
      
      k_all[[i]] = c(rep(k_pre[[i]], n_pre[[i]] - attempt[[i]]),
                     rep(k_during[[i]], opt_length[[i]]),
                     rep(k_post[[i]], n_size[[i]] -
                           n_pre[[i]] - opt_length[[i]]))
      
      
      p_num[[i]] = numerator_p(p0 = p0[[i]], g = gt[[i]], 
                               cases = counties[[i]]$newcases, 
                               br = br[[i]],
                               time_breaks = time_bins[[i]],
                               time_pairs = time_pairs[[i]], 
                               k = k_all[[i]])
      p_den[[i]] = denominator_p(p0 = p0[[i]], g = gt[[i]], 
                                 cases = counties[[i]]$newcases, 
                                 br = br[[i]],
                                 time_breaks = time_bins[[i]],
                                 time_pairs = time_pairs[[i]], 
                                 k = k_all[[i]])
      
      pc[[i]] = p_divide(p_num[[i]], p_den[[i]])
      
      max_diff[[i]] = check_p(p0 = p0[[i]], p = pc[[i]])
      n_iterations[[i]] = n_iterations[[i]] + 1
      p0[[i]] = pc[[i]]
      
      if(attempt[[i]] == 6) {
        max_diff[[i]] = 0
        n_iterations[[i]] = 501
      }
    }
  }
  
  
  # OUTPUT
  perc_diag = list()
  for(i in 1:nc) {
    perc_diag[[i]] = sum(diag(p0[[i]])) / nrow(p0[[i]])
  }
  
  fail = c()
  for(i in 1:nc){
    fail[i] = ifelse(n_iterations[[i]] < 500, 0, 1)
  }
  
  out = list(p0 = p0, g = gt, n_iterations = n_iterations,
             time_bins = time_bins, 
             perc_diag = perc_diag, k = k_all,
             opt_length = opt_length, br = br, ndays = ndays,
             data = counties, 
             ee_time = ee_time, k_2 = k_2, k_fit = k_fit, fail = fail, 
             increase_check = increase_check)
  return(out)
  
}

misd_ee_state_1 = function(counties, rallies, time_bins = NULL, 
                           cut_time = 14, stopwhen = 1e-3, set_length) {
  
  # GROUP DATA BY COUNTY
  nc = length(counties) #number of counties
  nrows = lapply(counties, nrow) # number of rows in each county
  for(i in 1:nc) {
    nrows[[i]] = nrows[[i]] - 1
  }
  
  ndays = list()
  for(i in 1:nc) {
    ndays[[i]] = as.numeric(
      difftime(counties[[i]]$date[length(counties[[i]]$date)],
               counties[[i]]$date[1], units = "days"))
  }
  
  
  # CREATE VARIABLE FOR DAILY NEW CASE NUMBERS
  # negative case changes set to 0
  for(i in 1:nc) {
    counties[[i]]$newcases = c(counties[[i]]$cases_tot[1],
                               diff(counties[[i]]$cases_tot))
  }
  # counties = lapply(counties, function(x)
  #   transform(x, newcases = c(cases[1], diff(cases))))
  counties = lapply(counties, function(x)
    transform(x, newcases = ifelse(newcases<0, 0, newcases)))
  
  # use second day as start to accurately 
  # capture first new case value
  for(i in 1:nc){
    counties[[i]] = counties[[i]][-1,]
  }
  
  # CREATE DAY NUMBER SINCE FIRST CASE
  for(i in 1:nc) {
    counties[[i]]$time = c(1:length(counties[[i]]$date))
  }
  
  # KEEP ONLY THE DAYS(ROWS) WITH NEW CASES
  n_size = list()
  for(i in 1:nc) {
    counties[[i]] = counties[[i]][which(counties[[i]]$newcases > 0),]
    n_size[[i]] = nrow(counties[[i]])
  }
  
  # INITIALIZE P MATRIX FOR EACH COUNTY
  p0 = lapply(counties, function(x)
    init_p0(x$newcases))
  
  # DEFINE TIME BINS FOR EACH COUNTY
  if(is.null(time_bins) == TRUE){
    time_bins = lapply(ndays, function(x)
      c(0:x))
  }
  
  
  # PUT EACH EVENT PAIR INTO TIME DIFFERENCE BINS
  time_pairs = list()
  for(i in 1:nc) {
    time_pairs[[i]] = get_time_bins(times = counties[[i]]$time, 
                                    time_breaks = time_bins[[i]])
  }
  
  ### INITIALIZE LISTS
  
  # ee time is depending on assumption that each day has case.
  # ee time is just getting index, rather than matching number of days since start
  counties_pre = list()
  ee_time = list()
  n_pre = list()
  
  max_diff = list()
  n_iterations = list()
  br = list()
  gt = list()
  k_pre = list()
  k_2 = list()
  k_post = list()
  k_during = list()
  opt_length = list()
  k_all = list()
  pc = list()
  p_num = list()
  p_den = list()
  k_fit = list()
  increase_check = list()
  g_frac = list()
  g_diff = list()
  g_integral = list()
  
  for (i in 1:nc) {
    # initialize max diff and n iterations
    max_diff[[i]] = 1
    n_iterations[[i]] = 0
    
    # get day number that ee occurs
    ee_time[[i]] = as.numeric(difftime(rallies$Date[i], 
                                       counties[[i]]$date[1], 
                                       units = "days"))
    counties_pre[[i]] = counties[[i]][which(
      counties[[i]]$time < ee_time[[i]]),]
    # find location (row number in p0) of ee date
    n_pre[[i]] = nrow(counties_pre[[i]])
  }
  
  # ITERATE
  for(i in 1:nc) {
    while(max_diff[[i]] > stopwhen & n_iterations[[i]] < 500){
      
      # CALC BR OF EACH COUNTY, OVER ENTIRE PERIOD 
      br[[i]] = calc_br(p0[[i]], 
                        counties[[i]]$newcases,
                        ndays[[i]])#, ee_date[[i]])
      
      # CALCULATE G TRIGGERING FUNC EACH COUNTY, OVER ENTIRE PERIOD 
      # g is matrix of num, den
      g_temp = get_g_ee(p0 = p0[[i]], time_breaks = time_bins[[i]], 
                        times = counties[[i]]$time, 
                        cases = counties[[i]]$newcases)
      
      # get g in vector form and round
      g_frac[[i]] = g_temp[,1] / g_temp[,2]
      g_diff[[i]] = diff(time_bins[[i]])
      g_integral[[i]] = sum(g_diff[[i]] * g_frac[[i]])
      gt[[i]] = g_frac[[i]] / g_integral[[i]]
      
      # CALC AVG COLSUMS PRE EE 
      # ks = offspring per row
      ks = (colSums(p0[[i]]) - diag(p0[[i]]))*
        (counties[[i]]$newcases * g_integral[[i]])      
      # CALC K PRE EE  
      k_pre[[i]] = mean(ks[1:n_pre[[i]]]) #/
      
      # CHECK THAT CASES ACTUALLY INCREASE
      # changing from month after to two weeks after
      
      # zzq: don't need for state
      # post_data = counties[[i]][which((counties[[i]]$time > ee_time[[i]]) &
      #                                   (counties[[i]]$time < 1.5*ee_time[[i]])),]
      # increase_check[[i]] = t.test(counties[[i]]$newcases[1:n_pre[[i]]],
      #                              post_data$newcases,
      #                              alternative = "less")$p.value
      
      # CALC AVG COLSUMS FOR VARIABLE DURING/POST LENGTHS
      # m = number of rows in during/post
      m = n_size[[i]] - n_pre[[i]] - cut_time # subtracted cut time 7/26
      k_both = matrix(nrow = m, ncol = 2, data = NA)
      
      # avg colsums for during and post
      # dividing by number of new cases during period
      for(j in 1:m) {
        k_both[j,1] = mean(ks[(n_pre[[i]] + 1):(n_pre[[i]] + j)]) 
        k_both[j,2] = mean(ks[(n_pre[[i]] + 1 + j):(n_size[[i]] - cut_time)]) 
      }
      k_2[[i]] = k_both[1:(m),]
      
      # fit loess curve, find minimum of difference(min first derivative,
      # estimate of peak)
      k_fit[[i]] = loess(k_2[[i]][,1] ~ c(1:length(k_2[[i]][,1])), 
                         span = 0.15)$fitted
      dk = diff(k_fit[[i]])
      # zzq: opt_length[[i]] = which(diff(sign(dk)) == -2)[1]
      opt_length[[i]] = set_length[[i]]
      
      # opt is ideal number of days for ee to last
      if(is.na(opt_length[[i]]) == TRUE) {
        opt_length[[i]] = 1
      }
      
      # zzq: if(increase_check[[i]] > 0.05) {
      #   opt_length[[i]] = 1
      # }
      
      k_post[[i]] = k_2[[i]][opt_length[[i]],2]
      k_during[[i]] = k_2[[i]][opt_length[[i]],1]
      
      k_all[[i]] = c(rep(k_pre[[i]], n_pre[[i]]),
                     rep(k_during[[i]], opt_length[[i]]),
                     rep(k_post[[i]], n_size[[i]] -
                           n_pre[[i]] - opt_length[[i]]))
      
      
      p_num[[i]] = numerator_p(p0 = p0[[i]], g = gt[[i]], 
                               cases = counties[[i]]$newcases, 
                               br = br[[i]],
                               time_breaks = time_bins[[i]],
                               time_pairs = time_pairs[[i]], 
                               k = k_all[[i]])
      p_den[[i]] = denominator_p(p0 = p0[[i]], g = gt[[i]], 
                                 cases = counties[[i]]$newcases, 
                                 br = br[[i]],
                                 time_breaks = time_bins[[i]],
                                 time_pairs = time_pairs[[i]], 
                                 k = k_all[[i]])
      
      pc[[i]] = p_divide(p_num[[i]], p_den[[i]])
      
      max_diff[[i]] = check_p(p0 = p0[[i]], p = pc[[i]])
      n_iterations[[i]] = n_iterations[[i]] + 1
      p0[[i]] = pc[[i]]
      
    }
  }
  
  
  # OUTPUT
  perc_diag = list()
  for(i in 1:nc) {
    perc_diag[[i]] = sum(diag(p0[[i]])) / nrow(p0[[i]])
  }
  
  fail = c()
  for(i in 1:nc){
    fail[i] = ifelse(n_iterations[[i]] < 500, 0, 1)
  }
  
  out = list(p0 = p0, g = gt, n_iterations = n_iterations,
             time_bins = time_bins, 
             perc_diag = perc_diag, k = k_all,
             opt_length = opt_length, br = br, ndays = ndays,
             data = counties, 
             ee_time = ee_time, k_2 = k_2, k_fit = k_fit, fail = fail, 
             increase_check = increase_check)
  return(out)
  
}


# ----------
# Simulation --------------------------------------------------------------

covid_sim = function(duration = c(30, 50, 40), 
                     k = c(3, 120, 102),
                     mu = 10,
                     time_density,
                     time_bins,
                     print_message = TRUE) {
  #burn_in = 10) {
  
  
  # SIMIULATE PRE STAGE CASES
  tot_time = sum(duration) #+ burn_in
  # add burn in period to duration vector
  # duration[1] = duration[1] + burn_in
  n_parent = rpois(1, lambda = mu * tot_time)
  data_parent = data.frame(
    t = rep(NA, n_parent),
    gen = rep(0, n_parent)
  )  
  data_parent$t = runif(n_parent, 0, tot_time)
  data_parent$day = ceiling(data_parent$t)
  data_parent = data_parent[order(data_parent$t),]
  data_parent$ref_num = c(1:n_parent)
  data_parent$parent = c(1:n_parent)
  data_parent$n_child = 0
  
  # Convert to matrix for rcpp ease
  # cols: c("t", "gen", "day", "ref_num", "parent", "n_child")
  data_all = unname(as.matrix(data_parent))
  i = 1
  n_events = nrow(data_all)
  k_val = 0
  g_t = c(0, cumsum(diff(time_bins) * time_density))
  current_time = data_all[1,1]
  
  # James edit:
  # while (current_time < tot_time) {
  # set.seed(333)
  while (i < nrow(data_all)) {
    # print(nrow(data_all) - i)
    
    k_val = pick_k(current_time, k, duration)
    
    ## draw number of children per parent
    n.child = rpois(1, k_val) # / mu?
    data_all[i,6] = n.child
    # n.child is random number drawn
    # n_child is data frame value
    if (n.child != 0) {
      
      # draw random point then place it in a time bin
      u = runif(n.child, 0, 1)
      whichbin = bins(u, g_t)
      data_all = update_df(parent_time = current_time,
                           parent_gen = data_all[i,2],
                           parent_num = data_all[i,4],
                           max_num = max(data_all[,4]),
                           n_child = n.child,
                           whichbin = whichbin,
                           time_bins = time_bins,
                           data_old = data_all,
                           n = n_events)
      
      data_all = data_all[order(data_all[,1]),]
    }
    
    n_events = nrow(data_all)
    i = i + 1
    current_time = data_all[i,1]
    if(print_message == TRUE){
      if(i %% 250 == 0) {
        print(current_time)
        print(i)
      }
    }
  }
  
  data_all = as.data.frame(data_all)
  names(data_all) = c("t", "gen", "day",
                      "ref_num", "parent", "n_child")
  
  daily_data = data_all %>% 
    group_by(day) %>% 
    summarize(cases = n())
  
  out = list(daily_data = daily_data,
             case_data = data_all)
  return(out)
}


# ----------
# Other simulation --------------------------------------------------------

# covid_sim1 = function(duration = c(30, 50, 40), 
#                       k = c(3, 120, 102),
#                       params = list(
#                         A = 0.1977,
#                         p = 1.102,
#                         c = 0.01903,
#                         mu = 0.5*100,
#                         b = 0.5),
#                       time_density,
#                       time_bins,
#                       print_message = TRUE) {
#   
#   # SIMIULATE PRE STAGE CASES
#   tot_time = sum(duration)
#   n_parent = rpois(1, lambda = params$mu * tot_time)
#   data_parent = data.frame(
#     t = rep(NA, n_parent),
#     gen = rep(0, n_parent)
#   )  
#   data_parent$t = runif(n_parent, 0, tot_time)
#   data_parent$day = ceiling(data_parent$t)
#   data_parent = data_parent[order(data_parent$t),]
#   data_parent$ref_num = c(1:n_parent)
#   data_parent$parent = c(1:n_parent)
#   data_parent$n_child = 0
#   
#   # Convert to matrix for rcpp ease
#   # cols: c("t", "gen", "day", "ref_num", "parent", "n_child")
#   data_all = unname(as.matrix(data_parent))
#   i = 1
#   n_events = nrow(data_all)
#   k_val = 0
#   g_t = c(0, cumsum(diff(time_bins) * time_density))
#   current_time = data_all[1,1]
#   set.seed(333)
#   while (current_time < tot_time) {
#     
#     k_val = pick_k(current_time, k, duration)
#     
#     #  method with nchild depending on time
#     prod = k_val * time_density
#     m = length(time_density)
#     n.child = rpois(m, prod)#/params$mu)
#     
#     ## draw number of children per parent
#     data_all[i,6] = sum(n.child)
#     # place all points in a new data frame
#     if (sum(n.child) != 0) {
#       data_all = add_data(data_old = data_all,
#                           time_bins = time_bins,
#                           n_child = n.child,
#                           parent_time = current_time,
#                           parent_gen = data_all[i,2],
#                           parent_num = data_all[i,4],
#                           max_num = max(data_all[,4]),
#                           n_events = n_events)
#       
#       # reorder by time
#       data_all = data_all[order(data_all[,1]),]
#     }
#     
#     n_events = nrow(data_all)
#     i = i + 1
#     current_time = data_all[i,1]
#     if(i %% 250 == 0) {
#       print(current_time)
#       print(i)
#     }
#   }
#   
#   data_all = as.data.frame(data_all)
#   names(data_all) = c("t", "gen", "day",
#                       "ref_num", "parent", "n_child")
#   
#   daily_data = data_all %>% 
#     group_by(day) %>% 
#     summarize(cases = n())
#   
#   out = list(daily_data = daily_data,
#              case_data = data_all)
#   return(out)
# }


# ----------
# Old code ----------------------------------------------------------------

# data_child = data.frame(
#   time = rep(NA, n.child),
#   gen = rep(NA, n.child),
#   parent = rep(NA, n.child),
#   ref_num = rep(NA, n.child),
#   day = rep(NA, n.child),
#   n_child = rep(NA, n.child)
# )

# for (j in 1:n.child) {
#   u = runif(1, 0, 1)
#   whichbin = which(u < cumsum(diff(time_bins) * time_density))[1]
#   time_child = runif(1, time_bins[whichbin], time_bins[whichbin + 1])
#   data_child$time[j] = data_all$time[i] + time_child
#   data_child$day[j] = ceiling(data_child$time[j])
#   data_child$gen[j] = data_all$gen[i] + 1
#   data_child$parent[j] = data_all$ref_num[i]
#   data_child$ref_num[j] = max(data_all$ref_num) + j
#   # data_child$n_child[j] = n_child
# }

# if(print_message == TRUE) {
#   print(paste(
#     ": simulating ",  n.child, "children for event ", i
#   ))
# }

# if(current_time <= duration[1]) {
#   k_val = k[1]
# } else if(current_time <= sum(duration[1:2]) &
#           current_time > duration[1]) {
#   k_val = k[2]
# } else if(current_time <= sum(duration[1:3]) &
#           current_time > sum(duration[1:2])){
#   k_val = k[3]
# } else {
#   print("incorrect k and duration dimensions")
# }
