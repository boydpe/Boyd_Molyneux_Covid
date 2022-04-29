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
    while(max_diff[[i]] > stopwhen) {
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
                        ndays[[i]])
      
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
      
      # CALC AVG COLSUMS PRE EE 
      # ks = offspring per row
      ks = (colSums(p0[[i]]) - diag(p0[[i]]))*
        (counties[[i]]$newcases * g_integral[[i]])
      
      # CALC K PRE EE 
      k_pre[[i]] = mean(ks[1:n_pre[[i]]]) 
      
      # CHECK THAT CASES ACTUALLY INCREASE
      post_data = counties[[i]][which((counties[[i]]$time > ee_time[[i]]) &
                                        (counties[[i]]$time < 2*ee_time[[i]])),]
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
      
      # CALC AVG COLSUMS FOR VARIABLE DURING/POST LENGTHS
      # m = number of rows in during/post
      m = n_size[[i]] - n_pre[[i]] - cut_time # subtracted cut time 7/26
      k_both = matrix(nrow = m, ncol = 3, data = NA)
      
      # avg colsums for during and post
      # dividing by number of new cases during period
      for(j in 1:m) {
        k_both[j,1] = mean(ks[(n_pre[[i]] + 1):(n_pre[[i]] + j)]) 
        k_both[j,2] = mean(ks[(n_pre[[i]] + 1 + j):(n_size[[i]] - cut_time)])
        k_both[j,3] = j
      }
      k_2[[i]] = k_both
      
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
    while(max_diff[[i]] > stopwhen) {
      
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
                        ndays[[i]])
      
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
      k_pre[[i]] = mean(ks[1:n_pre[[i]]])
      
      # CALC AVG COLSUMS FOR VARIABLE DURING/POST LENGTHS
      # m = number of rows in during/post
      m = n_size[[i]] - n_pre[[i]] - cut_time # subtracted cut time 7/26
      k_both = matrix(nrow = m, ncol = 3, data = NA)
      
      # avg colsums for during and post
      # dividing by number of new cases during period
      for(j in 1:m) {
        k_both[j,1] = mean(ks[(n_pre[[i]] + 1):(n_pre[[i]] + j)]) 
        k_both[j,2] = mean(ks[(n_pre[[i]] + 1 + j):(n_size[[i]] - cut_time)])
        k_both[j,3] = j
      }
      k_2[[i]] = k_both
      
      # fit loess curve, find minimum of difference(min first derivative,
      # estimate of peak)
      k_fit[[i]] = loess(k_2[[i]][,1] ~ k_2[[i]][,3], 
                         span = loess_smooth)$fitted
      dk = diff(k_fit[[i]])
      opt_length[[i]] = set_length[[i]]
      
      # opt is ideal number of days for ee to last
      if(is.na(opt_length[[i]]) == TRUE) {
        opt_length[[i]] = 0
      }
      
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
