covid_sim = function(duration = c(30, 50, 40), 
                     k = c(3, 120, 102),
                     mu = 10,
                     time_density,
                     time_bins,
                     print_message = TRUE) {
  
  
  # SIMIULATE PRE STAGE CASES
  tot_time = sum(duration)
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
  
  while (i < nrow(data_all)) {
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

