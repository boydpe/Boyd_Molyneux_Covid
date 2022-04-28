# Results table for county results
results_df = data.frame(county = rep(NA,n), state = rep(NA,n),
                        pop = rep(NA, n),
                        date = rep(NA, n), fail = rep(NA,n),
                        br = rep(NA,n), duration = rep(NA,n),  
                        k_before = rep(NA,n), k_during = rep(NA,n), 
                        #k_after = rep(NA,n), 
                        #impact = rep(NA,n), 
                        # ttest = rep(NA,n),  
                        # ratio = rep(NA,n), 
                        cases = rep(NA,n),
                        k_diff = rep(NA,n))
# use state abbreviations
rally$state_abbr = rep(NA,n)
abbr = c("AZ", "FL", "GA", 
         "IA", "MI", "MN", 
         "NE", "NV", "NH", "NC", 
         "OH", "OK", "PA", "WI")
non_abbr = c("Arizona", "Florida", "Georgia", 
             "Iowa", "Michigan", "Minnesota", 
             "Nebraska", "Nevada", "New Hampshire", 
             "North Carolina", "Ohio", "Oklahoma", 
             "Pennsylvania", "Wisconsin")
for(i in 1:n){
  for(j in 1:length(non_abbr)) {
    if(rally$State[i] == non_abbr[j]) {
      rally$state_abbr[i] = abbr[j]
    }
  }
}

# populate table
# TRY REMOVING DIVISION OF COUNTY/STATE BEFORE VALUE IN K_DIFF
# county.x for some reason?
for(i in 1:n) {
  results_df$county[i] = out_all$data[[i]]$county.x[1]
  results_df$state[i] = rally$state_abbr[i]
  results_df$pop[i] = out_all$data[[i]]$POPESTIMATE2020[1]
  results_df$duration[i] = out_all$opt_length[[i]]
  results_df$fail[i] = out_all$fail[[i]]
  results_df$k_before[i] = round(unique(out_all$k[[i]])[1],2)
  results_df$k_during[i] = round(unique(out_all$k[[i]])[2],2)
  #results_df$k_after[i] = round(unique(out_all$k[[i]])[3],2)
  # results_df$impact[i] = round((unique(out_all$k[[i]])[2] - 
  # unique(out_all$k[[i]])[1]) / 
  # unique(out_all$k[[i]])[1], 2)
  # results_df$ttest[i] = round(out_all$increase_check[[i]],2)
  results_df$br[i] = round(out_all$br[[i]], 2)
  # results_df$ratio[i] = round(ratios$comparison[i],2)
  results_df$k_diff[i] = round((unique(out_all$k[[i]])[2] - unique(out_all$k[[i]])[1])/(out_all$data[[i]]$POPESTIMATE2020[1]) - 
                                 (unique(out_state$k[[i]])[2] - unique(out_state$k[[i]])[1])/(out_state$data[[i]]$pop_tot[1]), 5)
  results_df$cases[i] = round(results_df$k_diff[i] * out_all$data[[i]]$POPESTIMATE2020[1]*results_df$duration[i])
  #round((results_df$k_during[i] - results_df$k_before[i]) *  results_df$duration[i])
}
results_df$date = rally$Date
#write.csv(file = "results1.csv", results_df)

# make counties with no evidence of increase last NA duration
# results_df$duration = ifelse(results_df$duration == 1, NA, results_df$duration)
results_df$cases = ifelse(results_df$cases < 0, 0, results_df$cases)

# James edit:
results_df <- results_df %>% 
  # Set k^* to zero when duration == 0
  mutate(k_during = ifelse(duration == 0, 0, k_during))

# create success and failure tables
success_df = results_df %>% filter(is.na(duration) == FALSE & fail == 0 & cases > 0)
fail_df = results_df %>% filter(is.na(duration) == TRUE | fail == 1 | cases == 0)
#   results_df[which(is.na(results_df$duration) == FALSE) &
#                           results_df$fail == 0,]
# fail_df = results_df[which(is.na(results_df$duration) == TRUE) |
#                        results_df$fail == 1,]