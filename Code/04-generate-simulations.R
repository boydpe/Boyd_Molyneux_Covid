# Generate simulations ---------------------------------------------------------------
n_sim = 25

# 5 durations
d1 = c(30, 0, 70)
d2 = c(30, 10, 60)
d3 = c(30, 20, 50)
d4 = c(30, 30, 40)
d5 = c(30, 40, 30)

# 2 replicates of each duration
ds = list(d1, d2, d3, d4, d5)
duration_list = rep(ds, each = n_sim*2)

# k values where an effect is realized
k_effect = list(c(0.2, 1.0, 0.4),
                c(0.2, 1.0, 0.4))


# k values where no effect is realized
k_no = list(c(0.2, 0.2, 0.25),
            c(0.2, 0.2, 0.25))

# make list of combined k values
k_vals = c(rep(k_no, each = n_sim),
           rep(rep(k_effect, each = n_sim), 4))

# create time bins and density
tb = c(1, 3, 7, 11, 15, 20, 40, 60, 110)
mp <- diff(tb / 2) + tb[-length(tb)]
mp <- tb[-length(tb)]
td = dexp(mp, rate = 0.35)

init_date = lubridate::as_date("2021-05-30") 
end_date = init_date + 102

# simulate new data
sims3 = list()

# Set.seed to make simulations reproducible
set.seed(123)

for(n1 in 1:length(k_vals)) {
  sim = covid_sim(duration = duration_list[[n1]],
                  k = k_vals[[n1]],
                  mu = 3,
                  time_density = td,
                  time_bins = tb,
                  print_message = FALSE)
  
  sim1 = sim$daily_data %>%
    rename(daily_cases = cases) %>% 
    mutate(sc = "OregonCorvallis", 
           POPESTIMATE2020 = 4e6,
           cases = cumsum(daily_cases),
           date = init_date + day) %>% 
    filter(date < end_date)
  
  # store data frame in list
  sims3[[n1]] = sim1
}
saveRDS(sims3, "Outputs/Model Fits/final_sims3.Rds")

# Fit Simulations ---------------------------------------------------------------
init_date = lubridate::as_date("2021-05-30") 
end_date = init_date + 102
n_sim = 25

fake_rally = data.frame(Date = rep("2021-07-01", length(sims3)),
                        sc = rep("OregonCorvallis", length(sims3)),
                        min_date = rep(init_date + 1, length(sims3)),
                        max_date = rep(end_date, length(sims3)))

# create list of time bins for model fit
tbins = list()
for(i in 1:nrow(fake_rally)){
  tbins[[i]] = tb
}

mod8 = misd_ee(counties = sims3, rallies = fake_rally,
               time_bins = tbins, cut_time = 7,
               stopwhen = 1e-3, 
               loess_smooth = 0.3,
               print_message = FALSE)

saveRDS(mod8, "Outputs/Model Fits/final_models8.Rds")