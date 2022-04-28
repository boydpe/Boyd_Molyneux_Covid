source("covid_functions.R")
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
k_effect = list(c(0.2, 1, 0.60), # moderate
                c(0.2, 1.3, 0.8)) # strong

# k values where no effect is realized
k_no = list(c(0.1, 0.3, 0.25), # weak (try 0.2, 0.2, 0.6)
            c(0.5, 0.5, 0.5)) # none (try 0.2 for all)

k_no = list(c(0.2, 0.2, 0.25),
            c(0.2, 0.2, 0.2))


# James Edits:
k_no = list(c(0.2, 0.2, 0.25),
            c(0.2, 0.2, 0.25))
k_effect = list(c(0.2, 1.0, 0.4),
                c(0.2, 1.0, 0.4))

# make list of combined k values
k_vals = c(rep(k_no, each = n_sim),
           rep(rep(k_effect, each = n_sim), 4))

# create time bins and density
tb = c(1, 3, 7, 14, 30, 60, 110)
td = c(0.45, 0.01, 0.02/7, 0.02/16, 0.015/30, 0.005/50)

# James Edits:
tb = c(1, 3, 7, 11, 15, 20, 40, 60, 110)
mp <- diff(tb / 2) + tb[-length(tb)]
mp <- tb[-length(tb)]
td = dexp(mp, rate = 0.35)

init_date = lubridate::as_date("2021-05-30") 
end_date = init_date + 102

# simulate new data
sims3 = list()

# James Edits
set.seed(123)

for(n1 in 1:length(k_vals)) {
  # if (n1 == 11) debugonce(covid_sim)
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
           date = init_date + day) %>%  #- burn_in) %>% # maybe - burnin but not sure
    filter(date < end_date)
  
  # store data frame in list
  sims3[[n1]] = sim1
  print(n1)
  
}
saveRDS(sims3, "final_sims3.Rds")

# Fit model ---------------------------------------------------------------

# create fake rally data
source("covid_functions.R")
mods2 = readRDS("final_models2.Rds")
sims = readRDS("final_sims3.Rds")
init_date = lubridate::as_date("2021-05-30") 
end_date = init_date + 102
n_sim = 25

fake_rally = data.frame(Date = rep("2021-07-01", length(sims)),
                        sc = rep("OregonCorvallis", length(sims)),
                        min_date = rep(init_date + 1, length(sims)),
                        max_date = rep(end_date, length(sims)))

# create list of time bins for model fit
tbins = list()
for(i in 1:nrow(fake_rally)){
  tbins[[i]] = tb
}

mod8 = misd_ee(counties = sims, rallies = fake_rally,
               time_bins = tbins, cut_time = 7,
               stopwhen = 1e-3, 
               loess_smooth = 0.3,
               print_message = TRUE)

saveRDS(mod8, "final_models8.Rds")