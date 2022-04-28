us_states <- map_data("state") 
us_counties = readRDS("Data/us_counties.Rds")
# relevant code in appendix

cols = c("0" = "gray80", "1" = "black")
county_map = ggplot() + geom_polygon( data = us_counties, aes(x=long, y=lat, group=group, fill = as.factor(rally), color = as.factor(rally)), color="gray50", #"darkblue", #fill="lightblue",
                                      size = .1) + 
  scale_fill_manual(values = cols) + 
  geom_polygon( data=us_states, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue",  size = 0.1, alpha = .2) +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#########

n = length(out_all$data)

# find counties that converged and had length > 1
county_fail = which(out_all$fail == 1)
county_no_impact = which(out_all$opt_length == 1)
success = c(1:n)[-c(county_fail, county_no_impact)]
fail = county_fail

# Plot state vs. county k levels
compare_plots = list()
for(i in 1:length(success)) { #length(success)) {
  compare_plots[[i]] = ggplot() + 
    # geom_point(data = out_all$data[[success[i]]], aes(x = date, y = newcases), alpha = 0.2) + 
    geom_vline(xintercept = out_all$data[[success[i]]]$date[1] +  lubridate::days(out_all$ee_time[[success[i]]] -1)) + 
    geom_vline(xintercept = out_all$data[[success[i]]]$date[1] +  lubridate::days(out_all$ee_time[[success[i]]] -1 + out_all$opt_length[[success[[i]]]])) + 
    geom_step(data = data.frame(k = out_all$k[[success[[i]]]] / out_all$data[[success[[i]]]]$POPESTIMATE2020, 
                                date = out_all$data[[success[i]]]$date),
              aes(x = date, y = k),
              linetype = "solid") +
    geom_step(data = data.frame(k = out_state$k[[success[[i]]]] /
                                  out_state$data[[success[[i]]]]$pop_tot, 
                                date = out_state$data[[success[i]]]$date),
              aes(x = date, y = k),
              linetype = "dashed") +
    ggtitle(paste(out_all$data[[success[i]]]$county, 
                  ",", 
                  out_all$data[[success[i]]]$state)) + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 9))
}
# this plot technique works but we may need to alter for population or county number?
# compare_plots
# img1 = patchwork::wrap_plots(compare_plots, ncol = 3)

#ggsave("Images/img1.pdf", img1, height = 24)

#########

# Grid of plots
# Plot 1: results
results_df$result = rep(NA, nrow(results_df))
results_df$effect = rep(NA, nrow(results_df)) 

for(i in 1:nrow(results_df)) {
  if(results_df$fail[i] == 1) {
    results_df$result[i] = "Algorithm fails\nto converge"
    results_df$effect[i] = 0
  } else if(results_df$fail[i] == 0 &
            (results_df$k_diff[i] < 0 |
             results_df$duration[i] == 0)) {
    results_df$result[i] = "No rally\neffect present"
    results_df$effect[i] = 1
  } else if (results_df$fail[i] == 0 &
             results_df$duration[i] > 0 &
             results_df$k_diff > 0){
    results_df$result[i] = "Rally effect\npresent"
    results_df$effect[i] = 0.5
  } else {
    results_df$result[i] = "no impact"
    results_df$effect[i] = -1
  }
}

p1 = results_df %>% 
  ggplot() + 
  geom_bar(aes(x = forcats::fct_infreq(result))) + 
  xlab("Result") + 
  # theme(axis.text.x = element_text(angle = 45)) +
  labs(tag = "A", y = "")
# Plot 2: distribution of k_diff (kappa in paper)
p2 = success_df %>% 
  ggplot() + 
  geom_histogram(aes(x = k_diff*10000), binwidth = 0.75, color = "white") + 
  xlab(expression(kappa*" multiplier per 10,000 people")) + 
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  labs(tag = "B", y = "")
# Plot 3: distribution of duration
p3 = success_df %>% 
  ggplot() + 
  geom_histogram(aes(x = duration), binwidth = 5, color = "white") + 
  xlab("Duration of Contagion") + 
  scale_x_continuous(breaks = seq(10, 100, by = 10))+
  labs(tag = "C", y = "")
# Plot 4: cases
p4 = success_df %>% 
  ggplot() + 
  geom_histogram(aes(x = cases / 1000), binwidth = 3, color = "white") + 
  xlab("Excess Cases (in thousands)") + 
  scale_x_continuous(breaks = seq(0, 70, by = 10), 
                     labels = scales::comma_format())+
  labs(tag = "D", y = "")

plot_grid = gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
# ggsave("Images/plot_grid.pdf", plot_grid)

######

# results_df
ggplot(data = results_df) + 
  geom_dotplot(aes(x = date, fill = result), stackgroups = T,
               stackdir = "center", binpositions="all", 
               binwidth = 5, method = "histodot") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "top", legend.title = element_blank()) +
  labs(x = "Date")

# ggplot(data = results_df) +
#   geom_point(aes(x = date, y = log(pop), color = result))
# timeline + theme_hc()
# ggsave("Images/timeline.pdf", timeline)