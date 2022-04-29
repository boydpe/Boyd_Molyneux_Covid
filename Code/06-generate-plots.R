## Figure 01 -------------------------------------------
us_states <- map_data("state") 
us_counties = readRDS("Data/Processed/us_counties.Rds")

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
        panel.grid.minor = element_blank()) +
  coord_map()

ggsave("Outputs/Plots/figure01.pdf", county_map, width = 8, height = 6)


## Figure 02 -------------------------------------------

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
             results_df$k_diff[i] > 0){
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
ggsave("Outputs/Plots/figure02.pdf", plot = plot_grid, width = 6, height = 6)


## Figure 03 -------------------------------------------
ggplot(data = results_df) + 
  geom_dotplot(aes(x = date, fill = result), stackgroups = T,
               stackdir = "center", binpositions="all", 
               binwidth = 5, method = "histodot") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "top", legend.title = element_blank()) +
  labs(x = "Date")
ggsave("Outputs/Plots/figure03.pdf", width = 6, height = 6)
