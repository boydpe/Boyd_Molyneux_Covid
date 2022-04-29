

## Table 2 --------------------------------------------------------------------
sims = readRDS("Outputs/Model Fits/final_sims3.Rds")
mods = readRDS("Outputs/Model Fits/final_models8.Rds")
n_sim = 25

est_time = unlist(mods$opt_length)
real_time = rep(c(0,10,20,30,40), each = 2*n_sim)
time_df = data.frame(est_time, real_time, diff = est_time - real_time)

sim_table = time_df %>% 
  group_by(real_time) %>% 
  summarize(min = min(diff),
            five = round(quantile(diff, prob = 0.05)),
            Q1 = round(quantile(diff, prob = 0.25)),
            median = median(diff),
            Q3 = round(quantile(diff, prob = 0.75)),
            ninefive = round(quantile(diff, prob = 0.95)),
            max = max(diff))
#mean = mean(diff),)

kableExtra::kbl(sim_table,
                align = c("c"), position = "h", booktabs = TRUE, format = "latex", label = "sim_duration",
                col.names = c("True Duration",
                              "Minimum",
                              "5th",
                              "25th",
                              "Median",
                              "75th",
                              "95th",
                              "Maximum"),
                caption = "Differences between estimated and actual durations from simulations")

# ggplot(time_df) +
# geom_boxplot(aes(x = real_time, y = diff, group = as.factor(real_time))) +
# labs(x = "True duration",
#      y = "Duration error")

# n_fail_dur_0 <- time_df %>% 
#   filter(real_time == 0) %>% 
#   summarize(sum(diff > 0))
# n_fail_dur_10 <- time_df %>% 
#   filter(real_time == 10) %>% 
#   summarize(sum(diff < -9))

## Table 3 --------------------------------------------------------------------

# Code for comparing k during to k before
rat <- vector(length = 250)

for (i in seq_along(rat)) {
  if (mods$opt_length[[i]] == 0) {
    rat[i] <- 0
    next
  }
  rat[i] <- (mods$k[[i]][mods$opt_length[[i]] + 30]) / (mean(mods$k[[i]][1:28]) + mods$br[[i]])
  
}

durations = rep(c(10,20,30,40), each = 2*n_sim)
prod_df = data.frame(durations, add_prod = rat[51:250])

prod_df %>% 
  group_by(durations) %>% 
  # summarize(quantile(add_prod))
  summarize(
    min = round(quantile(add_prod, p = 0.00), digits = 2),
    five = round(quantile(add_prod, p = 0.05), digits = 2),
    q1 = round(quantile(add_prod, p = 0.25), digits = 2),
    median = round(quantile(add_prod, p = 0.50), digits = 2),
    q3 = round(quantile(add_prod, p = 0.75), digits = 2),
    ninefive = round(quantile(add_prod, p = 0.95), digits = 2),
    max = round(quantile(add_prod, p = 1.00), digits = 2)) %>% 
  kbl(align = c("c"), position = "h", booktabs = TRUE, format = "latex", label = "sim_prod",
      col.names = c(
        "Durations",
        "Minimum",
        "5th",
        "25th",
        "Median",
        "75th",
        "95th",
        "Maximum"),
      caption = "Estimated additional productivity relative to baseline from simulations")

# #
# n_noeffect <- prod_df %>% 
#   filter(durations == 10, add_prod == 0) %>% 
#   summarize(n())

# ggplot(prod_df) +
#   geom_histogram(aes(x = add_prod)) +
#   facet_wrap(~durations, scales = "free")


## Table 4 --------------------------------------------------------------------

success_df1 = success_df %>% 
  select(-c(fail, k_diff, pop)) %>% 
  select(county, state, date, 
         br, k_before, k_during, 
         duration, cases)
kable(success_df1, 
      caption = "Counties in which an increase in COVID-19 productivity, over baseline, was found following campaign rallies held by then President Donald Trump during the 2020 United States election cycle",
      label = "results_success", 
      format = "latex", 
      col.names = c("County", "State", "Date of Rally", 
                    "$\\mu_\\ell$", "$k_\\ell$", "$k_\\ell^*$", 
                    "Duration", "Cases"), 
      escape = FALSE,
      booktabs = TRUE,
      position = "h") %>% 
  kableExtra::kable_styling(latex_options=c("repeat_header"))


## Table 5 --------------------------------------------------------------------

## Summary stats for success_df
success_df %>% 
  mutate(added_prod = k_during / (k_before + br)) %>% 
  select(county, 
         `Additional productivity relative to baseline` = added_prod, 
         `Duration` = duration, 
         `Additional cases` = cases) %>% 
  pivot_longer(cols = 2:4, names_to = "metric", values_to = "vals") %>% 
  mutate(metric = fct_inorder(metric)) %>% 
  group_by(metric) %>% 
  summarize(
    min = round(quantile(vals, p = 0.00), digits = 2),
    five = round(quantile(vals, p = 0.05), digits = 2),
    q1 = round(quantile(vals, p = 0.25), digits = 2),
    median = round(quantile(vals, p = 0.50), digits = 2),
    q3 = round(quantile(vals, p = 0.75), digits = 2),
    ninefive = round(quantile(vals, p = 0.95), digits = 2),
    max = round(quantile(vals, p = 1.00), digits = 2)) %>% 
  kbl(align = c("l", rep("c", 7)), position = "h", booktabs = TRUE, 
      format = "latex", label = "success_summ",
      col.names = c(
        "",
        "Minimum",
        "5th",
        "25th",
        "Median",
        "75th",
        "95th",
        "Maximum"),
      caption = "Estimated additional productivity relative to baseline from simulations") %>% 
  column_spec(1, width = "1.25in") 


## Table 6 --------------------------------------------------------------------

fail_df1 = fail_df %>% 
  select(-c(k_diff, pop)) %>% 
  select(county, state, date,
         br, k_before, k_during,
         duration, cases, fail) %>% 
  # If convergence failed, replace estimated values with "-"
  mutate(
    br = ifelse(fail == 1, "-", br),
    duration = ifelse(fail == 1, "-", duration),
    k_before = ifelse(fail == 1, "-", k_before),
    k_during = ifelse(fail == 1, "-", k_during),
    cases = ifelse(fail == 1, "-", cases)
  ) %>% 
  mutate(fail = ifelse(fail == 1, "No", "Yes"))

kable(fail_df1, 
      caption = "Counties in which little to no increase in COVID-19 productivity, over baseline, was found following campaign rallies held by then President Donald Trump during the 2020 United States election cycle",
      label = "results_fail", 
      format = "latex", 
      col.names = c("County", "State", "Date of Rally", 
                    "$\\mu_\\ell$", "$k_\\ell$", "$k_\\ell^*$", 
                    "Duration", "Cases", "Converged?"), 
      escape = FALSE,
      booktabs = TRUE,
      position = "h") %>% 
  kableExtra::kable_styling(latex_options=c("repeat_header"))  %>% 
  column_spec(column = 9, latex_column_spec = "c")
