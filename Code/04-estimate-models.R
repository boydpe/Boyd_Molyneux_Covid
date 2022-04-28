# create time bins
# tb = c(0, 7, 14, 30, 60, 190)
# ZZQ: c(1,3,7,14,30,60,190)
tb = c(0, 3, 7, 14, 30, 60, 190)
tbins = list()
for(i in 1:nrow(rally)){
  tbins[[i]] = tb
}
# temporary adjustment for troubleshooting

# all_data[[2]]$cases[1:5] = c(8627, 8896, 9112, 9522, 9815) #20000
# was 8627, 8896, 9112, 9522, 9815

# model each county 
out_all = misd_ee(counties = all_data, rallies = rally,
                  time_bins = tbins, cut_time = 14,
                  loess_smooth = 0.20,
                  stopwhen = 1e-3)

# model state without rally counties
out_state = misd_ee_state(counties = state_sub_rally, rallies = rally,
                          time_bins = tbins, cut_time = 14,
                          stopwhen = 1e-3, set_length = out_all$opt_length,
                          loess_smooth = 0.20
)