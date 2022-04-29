# create time bins for temporal decay
tb = c(0, 3, 7, 14, 30, 60, 190)
tbins = list()
for(i in 1:nrow(rally)){
  tbins[[i]] = tb
}
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

saveRDS(out_all, "Outputs/Model Fits/out_all.Rds")
saveRDS(out_state, "Outputs/Model Fits/out_state.Rds")