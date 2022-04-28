# Peter Boyd
# COVID
# extra work

### DATA
# covid data
# urlfile = 'https://raw.githubusercontent.com/boydpe/covid-19-data/master/us-counties.csv'
# covid = read_csv(url(urlfile))
# covid = as.data.frame(covid)
# covid$sc = paste(covid$state, covid$county, sep = "")
# # rally data
# rally = read.csv("Data/rally_data.csv")
# rally$Date = lubridate::ymd(rally$Date)
# rally = rally[-c(20,31),]
# rally = rally %>% filter(Date < "2020-11-04")
# 
# # define data period
# pre_time = 29
# post_time = 150
# for(i in 1:nrow(rally)) {
#   rally$min_date = rally$Date - pre_time
#   rally$max_date = rally$Date + post_time
# }
# 
# # eliminate duplicated counties
# rally$sc = paste(rally$State, rally$County, sep = "")
# rally = rally[!duplicated(rally$sc),]
# 
# # add county population
# county_pop = read.csv("Data/county_pop.csv")
# county_pop = county_pop %>% mutate(
#   CTYNAME = stringr::str_remove_all(CTYNAME, " County")) %>% 
#   select(STNAME, CTYNAME, POPESTIMATE2020) %>% 
#   rename(county = CTYNAME, state = STNAME)  
# county_pop$sc = paste(county_pop$state, county_pop$county, sep = "")
# 
# n1 = nrow(rally)
# all_data = list()
# for(i in 1:n1) { 
#   all_data[[i]] = covid %>% filter(county == rally$County[i] & 
#                                       state == rally$State[i]) %>% 
#     filter(date > rally$min_date[i], date < rally$max_date[i]) %>%   
#     merge(county_pop, by = "sc")
# }
# 
# # create state w/o county data
# # OR any rally county data=
# # this will be used for post hoc analyses
# state_sub_rally = list()
# for (i in 1:n1) {
#   counties_list = rally %>% filter(State == rally$State[i])
#   state_sub_rally[[i]] = covid %>%
#     filter(!sc %in% rally$sc) %>% 
#     merge(county_pop, by = "sc") %>%  #c("state", "county")) %>% 
#     # counties_list$County &
#     #          state == rally$State[i]) %>% 
#     filter(date > rally$min_date[i], 
#            date < rally$max_date[i]) %>% 
#     group_by(date) %>% 
#     summarise(cases_tot = sum(cases), 
#               pop_tot = sum(unique(POPESTIMATE2020))) %>% 
#     mutate(newcases = c(0, diff(cases_tot)), 
#            state = rally$State[i])
#   print(paste("county ", i, " done"))
# }
# 
# saveRDS(state_sub_rally, file = "Data/state_data.Rds")
# saveRDS(all_data, file = "Data/county_data.Rds")
# saveRDS(rally, file = "Data/rally_edit.Rds")