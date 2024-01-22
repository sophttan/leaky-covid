# # Sophia Tan 1/16/23
# incidence data by building

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

data <- NULL
for(i in 1:4){
  data <- rbind(data, read_csv(paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv")))
}

data

infection <- read_csv("complete_testing_data121323.csv") %>% 
  group_by(ResidentId, num_pos) %>%
  summarise_all(first) %>% filter(num_pos>0)

infection <- infection %>% select(ResidentId, Day, num_pos)

data_with_inf <- data %>% left_join(infection, by=c("ResidentId", "Night"="Day"))
data_with_inf

data_with_inf <- data_with_inf %>%  
  mutate(week=difftime(Night, as.Date("2020-03-01"), units="weeks")%>%as.numeric()%>%round()) 

data_with_inf_summary <- data_with_inf %>% group_by(Institution, BuildingId, Night) %>% 
  summarise(week=first(week), n=n(), inf=sum(!num_pos%>%is.na()))
data_with_inf_summary

data_with_inf_summary %>% 
  group_by(Institution, BuildingId) %>% summarise(n=mean(n)) %>% 
  group_by(BuildingId) %>% filter(n()>1) %>% arrange(BuildingId)

data_with_inf_summary_week <- data_with_inf_summary %>% group_by(Institution, BuildingId, week) %>% 
  summarise(Night=first(Night), n=mean(n), inf=sum(inf))

data_with_inf_summary_week_study_period <- data_with_inf_summary_week %>% filter(Night >= "2021-12-09")
data_with_inf_summary_week_study_period <- data_with_inf_summary_week_study_period %>% mutate(inc=inf/n*100)
data_with_inf_summary_week_study_period %>% 
  group_by(week) %>% 
  summarise(Night=first(Night), mean=mean(inc), lb=quantile(inc, .025), ub=quantile(inc, .975)) %>%
  ggplot(aes(as.POSIXct(Night), mean)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=lb, ymax=ub)) +
  scale_x_datetime("Time",date_breaks = "1 month", expand = c(0,0)) + 
  scale_y_continuous("Weekly building incidence (per 100)") + 
  theme(axis.text.x = element_text(angle=90))

  
data_with_inf_summary_week_study_period %>% 
  group_by(week) %>% 
  summarise(Night=first(Night), mean=mean(inf), lb=quantile(inf, .025), ub=quantile(inf, .975)) %>%
  ggplot(aes(as.POSIXct(Night), mean)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=lb, ymax=ub)) +
  scale_x_datetime("Time",date_breaks = "1 month", expand = c(0,0)) + 
  scale_y_continuous("Weekly building cases") + 
  theme(axis.text.x = element_text(angle=90))


start_end_windows <- data_with_inf_summary %>% 
  select(Institution, BuildingId, Night) %>% 
  rename("start"="Night") %>%
  mutate(end=start+6) %>% filter(start >= "2021-12-01")

inc_start_end_windows <- start_end_windows %>% full_join(data_with_inf_summary %>% filter(Night >= "2021-12-01"))
inc_start_end_windows <- inc_start_end_windows %>% filter(Night >= start & Night <= end)
inc_start_end_windows <- inc_start_end_windows %>% select(!week)
inc_start_end_windows <- inc_start_end_windows %>% group_by(Institution, BuildingId, start) %>%
  summarise(end=first(end), n=max(n), inf=sum(inf))

inc_start_end_windows <- inc_start_end_windows %>% filter(end <= "2023-03-01")
inc_start_end_windows <- inc_start_end_windows %>% mutate(inc=inf/n*100)
inc_start_end_windows %>% write_csv("cleaned_incidence_data_rolling.csv")
