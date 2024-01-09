# # Sophia Tan 12/19/23
# Combine housing and building data

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

resident_movement <- read_csv("resident_movement1.csv") %>% rbind(read_csv("resident_movement2.csv"))

resident_movement <- resident_movement %>% group_by(ResidentId) %>% filter(first(first)<"2020-04-01")

testing <- read_csv("complete_testing_data121323.csv")

vacc <- read_csv("complete_vaccine_data121523.csv")
vacc <- vacc %>%
  rename("vacc.Day"="Date")

inf <- testing %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% summarise_all(first) %>% select(ResidentId, Day, num_pos)
inf <- inf %>%
  rename("inf.Day"="Day")


test <- function(subvariant) {
  first_day <- c("2021-12-15", "2022-05-15", "2022-08-15", "2022-12-15") %>% as.Date()
  last_day <- c("2022-05-14", "2022-08-14", "2022-12-14", "2023-03-14") %>% as.Date()
  
  resident_movement_subvar <- resident_movement %>% filter(first <= first_day[subvariant] & last >= first_day[subvariant])

  exclude_residents_initial <- (inf %>% filter(any(first_day[subvariant]-inf.Day<=90 & first_day[subvariant]-inf.Day>0)))$ResidentId %>% unique()
  
  resident_movement_subvar <- resident_movement_subvar %>% filter(!ResidentId %in% exclude_residents_initial)
  
  resident_movement_subvar <- resident_movement_subvar %>% mutate(first_adj=first_day[subvariant])
  
  resident_movement_subvar <- resident_movement_subvar %>% 
    left_join(inf %>% 
                filter(inf.Day >= first_day[subvariant] & inf.Day <= last_day[subvariant])) %>% 
    filter(n()==1 | inf.Day==first(inf.Day)) 
  
  resident_movement_subvar <- resident_movement_subvar %>%
    left_join(vacc %>% 
                select(ResidentId, vacc.Day, num_dose) %>% 
                filter(vacc.Day >= first_day[subvariant] & vacc.Day <= last_day[subvariant])) %>%
    filter(n()==1 | vacc.Day==last(vacc.Day))
  
  resident_movement_subvar <- resident_movement_subvar %>% 
    mutate(vacc.Day = if_else(vacc.Day <= first_day[subvariant], NA, vacc.Day),
           inf.Day = if_else(inf.Day <= first_day[subvariant], NA, inf.Day),
           earliest=pmin(last_day[subvariant], last, vacc.Day,na.rm=T),
           earliest_novacc=pmin(last_day[subvariant], last,na.rm=T),
           censored = case_when(earliest==last_day[subvariant]~"end_period",
                                earliest==vacc.Day~"vaccinated",
                                earliest==last~"moved"),
           censored_novacc = case_when(earliest_novacc==last_day[subvariant]~"end_period",
                                       earliest_novacc==last~"moved"), 
           time_til_earliest = (earliest - first_day[subvariant]) %>% as.numeric(),
           time_til_earliest_vacc = (earliest_novacc - first_day[subvariant]) %>% as.numeric())
  
  print((resident_movement_subvar %>% filter(censored!="end_period"))$time_til_earliest %>% hist())

  summary_subvar <- resident_movement_subvar %>% 
    group_by(censored) %>% summarise(prop=round(n()/nrow(.),2))
  summary_subvar_novacc <- resident_movement_subvar %>% 
    group_by(censored_novacc) %>% summarise(prop=round(n()/nrow(.),2)) %>% 
    rbind(list(NA, NA, NA))
  
  cbind(subvar=subvariant, n=nrow(resident_movement_subvar)) %>% cbind(summary_subvar, summary_subvar_novacc)
}

t <- rbind(test(1), test(2), test(3), test(4))
t %>% write_csv(here::here("tables/censoring.csv"))


