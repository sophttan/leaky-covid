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

inf <- testing %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% summarise_all(first) %>% select(ResidentId, Day, num_pos)
inf

vacc

resident_movement_subvar1 <- resident_movement %>% filter(first <= "2021-12-15" & last >="2021-12-15")

exclude_residents_initial <- (inf %>% filter(any(as.Date("2021-12-15")-Day<=90 & as.Date("2021-12-15")-Day>0)))$ResidentId %>% unique()

resident_movement$ResidentId%>%unique()

resident_movement_subvar1 <- resident_movement_subvar1 %>% filter(!ResidentId %in% exclude_residents_initial)

resident_movement_subvar1 <- resident_movement_subvar1 %>% mutate(first_adj="2021-12-15")

resident_movement_subvar1

resident_movement_subvar1 <- resident_movement_subvar1 %>% left_join(inf %>% filter(Day <= "2022-05-14")) %>% 
  filter(n()==1 | Day==last(Day)) %>%
  rename("inf.Day"="Day")

resident_movement_subvar1 <- resident_movement_subvar1 %>%
  left_join(vacc %>% select(ResidentId, Date, num_dose) %>% rename("vacc.Day"="Date") %>% filter(vacc.Day <= "2022-05-14")) %>%
  filter(n()==1 | vacc.Day==last(vacc.Day))

resident_movement_subvar1 <- resident_movement_subvar1 %>% 
  mutate(vacc.Day = if_else(vacc.Day <= "2021-12-15", NA, vacc.Day),
         inf.Day = if_else(inf.Day <= "2021-12-15", NA, inf.Day),
         earliest=pmin(as.Date("2022-05-14"), last, inf.Day, vacc.Day, na.rm=T),
         censored = case_when(earliest==as.Date("2022-05-14")~"end_period",
                              earliest==last~"moved",
                              earliest==vacc.Day~"vacc",
                              earliest==inf.Day~"infection"))

resident_movement_subvar1$censored %>% table()
