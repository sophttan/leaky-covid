# # Sophia Tan 12/19/23
# Basic survival data + descriptive statistics

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

resident_movement <- read_csv("resident_movement1.csv") %>% rbind(read_csv("resident_movement2.csv")) %>%
  rbind(read_csv("resident_movement3.csv")) %>% rbind(read_csv("resident_movement4.csv"))

resident_movement <- resident_movement %>% group_by(ResidentId) %>% filter(first(first)<"2020-04-01")

testing <- read_csv("complete_testing_data121323.csv")

vacc <- read_csv("complete_vaccine_data121523.csv")
vacc <- vacc %>%
  rename("vacc.Day"="Date")

inf <- testing %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% summarise_all(first) %>% select(ResidentId, Day, num_pos)
inf <- inf %>%
  rename("inf.Day"="Day")


generate_survival_data <- function(subvariant, include_inf=F) {
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
           earliest=if_else(include_inf, 
                            pmin(last_day[subvariant], last, inf.Day, vacc.Day,na.rm=T), 
                            pmin(last_day[subvariant], last, vacc.Day,na.rm=T)),
           earliest_novacc=if_else(include_inf, 
                                   pmin(last_day[subvariant], last, inf.Day,na.rm=T), 
                                   pmin(last_day[subvariant], last,na.rm=T)),
           censored = case_when(earliest==last_day[subvariant]~"end_period",
                                earliest==inf.Day~"inf",
                                earliest==vacc.Day~"vaccinated",
                                earliest==last~"moved"),
           censored_novacc = case_when(earliest_novacc==last_day[subvariant]~"end_period",
                                       earliest==inf.Day~"inf",
                                       earliest_novacc==last~"moved"), 
           survival = (earliest - first_day[subvariant]) %>% as.numeric(),
           survival_novacc = (earliest_novacc - first_day[subvariant]) %>% as.numeric())
  
  resident_movement_subvar
}

print((generate_survival_data(1,F) %>% filter(censored!="end_period"))$survival %>% hist()) 

summarise_censoring <- function(data) {
  summary_subvar <- data %>% 
    group_by(censored) %>% summarise(prop=round(n()/nrow(.),2))
  
  summary_subvar_novacc <- resident_movement_subvar %>% 
    group_by(censored_novacc) %>% summarise(prop=round(n()/nrow(.),2)) %>% 
    rbind(list(NA, NA, NA))
  
  cbind(subvar=subvariant, n=nrow(resident_movement_subvar)) %>% cbind(summary_subvar, summary_subvar_novacc)
} 

survival <- rbind(generate_survival_data(1, T),
                  generate_survival_data(2, T), 
                  generate_survival_data(3, T), 
                  generate_survival_data(4, T))

survival

survival_mostrecent_vacc <- survival %>% group_by(ResidentId, first_adj) %>% 
  left_join(vacc %>% select(ResidentId, vacc.Day, num_dose, full_vacc) %>% 
              rename("last.vacc"="vacc.Day", "last.dose"="num_dose")) %>% 
  filter(last.vacc%>%is.na()|all(last.vacc>first_adj)|last.vacc<=first_adj) %>% 
  filter(last.vacc%>%is.na()|last.vacc==max(last.vacc)) %>% 
  mutate(last.vacc=if_else(last.vacc>first_adj, NA, last.vacc),
         last.dose=if_else(last.vacc>first_adj, NA, last.dose))

survival_mostrecent_vacc <- survival_mostrecent_vacc %>% 
  replace_na(list(last.dose=0)) %>%
  # add 1 to J&J recipients 
  # max doses at 4
  mutate(last.dose.adj=if_else(last.dose!=0&full_vacc==1&last.dose>=full_vacc, last.dose+1, last.dose),
         last.dose.adj=if_else(last.dose.adj>4, 4, last.dose.adj))

survival_mostrecent_vacc_filtered <- survival_mostrecent_vacc %>% filter(last.dose.adj!=1)
survival_mostrecent_vacc_filtered <- survival_mostrecent_vacc_filtered %>% select(!c(num_pos, inf.Day, vacc.Day, full_vacc))

survival_mostrecent_vaccinf_filtered <- survival_mostrecent_vacc_filtered %>% group_by(ResidentId, first_adj) %>% 
  left_join(inf %>% rename("last.inf"="inf.Day")) %>% 
  filter(last.inf%>%is.na()|all(last.inf>first_adj)|last.inf<=first_adj) %>% 
  filter(last.inf%>%is.na()|last.inf==max(last.inf)) %>% 
  mutate(last.inf=if_else(last.inf>first_adj, NA, last.inf), 
         has.past.inf=if_else(last.inf%>%is.na(), 0, 1))

survival_mostrecent_vaccinf_filtered <- survival_mostrecent_vaccinf_filtered %>% mutate(last.dose.adj.binary = if_else(last.dose.adj==0, 0, 1))

survival_mostrecent_vaccinf_filtered %>% group_by(first_adj) %>% summarise(n=n()) %>%
  write_csv(here::here("tables/overall_n.csv"))

inf_summary <- survival_mostrecent_vaccinf_filtered %>% group_by(first_adj, has.past.inf) %>% summarise(n=n()) %>% mutate(prop=n/sum(n))
inf_summary %>% write_csv(here::here("tables/inf_binary_n.csv"))

inf_vacc_summary <- survival_mostrecent_vaccinf_filtered %>%
  group_by(first_adj, last.dose.adj.binary, has.past.inf) %>% 
  summarise(n=n()) %>% mutate(prop=n/sum(n))
inf_vacc_summary %>% write_csv(here::here("tables/inf_vacc_binary_n.csv"))

vaccine_summary <- survival_mostrecent_vaccinf_filtered %>% 
  group_by(first_adj, last.dose.adj) %>%
  summarise(n=n()) %>% mutate(prop=n/sum(n))
vaccine_summary %>% write_csv(here::here("tables/vaccine_adj_dose_n.csv"))

survival_mostrecent_vaccinf_filtered %>% 
  group_by(first_adj, last.dose.adj.binary) %>% 
  summarise(n=n()) %>% mutate(prop=n/sum(n)) %>% 
  write_csv(here::here("tables/vaccine_adj_binary_n.csv"))

survival_mostrecent_vaccinf_filtered <- survival_mostrecent_vaccinf_filtered %>% 
  mutate(time_since_vacc=(first_adj-last.vacc)%>%as.numeric(), 
         time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 91, 182, 365, Inf), right = F))  

survival_mostrecent_vaccinf_filtered %>% filter(last.dose.adj>0) %>%
  group_by(first_adj, last.dose.adj, time_since_vacc_cut) %>% 
  summarise(n=n()) %>% mutate(prop=n/sum(n)) %>% 
  write_csv(here::here("tables/vaccine_time_n.csv"))

survival_mostrecent_vaccinf_filtered %>% filter(last.dose.adj>0) %>%
  group_by(first_adj, last.dose.adj.binary, time_since_vacc_cut) %>% 
  summarise(n=n()) %>% mutate(prop=n/sum(n)) %>% 
  write_csv(here::here("tables/vaccine_binary_time_n.csv"))

# t %>% write_csv(here::here("tables/censoring.csv"))

#### look at testing data ####
survival_mostrecent_vaccinf_filtered_testing <- survival_mostrecent_vaccinf_filtered %>%
  mutate(last_adj=case_when(first_adj=="2021-12-15"~"2022-05-14",
                            first_adj=="2022-05-15"~"2022-08-14",
                            first_adj=="2022-08-15"~"2022-12-14",
                            first_adj=="2022-12-15"~"2023-03-14") %>% as.Date()) %>% 
  left_join(testing %>% select(ResidentId, Day) %>% rename("test.Day"="Day"))

survival_mostrecent_vaccinf_testing_filtered <- survival_mostrecent_vaccinf_filtered_testing %>% 
  filter(n()==1|all(test.Day > last_adj|test.Day < first_adj)|(test.Day <= last_adj & test.Day >= first_adj))

survival_mostrecent_vaccinf_testing_filtered <- survival_mostrecent_vaccinf_testing_filtered %>%
  mutate(test.Day=if_else(test.Day %>% is.na() | test.Day < first_adj | test.Day>last_adj, NA, test.Day)) %>% 
  distinct(ResidentId, first_adj, test.Day, .keep_all = T)

survival_mostrecent_vaccinf_testing_filtered_selected <- survival_mostrecent_vaccinf_testing_filtered %>%
  select(first_adj, last_adj, last.dose.adj, last.dose.adj.binary, test.Day)

total_tests <- survival_mostrecent_vaccinf_testing_filtered_selected %>% 
  summarise(last_adj=first(last_adj), 
            last.dose.adj=first(last.dose.adj), 
            last.dose.adj.binary=first(last.dose.adj.binary),
            tests=sum(!test.Day%>%is.na()))

total_tests <- total_tests %>% mutate(total_days=(last_adj-first_adj+1)%>%as.numeric(),
                                      testing_rate=tests/total_days*30.4)

total_tests %>% group_by(first_adj) %>% summarise(test_rate=mean(testing_rate)) 
total_tests %>% group_by(first_adj) %>% summarise(test_rate=mean(testing_rate)) 
total_tests %>% group_by(first_adj, last.dose.adj.binary) %>% 
  summarise(testing_rate=mean(testing_rate), prop_notests=sum(tests==0)/n()) %>%
  write_csv(here::here("tables/testing_descriptive.csv"))
total_tests %>% group_by(first_adj, last.dose.adj) %>% summarise(no_tests=sum(tests==0), n=n(), prop_notests=sum(tests==0)/n())
