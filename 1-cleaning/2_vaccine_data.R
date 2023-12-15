# Sophia Tan 12/13/23
# Cleaning CDCR COVID-19 vaccination data through 5/26/23

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

###### COVID Infection Data ######
vacc <-  read_delim("Immunization_20230526.csv", delim=";")
head(vacc)

unique(vacc$Vaccine)
covid_vacc <- vacc %>% filter(grepl("SARS", Vaccine))
unique(covid_vacc$Vaccine)

# rename so all JJ vaccines named the same
covid_vacc <- covid_vacc %>% mutate(Date = as.Date(Date), 
                                    Vaccine = ifelse(grepl("Ad26", Vaccine), "SARS-CoV-2 (COVID-19) Ad26 , recomb", Vaccine),
                                    Vaccine = ifelse(grepl("tozinameran", Vaccine), "SARS-CoV-2 (COVID-19) mRNA BNT-162b2 vax", Vaccine))

# there are 31 residents with multiple records of vaccination on a single day (i.e. 1 dose of J&J and 1 dose of Moderna vaccine on 6/5/21)
# collapse records but unsure which vaccine type is correct
covid_vacc_cleaned <- covid_vacc %>% filter(Result=="Received") 

# also some who have incorrect years in the reporting
early <- covid_vacc_cleaned %>% group_by(ResidentId) %>% arrange(ResidentId, Date) %>% filter(any(Date<"2020-12-01")) # all likely 2021 vaccinations - question mark is 1611265470

covid_vacc_cleaned <- covid_vacc_cleaned %>% mutate(Date=if_else(Date<"2020-12-01", ymd(format(Date, "2021-%m-%d")), Date)) %>% select(!Month)
covid_vacc_cleaned <- covid_vacc_cleaned %>% distinct()
covid_vacc_cleaned <- covid_vacc_cleaned %>% group_by(ResidentId, Date) %>% arrange(Vaccine) %>% 
  summarise(Vaccine=ifelse(all(Vaccine==first(Vaccine)), first(Vaccine), paste(Vaccine, collapse = "|")))
covid_vacc_cleaned <- covid_vacc_cleaned %>% arrange(Date) %>% 
  group_by(ResidentId) %>% mutate(num_dose = 1:n(), max_dose = n()) %>% arrange(ResidentId)

# if someone was marked to have received any j&j as first dose and second dose is >60 days, mark as receiving j&j, even if
# mrna also reported as first dose
covid_vacc_cleaned <- covid_vacc_cleaned %>% 
  mutate(time_since=c(diff(Date)%>%as.numeric(),NA)) %>%
  mutate(full_vacc = ifelse(grepl("Ad26", first(Vaccine)), 1, 2),
         full_vacc = ifelse(full_vacc==1&grepl("mRNA", first(Vaccine))&!is.na(first(time_since))&first(time_since<60), 2, full_vacc),
         booster_add_dose = ifelse(num_dose<=full_vacc, 0, num_dose - full_vacc),
         incomplete = ifelse(max(num_dose) < full_vacc, 1, 0))

# 122184 residents received at least 1 dose of a vaccine
num_res_vacc <- covid_vacc_cleaned$ResidentId %>% unique() %>% length()

# 3666 residents (3% of vaccinated residents) received only 1 dose of an mRNA vaccine 
covid_vacc_cleaned %>% filter(incomplete==1)

# 38026 residents (38% of vaccinated residents) have completed series but no booster (not accounting for time eligibility)
covid_vacc_cleaned %>% filter(all(booster_add_dose == 0 & incomplete ==0))

# offset doses of covid vaccines to account for delay in protection 
covid_vacc_cleaned <- covid_vacc_cleaned %>% mutate(Date_offset = Date + 14)

write_csv(covid_vacc_cleaned, "D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv")

covid_vacc_cleaned %>% group_by(Date) %>% summarise(primary=sum(full_vacc==num_dose),
                                                    boost1=sum(booster_add_dose==1),
                                                    boost2=sum(booster_add_dose==2), 
                                                    boost3=sum(booster_add_dose>2)) %>% 
  mutate_at(c("primary", "boost1", "boost2", "boost3"), cumsum) %>% 
  ggplot(aes(as.POSIXct(Date))) +
  geom_line(aes(y=primary, color="Primary series")) + 
  geom_line(aes(y=boost1, color="1 booster")) + 
  geom_line(aes(y=boost2, color="2+ boosters")) +
  geom_line(aes(y=boost3, color="3+ boosters")) +
  scale_x_datetime("Time", breaks="1 month") + 
  scale_y_continuous("Cumulative number of residents") +
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

  
