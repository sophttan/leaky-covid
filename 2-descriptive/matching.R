# # Sophia Tan 12/19/23
# test matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

data <- read_csv("cleaned_survival_data_prematch012624.csv") %>% filter(first_adj=="2021-12-15")

# matchit_results <- matchit(last.dose.adj.binary~first_adj+Institution+BuildingId+has.past.inf, 
#                            data=data,
#                            exact=~first_adj+Institution+BuildingId+has.past.inf)

data <- data %>% mutate(time_since_vacc=(first_adj-last.vacc)%>%as.numeric(),
                        time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F, )) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"
data <- data %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,365)", "[365,730)","[730,Inf)")))

demo <- read_csv("demographic121523.csv")
risk <- read_csv("covid_risk_score012324.csv") %>% mutate(risk=ifelse(risk>2, 2, risk)) %>% 
  group_by(ResidentId) %>% arrange(ResidentId, risk.start) 
risk_summary <- risk %>% 
  mutate(same_risk=(c(NA, diff(risk)))==0) %>% 
  filter(same_risk%>%is.na()|!same_risk) %>%
  mutate(group=1:n())
risk <- risk %>% left_join(risk_summary) %>% 
  fill(group, .direction="down")
risk <- risk %>% group_by(ResidentId, group) %>%
  summarise(risk.start=first(risk.start), risk.end=last(risk.end), risk=first(risk)) %>% select(!group)
risk  

data <- data %>% left_join(demo %>% select(ResidentId, BirthYear, Sex))
data <- data %>% mutate(age=if_else(first_adj=="2022-12-15", 2023-BirthYear, 2022-BirthYear))
data <- data %>% left_join(risk)
data <- data %>% group_by(ResidentId) %>% 
  mutate(keep=n()==1|(risk.start<=first_adj&risk.end>=first_adj)|risk.start==max(risk.start))%>%
  filter(keep) %>% filter(n()==1|risk.start==min(risk.start)) %>%
  mutate(risk=if_else(risk%>%is.na()|risk.start>first_adj, 0, risk))
data <- data %>% select(!keep)

# add exact match by first_adj if doing other periods
matchit_results_inf <- matchit(last.dose.adj.binary~Institution+BuildingId+RoomType+time_since_inf_cut+
                                 age + risk + Sex, 
                               data=data,
                               exact=~Institution+BuildingId+RoomType+time_since_inf_cut+Sex) 
summary(matchit_results_inf)

plot(matchit_results_inf)
m <- matchit_results_inf %>% get_matches() 

m %>% group_by(first_adj, last.dose.adj.binary) %>% summarise(n=n())

m <- m %>% 
  mutate(censored_binaryvacc = if_else(last.dose.adj.binary==0&censored=="vaccinated", "vaccinated", censored_novacc),
         survival_binaryvacc = if_else(last.dose.adj.binary==0&censored=="vaccinated", survival, survival_novacc),
         status=if_else(censored_binaryvacc=="inf", 1, 0))

basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary + time_since_inf_cut + frailty(subclass), m)
summary(basic_ve)

m <- m %>% group_by(subclass) %>% mutate(status=if_else(survival_binaryvacc==min(survival_binaryvacc), status, 0),
                                         survival_binaryvacc=min(survival_binaryvacc))

for (i in c("None", "[0,365)", "[365,730)")) {
  autoplot(survfit(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary + time_since_inf_cut, 
                   m %>% filter(time_since_inf_cut==i)),
           main = paste("Recent prior infection: ", i), ylim = c(0.7, 1), legTitle="vaccine") %>% print()
}

basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass), m)
summary(basic_ve)

# time-varying dataset

m <- m %>% mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 91, 182, 365, Inf), right = F)) 
levels(m$time_since_vacc_cut)<-c(levels(m$time_since_vacc_cut), "None") 
m$time_since_vacc_cut[is.na(m$time_since_vacc_cut)] <- "None"
m <- m %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,91)", "[91,182)","[182,365)", "[365,Inf)")))

basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ time_since_vacc_cut + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass), m)
summary(basic_ve)


for (i in c("2021-12-15", "2022-05-15", "2022-08-15", "2022-12-15")) {
  autoplot(survfit(Surv(survival, status) ~ time_since_vacc_cut, 
                   m %>% filter(first_adj==i)),
           main = paste("Subvariant start: ", i), legTitle="vaccine") %>% print()
}
