# # Sophia Tan 1/18/23
# test relative ve matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

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

data <- read_csv("cleaned_survival_data_prematch020624.csv") %>% 
  filter(first_adj=="2021-12-15") 

data <- data %>% mutate(time_since_vacc=(first_adj-last.vacc)%>%as.numeric(),
                        time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F)) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"

data_vacc <- data %>% filter(last.dose.adj.binary>0) %>% 
  mutate(treatment=if_else(last.dose.adj==2, 0, 1)) %>%
  mutate(censored_treatment = if_else(treatment==0&censored=="vaccinated", "vaccinated", censored_novacc),
         survival_treatment = if_else(treatment==0&censored=="vaccinated", survival, survival_novacc))
data_vacc <- data_vacc %>% filter(survival_treatment>0)

data_vacc <- data_vacc %>% left_join(demo %>% select(ResidentId, BirthYear, Sex))
data_vacc <- data_vacc %>% mutate(age=if_else(first_adj=="2022-12-15", 2023-BirthYear, 2022-BirthYear))
data_vacc <- data_vacc %>% left_join(risk)
data_vacc <- data_vacc %>% group_by(ResidentId) %>% 
  mutate(keep=n()==1|(risk.start<=first_adj&risk.end>=first_adj)|risk.start==max(risk.start))%>%
  filter(keep) %>% filter(n()==1|risk.start==min(risk.start)) %>%
  mutate(risk=if_else(risk%>%is.na()|risk.start>first_adj, 0, risk))
data_vacc <- data_vacc %>% select(!keep)

matchit_results_inf <- matchit(treatment~first_adj+Institution+BuildingId+RoomType+time_since_inf_cut+
                                 age + risk + Sex, 
                               data=data_vacc,
                               exact=~first_adj+Institution+BuildingId+RoomType+time_since_inf_cut) 
summary(matchit_results_inf)

m <- matchit_results_inf %>% get_matches() 
m

m %>% group_by(first_adj, treatment) %>% summarise(n=n())

m <- m %>% mutate(status=if_else(censored_treatment=="inf", 1, 0))
m <- m %>% group_by(subclass) %>% mutate(status=if_else(survival_treatment>min(survival_treatment), 0, status),
                                         survival_treatment=min(survival_treatment))

par(mfrow = c(2, 2))
for (i in c("None", "[0,365)", "[365,730)")) {
  autoplot(survfit(Surv(survival, status) ~ treatment + time_since_inf_cut, 
                   m %>% filter(first_adj=="2021-12-15" & time_since_inf_cut==i)),
           main = paste("Recent prior infection: ", i), ylim = c(0.7, 1), legTitle="vaccine") %>% print()
}


m <- m %>% mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 91, 182, 365, Inf), right = F)) 
m <- m %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("[0,91)", "[91,182)","[182,365)", "[365,Inf)")))

basic_ve <- coxph(Surv(survival_treatment, status) ~ treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass), m)
cox.zph(basic_ve)
summary(basic_ve)
plot(cox.zph(basic_ve),se=T, var=9, resid=F)

ggcoxdiagnostics(basic_ve, type = "martingale", linear.predictions = TRUE)
ggcoxdiagnostics(basic_ve, type = "deviance", linear.predictions = TRUE)

ggcoxdiagnostics(coxph(Surv(survival_treatment, status) ~ age, m), type = "deviance", linear.predictions = TRUE)
ggcoxdiagnostics(coxph(Surv(survival_treatment, status) ~ age, m), type = "deviance", linear.predictions = TRUE)


basic_ve <- coxph(Surv(survival_treatment, status) ~ tt(time_since_vacc) + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass), 
                  tt=function(x,t,...){x+t},m)
summary(basic_ve)

basic_ve <- coxph(Surv(survival_treatment, status) ~ time_since_vacc_cut + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),m)
summary(basic_ve)

basic_ve <- coxph(Surv(survival_treatment, status) ~ treatment*tt(time_since_vacc) + time_since_inf_cut +
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass), 
                  tt=function(x,t,...){x+t},m)
summary(basic_ve)

basic_ve <- coxph(Surv(survival_treatment, status) ~ treatment + tt(time_since_vacc) + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  tt=function(x,t,...){x+t},m)
summary(basic_ve)


# time-varying dataset
m_timevar <- m %>%
  group_by(id) %>% 
  uncount(survival_treatment,.remove = F) 

m_timevar <- m_timevar %>% 
  mutate(time1=seq(0,first(survival_treatment)-1),
         time2=seq(first(survival_treatment))) %>%
  mutate(first_adj=first_adj+time1)

m_timevar <- m_timevar %>%
  mutate(time_since_inf=time_since_inf+time1,
         time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F)) 
levels(m_timevar$time_since_inf_cut)<-c(levels(m_timevar$time_since_inf_cut),"None") 
m_timevar$time_since_inf_cut[is.na(m_timevar$time_since_inf_cut)] <- "None"
m_timevar <- m_timevar %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,365)", "[365,730)", "[730,Inf)")))

vacc <- read_csv("complete_vaccine_data121523.csv") %>% select(ResidentId, Date, num_dose) %>% 
  mutate(Date=Date+14)
m_timevar <- m_timevar %>% left_join(vacc, by=c("ResidentId", "first_adj"="Date"))
m_timevar <- m_timevar %>% mutate(num_dose=if_else(time1==0, NA, num_dose))
m_timevar <- m_timevar %>% group_by(id) %>% fill(num_dose, .direction="down")
m_timevar <- m_timevar %>% group_by(id, num_dose) %>% mutate(time_since_vacc2=0:(n()-1))
m_timevar <- m_timevar %>%
  mutate(time_since_vacc=if_else(num_dose%>%is.na(),time_since_vacc+time1, time_since_vacc2),
         time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 91, 182, 365, Inf), right = F)) 
m_timevar <- m_timevar %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("[0,91)", "[91,182)","[182,365)", "[365,Inf)")))


incidence <- read_csv("cleaned_incidence_data_rolling.csv") %>% 
  select(Institution, BuildingId, start, end, inc, inf) %>% 
  mutate(end_offset=end+3)
m_timevar_inc <- m_timevar %>% left_join(incidence, by=c("Institution", "BuildingId", "first_adj"="end_offset")) %>%
  replace_na(list(inf=0, inc=0))

m_timevar_inc <- m_timevar_inc %>% mutate(status=if_else(time2<survival_treatment, 0, status))

# m_timevar_inc_summary <- m_timevar_inc %>% group_by(id, inc, time_since_vacc_cut, time_since_inf_cut) %>% 
#   summarise(time1=time1, consecutive=c(NA,diff(time1))) %>% filter(consecutive%>%is.na()|consecutive!=1) %>%
#   ungroup() %>% mutate(group=1:n()) %>% select(!c(inc, consecutive))

# m_timevar_inc <- m_timevar_inc %>% left_join(m_timevar_inc_summary) %>% 
#   group_by(id) %>% fill(group, .direction = "down") %>% 
#   group_by(id, group, subclass, Institution, BuildingId, RoomType, survival_treatment, treatment,
#            age, risk, Sex, time_since_inf_cut, time_since_vacc_cut, status) %>%
#   summarise(inc=first(inc), time1=first(time1), time2=last(time2))%>%
#   ungroup()%>%
#   mutate(status=if_else(time2<survival_treatment, 0, status)) %>%
#   arrange(id, time1)

clean_results <- function(results) {
  summary(results)%>%coef()%>%as.data.frame()%>%
    mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
    select(coef, lb, ub, p) %>% round(4) %>% 
    filter(!grepl("Institution", row.names(.)))
}

set.seed(15)
subclass_subset <- m$subclass%>%sample(32972/4)
m_timevar_inc_sub <- m_timevar_inc%>%ungroup()%>%filter(subclass%in%subclass_subset)

# check basic ve 
basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                    m_timevar_inc)
cox.zph(basic_ve)
clean_results(basic_ve)


basic_ve <- coxph(Surv(time1, time2, status) ~ time_since_vacc*inc + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + frailty(subclass), #+ factor(Institution) + frailty(subclass),
                  m_timevar_inc)
cox.zph(basic_ve)
summary(basic_ve)


basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*inc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  m_timevar_inc)
cox.zph(basic_ve)
clean_results(basic_ve)


# categorical incidence
m_timevar_inc <- m_timevar_inc %>% mutate(inc_cat = if_else(inc==0, 0, 1))

basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*inc_cat + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + frailty(subclass), #+ factor(Institution) + frailty(subclass),
                  m_timevar_inc)
cox.zph(basic_ve)
summary(basic_ve)


basic_ve <- coxph(Surv(time1, time2, status) ~ time_since_vacc*inc + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + frailty(subclass), #+ factor(Institution) + frailty(subclass),
                  m_timevar_inc)
cox.zph(basic_ve)
summary(basic_ve)


# categorical incidence
m_timevar_inc <- m_timevar_inc %>% mutate(inc_binary=ifelse(inc==0, 0, 1),
                                          inc_cat = case_when(inc==0~0,
                                                              inc<1~1,
                                                              inc<5~2,
                                                              inc<10~3,
                                                              T~4)%>%factor(),
                                          inf_cat = case_when(inf==0~0,
                                                              inf==1~1,
                                                              inf<5~2,
                                                              inf<10~3,
                                                              T~4)%>%factor())

basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*inc_binary + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  m_timevar_inc)
clean_results(basic_ve)

basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*inf_cat + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + frailty(subclass), #+ factor(Institution) + frailty(subclass),
                  m_timevar_inc)
cox.zph(basic_ve)
summary(basic_ve)


basic_ve <- coxph(Surv(time1, time2, status) ~ time_since_vacc*inc_cat + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + frailty(subclass), #+ factor(Institution) + frailty(subclass),
                  m_timevar_inc)
cox.zph(basic_ve)
summary(basic_ve)

ggsurvplot(fit  = survfit(Surv(time1, time2, event=status) ~ inc_cat, data=m_timevar_inc), 
           fun = "cloglog")


data_vacc_filtered <- data_vacc %>% filter((treatment==0&time_since_vacc>=200)|(treatment==1&time_since_vacc<200))
matchit_results_inf <- matchit(treatment~first_adj+Institution+BuildingId+RoomType+time_since_inf_cut+
                                 age + risk + Sex, 
                               data=data_vacc,
                               exact=~first_adj+Institution+BuildingId+RoomType+time_since_inf_cut) 
summary(matchit_results_inf)

m2 <- matchit_results_inf %>% get_matches() 
m2
m2 <- m2 %>% mutate(status=if_else(censored_treatment=="inf", 1, 0))

m2_timevar <- m2 %>%
  group_by(id) %>% 
  uncount(survival_treatment,.remove = F) 

m2_timevar <- m2_timevar %>% 
  mutate(time1=seq(0,first(survival_treatment)-1),
         time2=seq(first(survival_treatment))) %>%
  mutate(first_adj=first_adj+time1)

m2_timevar2 <- m_timevar2 %>%
  mutate(time_since_vacc=time_since_vacc+time1,
         time_since_inf=time_since_inf+time1,
         time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F)) 

levels(m2_timevar$time_since_inf_cut)<-c(levels(m2_timevar$time_since_inf_cut),"None") 
m2_timevar$time_since_inf_cut[is.na(m2_timevar$time_since_inf_cut)] <- "None"
m2_timevar <- m2_timevar %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,365)", "[365,730)", "[730,Inf)")))

m2_timevar <- m2_timevar %>% mutate(status=if_else(time2<survival_treatment, 0, status))
m2_timevar <- m2_timevar %>% filter(treatment==0|(treatment==1&time_since_vacc<200))
m2_timevar <- m2_timevar %>% group_by(id) %>% mutate(survival_treatment=max(time2)) %>%
  group_by(subclass) %>% filter(time2<=min(survival_treatment))

m2_timevar_inc <- m2_timevar %>% left_join(incidence, by=c("Institution", "BuildingId", "first_adj"="end_offset")) %>%
  replace_na(list(inf=0, inc=0))

basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  m2_timevar_inc)
summary(basic_ve)

basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*inc_binary + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  m2_timevar_inc)
summary(basic_ve)

basic_ve <- coxph(Surv(time1, time2, status) ~ time_since_vacc*inc + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  m2_timevar_inc)
summary(basic_ve)

m2_timevar_inc_filter <- m2_timevar_inc %>% filter(inc>0) 
basic_ve <- coxph(Surv(time1, time2, status) ~ treatment*inf + treatment*time_since_vacc + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass),
                  m2_timevar_inc_filter)
summary(basic_ve)
