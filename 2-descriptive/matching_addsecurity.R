# # Sophia Tan 12/19/23
# test matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

data <- read_csv("cleaned_survival_data_prematch020624.csv") %>% 
  filter(first_adj=="2021-12-15") 

data <- data %>% mutate(time_since_vacc=(first_adj-last.vacc)%>%as.numeric(),
                        time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F, )) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"
data <- data %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,365)", "[365,730)", "[730,Inf)")))

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

security <- read_csv("cleaned_security_data.csv")
data <- data %>% left_join(security) 
data <- data %>% filter(Starting < "2022-05-14") %>% filter(Starting==first(Starting)) %>% 
  filter(!level%>%is.na())

data <- data %>% 
  mutate(censored_binaryvacc = if_else(last.dose.adj.binary==0&censored=="vaccinated", "vaccinated", censored_novacc),
         survival_binaryvacc = if_else(last.dose.adj.binary==0&censored=="vaccinated", survival, survival_novacc),
         status=if_else(censored_binaryvacc=="inf", 1, 0))
data <- data %>% filter(survival_binaryvacc>0)

# add exact match by first_adj if doing other periods
matchit_results_inf <- matchit(last.dose.adj.binary~Institution+BuildingId+RoomType+time_since_inf_cut+
                                 age + risk + Sex + level, 
                               data=data,
                               exact=~Institution+BuildingId+RoomType+time_since_inf_cut+Sex+level) 
summary(matchit_results_inf)

plot(matchit_results_inf)
m <- matchit_results_inf %>% get_matches() 

m %>% group_by(first_adj, last.dose.adj.binary) %>% summarise(n=n())

m <- m %>% group_by(subclass) %>% mutate(status=if_else(survival_binaryvacc>min(survival_binaryvacc), 0, status),
                                         survival_binaryvacc=min(survival_binaryvacc))

# basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary + time_since_inf_cut + frailty(subclass), m)
# summary(basic_ve)
# 
# for (i in c("None", "[0,365)", "[365,730)")) {
#   autoplot(survfit(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary + time_since_inf_cut, 
#                    m %>% filter(time_since_inf_cut==i)),
#            main = paste("Recent prior infection: ", i), ylim = c(0.7, 1), legTitle="vaccine") %>% print()
# }
# 
# basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary + time_since_inf_cut + 
#                     age + risk + Sex + level + 
#                     RoomType + factor(Institution) + frailty(subclass), m)
# 
# summary(basic_ve)
# cox.zph(basic_ve)
# plot(cox.zph(basic_ve),se=T, var=4, resid=F)
# 
# ggsurvplot(fit  = survfit(Surv(type = "right", time=survival_subclass, event=status) ~ last.dose.adj.binary, data=m), 
#            fun = "cloglog")
# 
# 
# basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ last.dose.adj.binary*t + time_since_inf_cut + 
#                     age + risk + Sex +
#                     RoomType + factor(Institution) + frailty(subclass), m)
# 
# summary(basic_ve)
# cox.zph(basic_ve)

# time-varying dataset
m_timevar <- m %>%
  group_by(id) %>% 
  uncount(survival_binaryvacc,.remove = F) 

m_timevar <- m_timevar %>% 
  mutate(time1=seq(0,first(survival_binaryvacc)-1),
         time2=seq(first(survival_binaryvacc))) %>%
  mutate(first_adj=first_adj+time1)

m_timevar <- m_timevar %>%
  mutate(time_since_inf=time_since_inf+time1,
         time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F)) 
levels(m_timevar$time_since_inf_cut)<-c(levels(m_timevar$time_since_inf_cut),"None") 
m_timevar$time_since_inf_cut[is.na(m_timevar$time_since_inf_cut)] <- "None"
m_timevar <- m_timevar %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,365)", "[365,730)", "[730,Inf)")))
m_timevar_summary <- m_timevar %>% group_by(id, subclass, Institution, BuildingId, RoomType, survival_binaryvacc, status, last.dose.adj.binary,
                                            age, risk, Sex, level, time_since_inf_cut) %>%
  summarise(time1=first(time1), time2=last(time2)) %>%
  mutate(status=if_else(time2<survival_binaryvacc, 0, status))

basic_ve <- coxph(Surv(time1, time2, status) ~ last.dose.adj.binary + time_since_inf_cut + 
                    age + risk + Sex + level + 
                    RoomType + factor(Institution) + frailty(subclass), m_timevar_summary)
summary(basic_ve)

vacc <- read_csv("complete_vaccine_data121523.csv") %>% select(ResidentId, Date, num_dose) %>% 
  mutate(Date=Date+14)
m_timevar <- m_timevar %>% left_join(vacc, by=c("ResidentId", "first_adj"="Date"))
m_timevar <- m_timevar %>% mutate(num_dose=if_else(time1==0, NA, num_dose))
m_timevar <- m_timevar %>% group_by(id) %>% fill(num_dose, .direction="down")
m_timevar <- m_timevar %>% group_by(id, num_dose) %>% mutate(time_since_vacc2=0:(n()-1))
m_timevar <- m_timevar %>%
  mutate(time_since_vacc=if_else(num_dose%>%is.na(),time_since_vacc+time1, time_since_vacc2))

m_timevar <- m_timevar %>% 
  mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 91, 182, 365, Inf), right = F)) 
m_timevar <- m_timevar %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("[0,91)", "[91,182)","[182,365)","[365,Inf)")))
levels(m_timevar$time_since_vacc_cut)<-c(levels(m_timevar$time_since_vacc_cut), "None") 
m_timevar$time_since_vacc_cut[is.na(m_timevar$time_since_vacc_cut)] <- "None"
m_timevar <- m_timevar %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,91)", "[91,182)","[182,365)","[365,Inf)")))

incidence <- read_csv("cleaned_incidence_data_rolling.csv") %>% select(Institution, BuildingId, start, end, inf, inc) %>% 
  mutate(end_offset=end+3)
m_timevar_inc <- m_timevar %>% left_join(incidence, by=c("Institution", "BuildingId", "first_adj"="end_offset")) %>%
  replace_na(list(inf=0, inc=0)) %>%
  mutate(status=if_else(time2<survival_binaryvacc, 0, status)) 


m_timevar_inc <- m_timevar_inc %>% ungroup() %>% select(!c(weights, Starting, Ending, Value, distance, time_since_vacc2, num_dose, start, end))
m_timevar_inc <- m_timevar_inc %>% arrange(ResidentId, first_adj) %>% 
  left_join(security %>% mutate(Starting=if_else(Starting<"2021-12-15", as.Date("2021-12-15"), Starting)) %>% 
              select(ResidentId, Starting, level), 
            by=c("ResidentId", "first_adj"="Starting")) %>%
  fill(level.y, .direction = "down")

# m_timevar_inc_summary <- m_timevar_inc %>% group_by(id, inf, time_since_vacc_cut, time_since_inf_cut) %>% 
#   summarise(time1=time1, consecutive=c(NA,diff(time1))) %>% filter(consecutive%>%is.na()|consecutive!=1) %>%
#   ungroup() %>% mutate(group=1:n()) %>% select(!c(inf, consecutive))
# 
# m_timevar_inc <- m_timevar_inc %>% left_join(m_timevar_inc_summary) %>% 
#   group_by(id) %>% fill(group, .direction = "down") %>% 
#   group_by(id, group, subclass, Institution, BuildingId, RoomType, survival_binaryvacc, last.dose.adj.binary,
#            age, risk, Sex, time_since_vacc_cut, time_since_inf_cut, status) %>%
#   summarise(inf=first(inf), time1=first(time1), time2=last(time2))%>%
#   ungroup()%>%
#   mutate(status=if_else(time2<survival_binaryvacc, 0, status)) %>%
#   arrange(id, time1)

# categorical incidence
m_timevar_inc <- m_timevar_inc %>% mutate(inc_cat = case_when(inc==0~0,
                                                              inc<2.5~1,
                                                              T~2)%>%factor())

basic_ve <- coxph(Surv(time1, time2, status) ~ last.dose.adj.binary + time_since_inf_cut + 
                    age + risk + Sex + level.y + 
                    RoomType + factor(Institution) + 
                    frailty(subclass), m_timevar_inc)
clean_results(basic_ve)

basic_ve <- coxph(Surv(time1, time2, status) ~ time_since_vacc_cut + time_since_inf_cut + 
                    age + risk + Sex + level + 
                    RoomType + factor(Institution) + 
                    frailty(subclass), m_timevar_inc)
clean_results(basic_ve)

basic_ve_int <- coxph(Surv(time1, time2, status) ~ last.dose.adj.binary*inc + time_since_inf_cut + 
                        age + risk + Sex + level.y + 
                        RoomType + factor(Institution) + 
                        frailty(subclass), m_timevar_inc)
clean_results(basic_ve_int)

time_since_ve_int <- coxph(Surv(time1, time2, status) ~ time_since_vacc_cut*inc + time_since_inf_cut + 
                             age + risk + Sex + level + 
                             RoomType + factor(Institution) + 
                             frailty(subclass), m_timevar_inc)
clean_results(time_since_ve_int)

m <- m %>% mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 91, 182, 365, Inf), right = F)) 
levels(m$time_since_vacc_cut)<-c(levels(m$time_since_vacc_cut), "None") 
m$time_since_vacc_cut[is.na(m$time_since_vacc_cut)] <- "None"
m <- m %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,91)", "[91,182)","[182,365)", "[365,Inf)")))

basic_ve <- coxph(Surv(survival_binaryvacc, status) ~ time_since_vacc_cut + time_since_inf_cut + 
                    age + risk + Sex +
                    RoomType + factor(Institution) + frailty(subclass), m)
summary(basic_ve)


# for (i in c("2021-12-15", "2022-05-15", "2022-08-15", "2022-12-15")) {
#   autoplot(survfit(Surv(survival, status) ~ time_since_vacc_cut, 
#                    m %>% filter(first_adj==i)),
#            main = paste("Subvariant start: ", i), legTitle="vaccine") %>% print()
# }

