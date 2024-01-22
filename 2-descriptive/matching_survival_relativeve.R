# # Sophia Tan 1/18/23
# test relative ve matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

demo <- read_csv("demographic121523.csv") %>% mutate(age=2022-BirthYear)

data <- read_csv("cleaned_survival_data_prematch011224.csv") 

data <- data %>% mutate(time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F, )) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"

data_vacc <- data %>% filter(last.dose.adj.binary>0) %>% 
  mutate(treatment=if_else(last.dose.adj==2, 0, 1))

matchit_results_inf <- matchit(treatment~first_adj+Institution+BuildingId+has.past.inf+time_since_inf_cut, 
                               data=data_vacc,
                               exact=~first_adj+Institution+BuildingId+has.past.inf+time_since_inf_cut) 
summary(matchit_results_inf)

m <- matchit_results_inf %>% get_matches() 
m

m %>% group_by(first_adj, treatment) %>% summarise(n=n())

m <- m %>% mutate(status=if_else(censored=="inf", 1, 0))

basic_ve <- coxph(Surv(survival, status) ~ treatment + time_since_inf_cut + frailty(subclass), 
                  m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)

par(mfrow = c(2, 2))
for (i in c("None", "[0,365)", "[365,730)")) {
  autoplot(survfit(Surv(survival, status) ~ treatment + time_since_inf_cut, 
                   m %>% filter(first_adj=="2021-12-15" & time_since_inf_cut==i)),
           main = paste("Recent prior infection: ", i), ylim = c(0.7, 1), legTitle="vaccine") %>% print()
}

levels(m$time_since_vacc_cut)<-c("None", levels(m$time_since_vacc_cut)) 
m$time_since_vacc_cut[is.na(m$time_since_vacc_cut)] <- "None"
m <- m %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,365)", "[365,730)","[730,Inf)")))

m <- m %>% left_join(demo)

basic_ve <- coxph(Surv(survival, status) ~ treatment + frailty(subclass), 
                  m %>% filter(first_adj=="2021-12-15"))

basic_ve <- coxph(Surv(survival, status) ~ treatment * time_since_vacc + frailty(subclass), 
                  m %>% filter(first_adj=="2021-12-15"))

basic_ve <- coxph(Surv(survival, status) ~ treatment*tt(time_since_vacc) + frailty(subclass), 
                  tt=function(x,t,...){x+t},
                  m %>% filter(first_adj=="2021-12-15"))

basic_ve <- coxph(Surv(survival, status) ~ treatment*tt(time_since_vacc) + time_since_inf_cut + frailty(subclass), 
                  tt=function(x,t,...){x+t},
                  m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)


basic_ve <- coxph(Surv(survival, status) ~ treatment + tt(time_since_vacc) + time_since_inf_cut + frailty(subclass), 
                  tt=function(x,t,...){x+t},
                  m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)

basic_ve <- coxph(Surv(survival, status) ~ tt(time_since_vacc) + time_since_inf_cut + frailty(subclass), 
                  tt=function(x,t,...){x+t},
                  m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)


basic_ve <- coxph(Surv(survival, status) ~ treatment*tt(time_since_vacc) + time_since_inf_cut + 
                    age + frailty(subclass) + factor(Institution), 
                  tt=function(x,t,...){x+t},
                  m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)
