# # Sophia Tan 1/18/23
# test relative ve matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

demo <- read_csv("demographic121523.csv") %>% mutate(age=2022-BirthYear)

data <- read_csv("cleaned_survival_data_prematch011224.csv") 

data <- data %>% mutate(time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F)) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"

data_vacc <- data %>% filter(last.dose.adj.binary>0) %>% 
  mutate(treatment=if_else(last.dose.adj==2, 0, 1))

matchit_results_inf <- matchit(treatment~first_adj+Institution+BuildingId+RoomType+time_since_inf_cut+
                                 age + risk + Sex, 
                               data=data_vacc,
                               exact=~first_adj+Institution+BuildingId+RoomType+time_since_inf_cut) 
summary(matchit_results_inf)

m <- matchit_results_inf %>% get_matches() 
m

m %>% group_by(first_adj, treatment) %>% summarise(n=n())

m <- m %>% 
  mutate(censored_treatment = if_else(treatment==0&censored=="vaccinated", "vaccinated", censored_novacc),
         survival_treatment = if_else(treatment==0&censored=="vaccinated", survival, survival_novacc),
         status=if_else(censored_treatment=="inf", 1, 0))
m <- m %>% group_by(subclass) %>% mutate(status=if_else(survival_treatment==min(survival_treatment), status, 0),
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




