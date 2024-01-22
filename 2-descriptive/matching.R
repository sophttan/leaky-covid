# # Sophia Tan 12/19/23
# test matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

data <- read_csv("cleaned_survival_data_prematch011224.csv")

matchit_results <- matchit(last.dose.adj.binary~first_adj+Institution+BuildingId+has.past.inf, 
                           data=data,
                           exact=~first_adj+Institution+BuildingId+has.past.inf)

data <- data %>% mutate(time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 365, 730, Inf), right = F, )) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"


matchit_results_inf <- matchit(last.dose.adj.binary~first_adj+Institution+BuildingId+has.past.inf+time_since_inf_cut, 
                               data=data,
                               exact=~first_adj+Institution+BuildingId+has.past.inf+time_since_inf_cut) 
summary(matchit_results_inf)

plot(matchit_results_inf)
m <- matchit_results_inf %>% get_matches() 

m %>% group_by(first_adj, last.dose.adj.binary) %>% summarise(n=n())
m

m <- m %>% mutate(status=if_else(censored=="inf", 1, 0))

basic_ve <- coxph(Surv(survival, status) ~ last.dose.adj + time_since_inf_cut + frailty(subclass), 
                  m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)

par(mfrow = c(2, 2))
for (i in c("None", "[0,365)", "[365,730)")) {
  autoplot(survfit(Surv(survival, status) ~ last.dose.adj + time_since_inf_cut, 
                   m %>% filter(first_adj=="2021-12-15" & time_since_inf_cut==i)),
           main = paste("Recent prior infection: ", i), ylim = c(0.7, 1), legTitle="vaccine") %>% print()
}

levels(m$time_since_vacc_cut)<-c("None", levels(m$time_since_vacc_cut)) 
m$time_since_vacc_cut[is.na(m$time_since_vacc_cut)] <- "None"
m <- m %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,91)", "[91,182)","[182,365)", "[365,Inf)")))

m <- m %>% mutate(last.dose.adj.p1 = if_else(last.dose.adj >= 3, 3, last.dose.adj), 
                  time_since_vacc=if_else(last.dose.adj.binary==0, difftime(first_adj, "2020-03-01")%>%as.numeric(), time_since_vacc))
basic_ve <- coxph(Surv(survival, status) ~ time_since_vacc*last.dose.adj.p1 + time_since_inf_cut + frailty(subclass), m %>% filter(first_adj=="2021-12-15"))
summary(basic_ve)


for (i in c("2021-12-15", "2022-05-15", "2022-08-15", "2022-12-15")) {
  autoplot(survfit(Surv(survival, status) ~ time_since_vacc_cut, 
                   m %>% filter(first_adj==i)),
           main = paste("Subvariant start: ", i), legTitle="vaccine") %>% print()
}
