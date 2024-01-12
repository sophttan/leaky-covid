# # Sophia Tan 12/19/23
# test matching

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/st/leaky/cleaned-data")

data <- read_csv("cleaned_survival_data_prematch011124.csv")

matchit_results <- matchit(last.dose.adj.binary~first_adj+Institution+BuildingId+has.past.inf, 
                           data=data,
                           exact=~first_adj+Institution+BuildingId+has.past.inf)

data <- data %>% mutate(time_since_inf=(first_adj-last.inf)%>%as.numeric(),
                        time_since_inf_cut=cut(time_since_inf, breaks=c(0, 182, 365, Inf), right = F, )) 
levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut),"None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"


matchit_results_inf <- matchit(last.dose.adj.binary~first_adj+Institution+BuildingId+has.past.inf+time_since_inf_cut, 
                               data=data,
                               exact=~first_adj+Institution+BuildingId+has.past.inf+time_since_inf_cut) 
summary(matchit_results_inf)

plot(matchit_results_inf)
m <- matchit_results_inf %>% get_matches() 

matchit_results <- matchit(last.dose.adj.binary~Institution+BuildingId+has.past.inf, 
                           data=data%>%filter(first_adj=="2021-12-15"),
                           exact=~Institution+BuildingId+has.past.inf)
