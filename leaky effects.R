# Sophia Tan 11/27/23

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)

d <- read_csv("complete-data-081423.csv")

specific_rooms <- d %>% select(Institution, BuildingId, RoomId) %>%
  distinct() %>% 
  group_by(Institution, RoomId) %>% filter(n()==1)

d <- d %>%
  mutate(weeks = difftime(Day, as.Date("2020-03-01") - 1, units = "weeks") %>% as.numeric() %>% round())

d <- d %>% right_join(specific_rooms)

inf <- d %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% summarise_all(first)

inf %>% select(ResidentId, Day, weeks)

week_infections <- inf %>% group_by(Institution, BuildingId, weeks) %>% summarise(inf=n()) 

week_residents <- d %>% group_by(Institution, BuildingId, weeks) %>% summarise(res=length(unique(ResidentId))) 

week_residents2 <- week_residents %>% group_by(Institution, BuildingId) %>% filter(all(res>=50))

inf_res_week <- week_infections %>% right_join(week_residents2) %>% arrange(Institution, BuildingId, weeks) %>%
  replace_na(list(inf=0, res=0)) %>% filter(!Institution %>% is.na() & !BuildingId %>% is.na())

inf_res_week <- inf_res_week %>% mutate(inc=inf/res*100)

weeks_day <- (d %>% group_by(weeks) %>% summarise(Day=min(Day)))[seq(1, 146, 5),]

pdf("building_inc_week.pdf")
for(inst in inf_res_week$Institution%>%unique()) {
  
  print(inf_res_week %>% filter(Institution==inst) %>%
          ggplot(aes(weeks, inc,
                     group=as.factor(BuildingId), 
                     color=as.factor(BuildingId))) + 
          geom_line() + 
          scale_x_continuous(breaks=weeks_day$weeks, labels=weeks_day$Day) + 
          ggtitle(inst) + 
          theme(legend.position = "none", 
                axis.text.x = element_text(angle=90)))
  
}
dev.off()

# use institution 30 as test example
inst30 <- inf_res_week %>% filter(Institution==30)
inst30 %>%
  ggplot(aes(weeks, inc,
             group=as.factor(BuildingId), 
             color=as.factor(BuildingId))) + 
  geom_line() + 
  scale_x_continuous(breaks=weeks_day$weeks, labels=weeks_day$Day) + 
  ggtitle(inst) + 
  theme(axis.text.x = element_text(angle=90))

inst30 %>% group_by(BuildingId) %>% 
  filter(weeks >= 91 & weeks <= 111 & inc > 0) %>%
  summarise(avg_inc = sum(inf)/mean(res)*100, res=mean(res))

inst30 %>% group_by(BuildingId) %>% 
  filter(weeks >= 91 & weeks <= 111) %>% 
  ggplot(aes(weeks, inc,
             group=as.factor(BuildingId), 
             color=as.factor(BuildingId))) + 
  geom_line() + 
  ggtitle(30) + 
  theme(axis.text.x = element_text(angle=90))

high <- c(-968463058, -968331986, -426676946, -107909634)
low <- c(-409899730, -107909842)

buildings <- inst30$BuildingId %>% unique()
low <- c(-426676754,-426676738, -409899730,-426676978)
medium <- c()
high <- buildings[!buildings %in% low & !buildings %in% medium]

inst30_full <- d %>% filter(weeks >= 91 & weeks <= 111) %>% filter(Institution==30)
(inst30_full %>% 
    group_by(ResidentId) %>% 
    summarise(same_building=unique(BuildingId)))%>% filter(n()>2)

inst30_full %>% filter(ResidentId==1611667198) %>% select(BuildingId, RoomId, Day, Result, num_pos)

inst30_full <- inst30_full %>% mutate(building_group = case_when(BuildingId %in% high~"high",
                                                                 BuildingId %in% medium~"medium",
                                                                 T~"low"))
inst30_full %>% select(ResidentId, Day, Result, BuildingId, building_group)

inst30_full <- inst30_full %>% arrange(ResidentId, Day) %>% group_by(ResidentId, building_group) %>% mutate(diff_Day=c(0, diff(Day))) 

inst30_full_summary <- inst30_full %>% 
  select(Institution, BuildingId, ResidentId, building_group, Day, diff_Day) %>%
  filter(diff_Day==0|diff_Day>1) %>% ungroup() %>% mutate(id=1:n())

inst30_full <- inst30_full %>% left_join(inst30_full_summary) %>% fill(id, .direction="down")

inst30_full_keep <- inst30_full %>% group_by(id) %>% filter(n()>5)
inst30_full_keep 

inst30_full_keep_summary <- inst30_full_keep %>% 
  summarise(ResidentId = first(ResidentId), 
            id = first(id), 
            building_group = first(building_group),
            last_inf = first(num_pos),
            vacc_doses = mean(num_dose_adjusted) %>% round(),
            vacc_doses_adjusted = ifelse(vacc_doses >= 3, 3, vacc_doses),
            vacc = ifelse(mean(num_dose_adjusted) > 1, 1, 0),
            first = first(Day),
            last=last(Day)) 

new_inf <- inst30_full_keep %>% select(id, Day, num_pos, Result) %>% filter(!Result %>% is.na()) %>%
  filter(any(Result=="Positive"))

new_inf <- new_inf %>% filter(Result=="Positive") %>% distinct(id, .keep_all = T) 

new_inf

survival_data <- inst30_full_keep_summary %>%
  left_join(new_inf) %>% 
  mutate(last=if_else(Result%>%is.na(), last, Day)) %>% 
  mutate(survival = (last-first+1)%>%as.numeric()) %>%
  mutate(status=ifelse(Result%>%is.na(),0,1)) %>% 
  select(!c(Day, num_pos, Result))

remove_recent_inf <- survival_data %>% group_by(ResidentId) %>% filter(n()>1&any(status==1))
remove_recent_inf <- remove_recent_inf %>% filter(status==1) %>% 
  summarise_all(first) %>% select(ResidentId, last) %>% rename("last_survival"="last")
survival_data <- survival_data %>% left_join(remove_recent_inf) %>% filter(is.na(last_survival)|first<last_survival)

survival_data %>% group_by(vacc, building_group) %>% summarise(res=n(), inc=sum(status)/sum(survival)*100)
survival_data

survival_data %>% group_by(vacc_doses_adjusted, building_group) %>% summarise(res=n(), inc=sum(status)/sum(survival)*100)


library(survival)
library(survminer)
library(ggfortify)
fit <- survfit(Surv(survival, status, type="right")~building_group + vacc, data = survival_data)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  # scale_fill_discrete(name=element_blank(), 
  #                     labels=c("High inc, Unvacc", "High inc, Vacc",
  #                              "Low inc, Unvacc", "Low inc, Vacc")) + 
  guides(color=F)

fit <- survfit(Surv(survival, status, type="right")~building_group, data = survival_data %>% filter(vacc_doses_adjusted==1))
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

fit <- survfit(Surv(survival, status, type="right")~building_group, data = survival_data %>% filter(vacc_doses_adjusted==2))
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

fit <- survfit(Surv(survival, status, type="right")~building_group, data = survival_data %>% filter(vacc_doses_adjusted==3))
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

survival_data %>% 
  group_by(vacc_doses_adjusted, building_group) %>% 
  summarise(res=n(), inc=sum(status)/sum(survival)*100) %>%
  group_by(vacc_doses_adjusted) %>% 
  mutate(inc_diff = ((inc[2])-(inc))/(inc))

vacc <- read_csv("cleaned_vaccination_data.csv") %>%
  select(ResidentId, num_dose, Date) %>% 
  rename("vaccDay"="Date")
survival_data <- survival_data %>% 
  left_join(vacc, by=c("ResidentId", "vacc_doses_adjusted"="num_dose"))
survival_data <- survival_data %>%
  left_join(inf %>% select(ResidentId, num_pos, Day) %>% rename("infDay"="Day"), 
            by=c("ResidentId", "last_inf"="num_pos"))

survival_data <- survival_data %>% 
  mutate(time_since_vacc = (first-vaccDay)%>%as.numeric(),
         time_since_inf = (first-infDay)%>%as.numeric())

survival_data <- survival_data %>% 
  mutate(most_recent=pmax(vaccDay, infDay,na.rm=T)) %>%
  mutate(first_adjusted=pmax(most_recent, first,na.rm=T)) %>%
  mutate(survival_adjusted=(last-first_adjusted+1)%>%as.numeric())%>%
  mutate(time_since_recent=(first_adjusted-most_recent)%>%as.numeric())

survival_data <- survival_data %>% filter(survival_adjusted >= 0)

survival_data <- survival_data %>%
  mutate(time_since_recent_cat = cut(time_since_recent, breaks = c(0, 90, Inf), right = F)) %>%
  replace_na(list(time_since_recent_cat="[90,Inf)"))

survival_data$time_since_recent %>% hist()
survival_data$survival_adjusted %>% hist()

fit <- survfit(Surv(survival_adjusted, status, type="right")~building_group + vacc_doses_adjusted + time_since_recent_cat, data = survival_data)
ggsurvplot_facet(fit, 
                 survival_data, 
                 facet.by = c("vacc_doses_adjusted", "time_since_recent_cat"),) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) 
  #guides(color=F)

fit <- survfit(Surv(survival_adjusted, status, type="right")~building_group + vacc_doses_adjusted, data = survival_data %>% filter(time_since_recent == 0|vacc==0))
ggsurvplot_facet(fit, 
                 survival_data, 
                 facet.by = "vacc_doses_adjusted") + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) 
#guides(color=F)
