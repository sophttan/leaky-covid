# Sophia Tan 12/15/23
# Clean nightly housing data 5/26/23

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

nh <- read_delim("NightlyHousing_20230526.csv", delim = ";", 
                 col_select = c("Night", "ResidentId", "RoomId", "Institution", "BuildingId", "RoomType"))#, 
                 # n_max = 50000000)

min(nh$Night)
max(nh$Night)

nh_after_3_1_2020 <- nh %>% filter(Night >= "2020-03-01" & Night <= "2023-03-15")
rm(nh)
gc()

names(nh_after_3_1_2020)

nh_after_3_1_2020

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% filter(!BuildingId %>% is.na())

nh_after_3_1_2020_roomtypes <- nh_after_3_1_2020 %>% group_by(Institution, RoomId, RoomType) %>% 
  summarise(n=n()) %>%
  group_by(Institution, RoomId) %>% filter(!RoomType %>% is.na()) %>% 
  arrange(Institution, RoomId, desc(n)) %>% select(!n) %>%
  distinct(Institution, RoomId, .keep_all = T)

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% left_join(nh_after_3_1_2020_roomtypes, by=c("Institution", "RoomId"))
nh_after_3_1_2020 <- nh_after_3_1_2020 %>% 
  select(!RoomType.x) %>% rename("RoomType"="RoomType.y") 

nh_after_3_1_2020_testroomtypes <- nh_after_3_1_2020 %>% group_by(Institution, RoomId, Night) %>% 
  summarise(RoomType=first(RoomType), n=n())
nh_after_3_1_2020_testroomtypes <- nh_after_3_1_2020_testroomtypes %>% mutate(roomtype_simple=if_else(n<3, "Cell", "Dorm"))
nh_after_3_1_2020_testroomtypes %>% group_by(RoomType) %>% summarise(sum(n>2)/n()) # almost all cells and rooms have <3 people and almost all dorms have >3 people

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% 
  mutate(RoomType.simple=case_when(RoomType%in%c(1,2,4,6)~0,
                                   RoomType%in%c(3,5)~1,
                                   T~NA))

# Residents without RoomType date (n=2 with ResidentIds 1629006254 1630486718)
(nh_after_3_1_2020 %>% filter(Night>="2021-12-15"&RoomType%>%is.na()))$ResidentId %>% unique()

residents <- nh_after_3_1_2020$ResidentId%>%unique()
residents <- data.frame(ResidentId = residents,
                        group = rep(1:4, length.out=length(residents)))

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% left_join(residents)

list_housing_data <- nh_after_3_1_2020 %>% group_by(group) %>% group_split()

for(i in 1:4){
  write_csv(list_housing_data[[i]]%>%select(!group),paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv"))
}

rm(list=ls())
gc()

for (i in 1:4) {
  d <- read_csv(paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv"))
  
  d <- d %>% group_by(ResidentId) %>% arrange(ResidentId, Night)

  d <- d %>% group_by(ResidentId, Institution, BuildingId) %>% 
    mutate(consecutive_day=c(0, diff(Night)%>%as.numeric())<=1) 
  
  move_buildings <- d %>% filter(Night==first(Night)|!consecutive_day) %>% group_by(ResidentId) %>% mutate(num_building=1:n())

  d <- d %>% left_join(move_buildings)
  
  d <- d %>% fill(num_building, .direction = "down")
  
  building_movement_summary <- d %>% 
    group_by(ResidentId, num_building) %>% summarise(days=n()) %>% 
    group_by(ResidentId) %>% summarise(num_movement=n(), days=mean(days))
  
  building_movement_summary$num_movement %>% hist(breaks=40)
  building_movement_summary$days %>% hist(breaks=20)
  
  resident_building_summary <- d %>% 
    group_by(ResidentId, num_building, Institution, BuildingId) %>% summarise(RoomType=first(RoomType.simple), first=min(Night), last=max(Night), days=n())

  write_csv(resident_building_summary,paste0("D:/CCHCS_premium/st/leaky/cleaned-data/resident_movement", i,".csv"))
  
  
  residents_building <- d %>% group_by(Institution, BuildingId, Night) %>% summarise(n=n(), residents=unique(ResidentId))
  write_csv(residents_building,paste0("D:/CCHCS_premium/st/leaky/cleaned-data/resident_buildings", i,".csv"))
  
}

