# Sophia Tan 12/15/23
# Clean nightly housing data 5/26/23

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

nh <- read_delim("NightlyHousing_20230526.csv", delim = ";", 
                 col_select = c("Night", "ResidentId", "RoomId", "Institution", "BuildingId"), 
                 n_max = 10000000)

min(nh$Night)
max(nh$Night)

nh_after_3_1_2020 <- nh %>% filter(Night >= "2020-03-01" & Night <= "2023-03-15")
rm(nh)
gc()

names(nh_after_3_1_2020)

nh_after_3_1_2020

residents <- data.frame(ResidentId = nh_after_3_1_2020$ResidentId%>%unique(),
                        group = rep(1:2, length.out=6137))

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% left_join(residents)

list_housing_data <- nh_after_3_1_2020 %>% group_by(group) %>% group_split()

for(i in 1:2){
  write_csv(list_housing_data[[i]]%>%select(!group),paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv"))
}

rm(list=ls())
gc()

for (i in 1:2) {
  d <- read_csv(paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv"))
  
  d <- d %>% group_by(ResidentId) %>% arrange(ResidentId, Night)

  d <- d %>% group_by(ResidentId, Institution, BuildingId) %>% mutate(consecutive_day=c(0, diff(Night)%>%as.numeric())<=1)
  
  move_buildings <- d %>% filter(Night==first(Night)|!consecutive_day) %>% group_by(ResidentId) %>% mutate(num_building=1:n())

  d <- d %>% left_join(move_buildings)
  
  d <- d %>% fill(num_building, .direction = "down")
  
  building_movement_summary <- d %>% 
    group_by(ResidentId, num_building) %>% summarise(days=n()) %>% 
    group_by(ResidentId) %>% summarise(num_movement=n(), days=mean(days))
  
  building_movement_summary$num_movement %>% hist(breaks=40)
  building_movement_summary$days %>% hist(breaks=20)
  
  resident_building_summary <- d %>% 
    group_by(ResidentId, num_building, Institution, BuildingId) %>% summarise(first=min(Night), last=max(Night), days=n())

  write_csv(resident_building_summary,paste0("D:/CCHCS_premium/st/leaky/cleaned-data/resident_movement", i,".csv"))
  
  
  residents_building <- d %>% group_by(Institution, BuildingId, Night) %>% summarise(residents=n())
  write_csv(residents_building,paste0("D:/CCHCS_premium/st/leaky/cleaned-data/resident_buildings", i,".csv"))
  
}

# summary_housing <- nh_after_3_1_2020 %>% group_by(ResidentId) %>% arrange(Night) %>% summarise(first=first(Night), last=last(Night), duration=n())
# summary_housing %>% write_csv("D:/CCHCS_premium/st/leaky/cleaned-data/housing_duration121823.csv")
# 
# nh_after_3_1_2020 %>% filter(is.na(RoomId)&is.na(Institution)&is.na(RoomType)) # no resident-nights where all info is missing
# 
# nh_omicron <- nh_after_3_1_2020_subset %>% filter(Night >= "2020-12-01")
# 
# roomtypes <- nh_omicron %>% group_by(Institution, RoomId, RoomType) %>% summarise(count=n()) %>% group_by(Institution, RoomId)
# roomtypes %>% filter(all(RoomType%>%is.na())) # 2 rooms here have no roomtype
# roomtypes %>% filter(any(RoomType%>%is.na()) & n()>1) # 4 rooms have roomtypes that have some missingness in reporting
# roomtypes %>% filter(!any(RoomType %>% is.na()) & n()>1) # 1 rooms has two different roomtypes listed - mostly roomtype 4, some reports of roomtype 2
# roomtypes <- roomtypes %>% filter(!RoomType %>% is.na()) %>% arrange(desc(count)) %>% summarise(RoomType=first(RoomType))
# nh_omicron <- nh_omicron %>% left_join(roomtypes, c("Institution", "RoomId"))
# nh_omicron <- nh_omicron %>% rename("RoomType" = "RoomType.y") %>% select(!RoomType.x)
# nh_omicron <- nh_omicron %>% mutate(RoomType = ifelse(Institution ==13, 1, RoomType))
# 
# weird_buildings <- nh_omicron %>% 
#   group_by(Institution, RoomId, BuildingId) %>% 
#   group_keys()
# w <- weird_buildings %>% group_by(Institution, RoomId) %>% 
#   filter(length(unique(BuildingId))>1) # Institution and RoomId should be identifiable, but there are some rooms that are associated with 2 buildings (1% of rooms)
# w
# 
# pdf("D:/CCHCS_premium/st/indirects/testing/building.pdf")
# for (room in unique(w$RoomId)) {
#   print(nh_omicron %>% filter(RoomId==room) %>% 
#           ggplot(aes(Night, group=as.factor(BuildingId), fill=as.factor(BuildingId))) + 
#           geom_histogram(position="dodge") + 
#           labs(title=room))
# }
# dev.off()
# 
# # some rooms have multiple building ids
# w %>% 
#   write_csv("D:/CCHCS_premium/st/indirects/cleaned-data/rooms_mult_buildings_vaccperiod.csv")
# 
# write_csv(nh_omicron, "D:/CCHCS_premium/st/indirects/cleaned-data/housing_vaccperiod.csv")
# 
# 
