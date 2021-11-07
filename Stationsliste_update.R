library(tidyverse)
library(knitr)
library(lubridate)
path_Rdat<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
load(file.path(path_Rdat,"stationlist.RData"))
load(file.path(path_Rdat,"Stationsliste.RData"))

head(stationlist,20)
Stationsliste<- stationlist%>% 
  right_join(Stationsliste, by=c("ID"="Stationsnummer","Messstelle","Ost_UTM","Nord_UTM"))#
saveRDS(Stationsliste,file.path(path_Rdat,"Stationsliste.rds"))  
Stationsliste%>% tail(15)
head(Stationsliste,19)
NROW(Stationsliste)
load("~/Documents/Luftqualitaet/Daten/BW_Rdat/BW_list_tbl.RData")
NROW(BW_list_tbl)
saveRDS(BW_list_tbl,"~/Documents/Luftqualitaet/Daten/BW_Rdat/BW_list_tbl.rds" )
BW_list_tbl<-readRDS("~/Documents/Luftqualitaet/Daten/BW_Rdat/BW_list_tbl.rds")
head(BW_list_tbl)

#BW_list_tbl<-BW_list_tbl%>% map(~ mutate(.,station = station%>% droplevels()%>%factor()))

summary(BW_list_tbl)                  
summary(BW_list_tbl[[6]])
#BW_list_tbl<-BW_list_tbl%>% map(~ mutate(.,name = name%>% factor()%>% droplevels()))                     

names(BW_list_tbl)
BW_list_tbl[[4]]$name%>%levels()
BW_list_tbl%>% map_chr(~.$name %>% levels())
BW_list_tbl%>% map_chr(~.$station %>% levels())
lookup<- tibble(name = BW_list_tbl%>% map_chr(~.$name %>% levels())%>%factor(),
                ID = BW_list_tbl%>% map_chr(~.$station %>% levels())%>%factor())
#
NROW(lookup)#22
namedlist<-lookup%>% left_join(Stationsliste, by=c("ID"="Stationsnummer"))
NROW(namedlist)#22
head(namedlist)
namedlist$name
namedlist %>% filter(name == "Nck")
namedlist[12,1]# "Stg_Nck"
NROW(Stationsliste)# 29
summary(Stationsliste)# 29 Stationen
stations<-Stationsliste$name
namedlist$name<- namedlist$name%>% recode_factor("Nck"="Stg_Nck")

ruralst <- c("Alb","Odw","Sws") 
trafic<- c("Lbg_Friedr","Stg_Nck","Rt_leder")
Stationsliste_rurl<-namedlist%>% 
  subset(name %in% ruralst)%>% 
  mutate(rural = TRUE)
Stationsliste_traf<-namedlist%>% 
  subset(name %in% trafic)%>% 
  mutate(trafic = TRUE)
Stationsliste_urbn<-namedlist%>% 
  subset((!name %in% trafic)&(!name %in% rural))%>% 
  mutate(urban = TRUE)
Stationsliste_urbn<-Stationsliste_urbn[-c(11,14,16),]
stationlist<-bind_rows(Stationsliste_rurl,Stationsliste_traf,Stationsliste_urbn)
listednames<-names(BW_list_tbl)
names(stationlist)
stationlist$name #19
stationlist$name<-stationlist$name%>% droplevels()
stationlist$name%>% levels()
stationlist$name<-stationlist$name%>% recode_factor("Can"= "Stg_Can")
save(stationlist,file = file.path(path_Rdat,"stationlist.RData"))
BW_list_tbl%>% subset(!(name %>% c("Stg_Schwz","Stg_SZ_afu")))
