library(tidyverse)
library(stringr)
path<-"~/Documents/Luftqualitaet/Daten/BW"
path_Rdat<-"~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path_Rdat)
# BW_15stations
BW_15stations_NO2%>% summary()#00-20/7
#  BW_all_data.RData teilweise aktualisiert bis 19/5
load(file= file.path(path_Rdat,"BW_all_data.RData"))# BW.all_data
names(BW.all_data)# 12 Stationen
summary(BW.all_data$Alb$Alb.NO2)
# BW_list_tbl
load(file= file.path(path_Rdat,"BW_list_tbl.RData"))
BW_list_tbl$Rt_pomol%>% summary() # 22 stationen  bis 12 

saveRDS (BW_list_tbl,file = file.path(path_Rdat,"BW_list_tbl.rds"))
          #BW_station.RData

names(BW_list_tbl)
summary(BW_list_tbl$Egg)
save(BW_15stations_NO2, file = file.path(path_Rdat,"BW_15station_NO2.RData"))
# BW_stations_NO2_tbl
load(file= file.path(path_Rdat,"BW_stations_NO2_tbl.RData"))
BW_stations_NO2_tbl%>% summary()
BW_stations_NO2_tbl$name%>% levels()
# BW_stations_list
load(file= file.path(path_Rdat,"BW_stations_list.RData"))
summary(Stationsliste)
save(BW_15stations_NO2, file = file.path(path_Rdat,"BW_15station_NO2.RData"))
#  NO2_data.RData
load(file= file.path(path_Rdat,"NO2_data.RData"))
NO2_data%>%summary() # bis 20/7
NO2_data$station%>% levels() 
# NO2 import
load(file= file.path(path_Rdat,"NO2_import_data.Rdata"))
summary(NO2_data$Brn)
# RT.RData
load(file= file.path(path_Rdat,"RT.Rdata"))
RT_data$Rt.no2%>% summary()
summary(Stgt_NO2)
# use rds file format
BW_list_tbl<-readRDS(file= file.path(path_Rdat,"BW_list_tbl.rds"))
BW_list_tbl%>% summary()