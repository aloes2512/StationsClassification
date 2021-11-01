library(lubridate)
library(tidyverse)
library(readxl)
library(xts)
library(stringr)
path<-"~/Documents/Luftqualitaet/Daten/BW"
path_Rdat<-"~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path_Rdat)
load(file= file.path(path_Rdat,"BW.RData"))# BW.all_data
names(BW.all_data)
load(file.path(url_Rdat,"BW_list_tbl.RData"))
names(BW_list_tbl)
# read function

rd_xlsx_lubw<- function(path,dat.file){df<-read_xlsx(file.path(path,dat.file),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor(Messstelle),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         Wert =as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,Wert)
return(df)
}
# add O3 measurements complete Eggenstein
Alb_O3<-rd_xlsx_lubw(path,"Alb_47650_O3_00_21.xlsx")
Egg_O3<-rd_xlsx_lubw(path,"Egg_4445_O3_00_21.xlsx")%>% rename(O3=Wert)
Egg_NO2 <-rd_xlsx_lubw(path,"Egg_4445_NO2_00_21.xlsx")%>% rename(NO2=Wert)
Egg_NO <- rd_xlsx_lubw(path,"Egg_4445_NO_00_21.xlsx")%>% rename(NO=Wert)
Egg_CO <- rd_xlsx_lubw(path,"Egg_4445_CO_00_21.xlsx")%>% rename(CO=Wert)
Egg_CO$datetime<- seq(first(Egg_CO$datetime),by ="30 min", length.out= NROW(Egg_CO))

Egg_Temp<-rd_xlsx_lubw(path,"Egg_4445_Temp_15_21.xlsx")%>% rename(Temp=Wert)
Egg_Glbl<-rd_xlsx_lubw(path,"Egg_4445_Glbl_15_21.xlsx")%>% rename(Glbl=Wert)
Egg_WG<- rd_xlsx_lubw(path,"Egg_4445_WG_00_21.xlsx")%>% rename(WG=Wert)
Egg_WR <- rd_xlsx_lubw(path,"Egg_4445_WR_00_21.xlsx")%>% rename(WR=Wert)
Egg_data<-left_join(Egg_NO2,Egg_NO)%>%
  left_join(Egg_O3)%>% 
  left_join(Egg_CO)%>% 
  left_join(Egg_Temp)%>%
  left_join(Egg_WG)%>%
  left_join(Egg_WR)%>% mutate(name="Eggenstein")%>%
  dplyr::select(station,name,datetime,NO2,NO,O3,CO,Temp,WG,WR)%>%
  mutate(name = as_factor(name))
BW_list_tbl$Egg<-Egg_data
Fri_O3<-rd_xlsx_lubw(path,"Fri_4471_O3_00_21.xlsx")
Heid_O3 <- rd_xlsx_lubw(path,"Heid_4453_O3_00_21.xlsx")
Heil_O3 <-rd_xlsx_lubw(path,"Heil_4461_O3_00_21.xlsx")
Lbg_O3 <-rd_xlsx_lubw(path,"Lbg_4463_O3_00_20.xlsx")
Rt_O3<- rd_xlsx_lubw(path,"Rt_4470_O3_00_21.xlsx")
Odw_O3 <- rd_xlsx_lubw(path,"Odw_76118_O3_00_11.xlsx")
Rt.l_NO2<-BW.all_data$Rt.l$Rt.l.no2%>% mutate(name= as_factor("Rt_leder"))
Rt.l_NO<-BW.all_data$Rt.l$Rt.l.no
Rt.l_CO<-BW.all_data$Rt.l$Rt.l.co# Start" 2015-12-31 00"
range(Rt.l_CO$datetime)# "2015-12-31 UTC" "2021-01-01 UTC"
Rt.l_data<- left_join(Rt.l_NO2,Rt.l_NO)%>%
  left_join(Rt.l_CO)%>% 
  dplyr::select(station,name,datetime,NO2,NO,CO)
head(Rt.l_data)
Rt.l_data %>% ggplot(aes(x=datetime, y= NO2))+
  geom_smooth(method = "auto",col = "red")+
  geom_smooth(method = "auto",aes(x= datetime, y= NO),col = "purple")
NROW(Rt.l_data)#120766
sum( is.na(Rt.l_data$CO))#77565
which(!is.na(is.na(Rt.l_data$CO)))
summary(Rt.l_data)
BW_list_tbl$Rt_leder<-Rt.l_data

Odw_O3$datetime<- seq(from= first(Odw_O3$datetime), by = "30 min",length.out=NROW(Odw_O3))
range(Odw_O3$datetime)
Frei_O3<-rd_xlsx_lubw(path,"Frei_4462_O3_00_21.xlsx")
BW_list_tbl$Alb<-BW_list_tbl$Alb%>% left_join(Alb_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Egg<-BW_list_tbl$Egg%>% left_join(Egg_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Frei<-BW_list_tbl$Frei%>% left_join(Frei_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Fri<-BW_list_tbl$Fri%>% left_join(Fri_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Heid<-BW_list_tbl$Heid%>%left_join(Heid_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Heil<-BW_list_tbl$Heil%>%left_join(Heil_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Lbg_weimar<-BW_list_tbl$Lbg_weimar%>%left_join(Lbg_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Rt_pomol<-BW_list_tbl$Rt_pomol%>% left_join(Rt_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)
BW_list_tbl$Odw<- BW_list_tbl$Odw%>% left_join(Odw_O3)%>% mutate(O3=Wert)%>% dplyr::select(-Wert)

#BW_list_tbl$Sws<-BW_list_tbl$Sws %>% dplyr::select(station,name,datetime =stnd,NO2,Temp,WG,O3,NO)
save(BW_list_tbl,file = file.path(url_Rdat,"BW_list_tbl.RData"))
# Mannheim- Mitte  4473
Man_NO2<-rd_xlsx_lubw(path,"Man_4473_NO2_00_21.xlsx")%>%
mutate(name="Man-Mitte")%>%
  dplyr::select(station,name,datetime,NO2=Wert)
head(Man_NO2)
Man_Temp<- rd_xlsx_lubw(path,"Man_4473_Temp_00_21.xlsx")%>%
  mutate(name="Man-Mitte")%>%
  dplyr::select(station,name,datetime,Temp=Wert)
head(Man_Temp)
Man_NO<- rd_xlsx_lubw(path,"Man_4473_NO_00_21.xlsx")%>%
  mutate(name="Man-Mitte")%>%
  dplyr::select(station,name,datetime,NO=Wert)
head(Man_NO)
Man_O3<- rd_xlsx_lubw(path,"Man_4473_O3_00_21.xlsx")%>%
  mutate(name="Man-Mitte")%>%
  dplyr::select(station,name,datetime,O3=Wert)
head(Man_O3)
Man_WG<- rd_xlsx_lubw(path,"Man_4473_WG_00_21.xlsx")%>%
  mutate(name="Man-Mitte")%>%
  dplyr::select(station,name,datetime,WG=Wert)
head(Man_WG)
Man_WR<- rd_xlsx_lubw(path,"Man_4473_WR_00_21.xlsx")%>%
  mutate(name="Man-Mitte")%>%
  dplyr::select(station,name,datetime,WR=Wert)
head(Man_WR)
# Stuttgart am Neckartor
Nck_NO2<-rd_xlsx_lubw(path,"Nck_76361_NO2_03_21.xlsx")%>%
  mutate(name="Nck")%>%
  dplyr::select(station,name,datetime,NO2=Wert)
head(Nck_NO2)
Nck_NO<-rd_xlsx_lubw(path,"Nck_76361_NO_03_21.xlsx")%>%
  mutate(name="Nck")%>%
  dplyr::select(station,name,datetime,NO=Wert)
head(Nck_NO)
Nck_PM10<-rd_xlsx_lubw(path,"Nck_76361_PM10_20_21.xlsx")%>%
  mutate(name="Nck")%>%
  dplyr::select(station,name,datetime,PM10=Wert)
head(Nck_PM10)
Nck_PM2.5<-rd_xlsx_lubw(path,"Nck_76361_PM2.5_20_21.xlsx")%>%
  mutate(name="Nck")%>%
  dplyr::select(station,name,datetime,PM2.5=Wert)
head(Nck_PM2.5)
Nck<- left_join(Nck_NO2,Nck_NO)
Nck_dat<-Nck%>% 
  left_join(Nck_PM10)%>% 
  left_join(Nck_PM2.5)
#replace old data
BW_list_tbl$Man_Mitte%>% head()
Man_Mitte<- left_join(Man_NO2,Man_Temp)%>% 
            left_join(Man_WG)%>% 
            left_join(Man_WR)%>%
            left_join(Man_NO)%>% 
            left_join(Man_O3)
BW_list_tbl$Man_Mitte<- Man_Mitte
summary(BW_list_tbl)
summary(Man_Mitte)# measurements from 2000-01-01 to 2013-12-30
BW_list_tbl$Stg_Nck<-Nck_dat
BW_list_tbl$Nck<- Nck
save(BW_list_tbl,file= paste0(url_Rdat,"BW_list_tbl.RData"))
BW.all_data$Man_Mitte <- Man_Mitte
save(BW.all_data, file = file.path(url_Rdat,"BW.RData"))
# station Rt Pomologie
#====================
BW.all_data$Rt %>% summary()
Rt_NO2<- rd_xlsx_lubw(path,"Rt_4470_NO2_00_21.xlsx")%>%
  mutate(name="Rt_pomol")%>%
  dplyr::select(station,name,datetime,NO2=Wert)
Rt_NO<- rd_xlsx_lubw(path,"Rt_4470_NO_00_21.xlsx")%>%
  mutate(name="Rt_pomol")%>%
  dplyr::select(station,name,datetime,NO=Wert)
summary(Rt_NO)
BW.all_data$Rt$Rt.no2$datetime%>% range()# 00 bis 2021
BW.all_data$Rt$Rt.wr%>%summary()
BW.all_data$Rt$Rt.wr%>%head()
#RT_CO=======
Rt_CO<-read_xlsx(file.path(path,"Rt_4470_CO_00_15.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         CO=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,CO)
dim(Rt_CO)#[1] 280511      3
range(Rt_CO$datetime)
start<-ymd_h("2000-01-01 00",tz = "UTC") #RT_CO[1,2]%>%as.POSIXct(origin= "1970-01-01")

Rt_CO_indx<-seq(from= start, by = "30 min",length.out= NROW(Rt_CO))
range(Rt_CO_indx)
Rt_CO$datetime<-RT_CO_indx
summary(Rt_CO)
head(Rt_CO)
BW.all_data$Rt$Rt.co<-Rt_CO
#RT_Glob========
Rt_Glbl<- read_xlsx(file.path(path,"RT_4470_Glbl_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         Glbl=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,Glbl)
head(Rt_Glbl)
BW.all_data$Rt$Rt.glob<-Rt_Glbl
#RT_WG==============
Rt_WG<-read_xlsx(file.path(path,"Rt_4470_WG_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         WG=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,WG)
summary(Rt_WG)
BW.all_data$Rt$Rt.wg<-Rt_WG
#'RT_NO'========
BW.all_data$Rt$Rt.no%>% head(2)
#update
Rt_NO<- read_xlsx(file.path(path,"RT_4470_NO_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         NO=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,NO)
summary(Rt_NO)
BW.all_data$Rt$Rt.no<-Rt_NO
#RT_NO2==========
BW.all_data$Rt$Rt.no2%>% head(3)
Rt_NO2<- read_xlsx(file.path(path,"RT_4470_NO2_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         NO2=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,NO2)
summary(Rt_NO2)
BW.all_data$Rt$Rt.no2<-Rt_NO2
#RT_SO2=====
BW.all_data$Rt$Rt.so2%>% head()
Rt_SO2<- read_xlsx(file.path(path,"RT_4470_SO2_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         SO2=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,SO2)
summary(Rt_SO2)
BW.all_data$Rt$Rt.so2<-Rt_SO2
#RT_Temp========
BW.all_data$Rt$Rt.temp%>% head()
#update Temp
Rt_Temp<- read_xlsx(file.path(path,"RT_4470_Temp_03_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         Temp=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,Temp)
head(Rt_Temp)
summary(Rt_Temp)
BW.all_data$Rt$Rt.temp<-Rt_Temp
#RT_WR=======
BW.all_data$Rt$Rt.no%>% head()

Rt_WR<- read_xlsx(file.path(path,"RT_4470_WR_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         WR=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,WR)
head(Rt_WR)
summary(Rt_WR)
BW.all_data$Rt$Rt.wr<-Rt_WR

#update
BW.all_data$Rt$Rt.no<-Rt_NO
BW.all_data$Rt$Rt.temp<-Rt_Temp
Rt_O3<-read_xlsx(file.path(path,"RT_4470_O3_00_21.xlsx"),skip=10)%>%
  dplyr::select(c(1,2,4,5))%>% 
  mutate(station=as_factor(Stationsnummer),
         name=as_factor("Rt_pomol"),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         O3=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,O3)
summary(Rt_O3)
BW.all_data$Rt$Rt.o3<-Rt_O3
save(BW.all_data,file= file.path(path,"BW.RData"))
Rt_data<- left_join(Rt_NO2,Rt_NO)%>%
   left_join(Rt_O3)%>%
  left_join(Rt_Temp)%>%
  left_join(Rt_SO2)%>% 
  left_join(Rt_CO)%>% 
  left_join(Rt_WG)%>%
  left_join(Rt_WR)%>%
  left_join(Rt_Glbl)
BW_list_tbl$Rt_pomol<-Rt_data
save(BW_list_tbl,file= file.path(url_Rdat,"BW_list_tbl.RData"))
# CAN ======
BW.all_data$Stg.Can$Stg.Can.o3%>%head()

BW.all_data$Stg.Can$Stg.Can.o3<-Can.O3
BW.all_data$Stg.Can$Stg.Can.no<-Can.NO
save(BW.all_data,file = file.path(path,"BW.RData"))

# save Rt Lederstraße as separate list
Rtl.no2<-BW.all_data$Rt$Rtl.no2
Rtl.no<-BW.all_data$Rt$Rtl.no
BW.all_data$Rt$Rtl.no2<-NULL
BW.all_data$Rt$Rtl.no<-NULL
load(file=file.path(path,"BW.RData"))
BW.all_data$Rt.l$NO<-BW.all_data$Rt.l$NO%>%mutate(datetime= ymd_hm(datetime),station=as_factor(station))%>%
  filter(datetime>=ymd_hm("2007-03-21 24:00")&
                               datetime<=ymd_hm("2020-12-31 24:00"))
BW.all_data$Rt.l$NO2<-BW.all_data$Rt.l$NO2%>%
  mutate(datetime= ymd_hm(datetime),station=as_factor(station),
         NO2=as.numeric(NO2))%>%
  filter(datetime>=ymd_hm("2007-03-21 24:00")&
           datetime<=ymd_hm("2020-12-31 24:00"))
summary(BW.all_data$Rt.l$NO2)
BW.all_data$Rt.l$NO2%>%head()
save(BW.all_data,file=file.path(path,"BW.RData"))
# Alb ======
knms<-"Alb.WG" 
map_df(BW.all_data$Alb[[knms]],~.x)%>% names(.)
load(file = file.path(path,"BW.RData"))
# Lbg
BW.all_data$Lbg$Lbg.no
load(file= file.path(path,"BW.RData"))
komp_nms<- names(BW.all_data[["Lbg"]])# "Lbg.no2" "Lbg.no"  "Lbg.wg"  "Lbg.wr"  "Lbg.o3"
for (i in 1:length(komp_nms)){
  BW.all_data$Lbg[[i]]<-BW.all_data$Lbg[[i]]%>% filter(!is.na(datetime))
}
head(BW.all_data$Lbg$Lbg.no)
summary(BW.all_data$Lbg[[5]])
save(BW.all_data,file= file.path(path,"BW.RData"))
stat_fact<- function(df){
  df<-df%>% mutate(station =as_factor(station))
}
for( i in seq_along(komp_nms)){
  BW.all_data[["Lbg"]][[i]]<-BW.all_data[["Lbg"]][[i]]%>% mutate(station= as_factor(station))
}
# Sws==========
names(BW.all_data$Sws)#"Sws.O3" "Sws.NO""Sws.NO2""Sws.WG""Sws.Temp"
summary(BW.all_data$Sws)
head(BW.all_data$Sws[["Sws.Temp"]],4)
NROW(BW.all_data$Sws[["Sws.Temp"]])#368064
BW.all_data$Sws[["Sws.Temp"]]%>% filter(!is.na(datetime))%>% NROW()
Sws.temp.indx<-seq(from=ymd_hm("2000-01-04 00:30"),to= ymd_hm("2021-01-01 00:00"),by = "30 min")
length(Sws.temp.indx)

# needs correction irregular timeseries
Sws_Temp<-left_join(tibble(datetime=Sws.temp.indx),BW.all_data$Sws[["Sws.Temp"]])
NROW(Sws_Temp)
Sws_Temp<-Sws_Temp%>% mutate(Temp=na.locf(Temp),
                             station=na.locf(station),
                             name =na.locf(name))
BW.all_data$Sws[["Sws.Temp"]]<-Sws_Temp
summary(BW.all_data$Sws[["Sws.Temp"]])
save(BW.all_data,file= file.path(path,"BW.RData"))
#===============
names(BW.all_data$Sws)#[1] "Sws.O3""Sws.NO""Sws.NO2""Sws.WG""Sws.Temp"
komp_nms<-names(BW.all_data$Sws)%>%str_replace("Sws.","")
# Import SWS data from udo sever
X.Wert<-function(xlsx_tbl) {
  thispath<- file.path(path,xlsx_tbl)
  read_xlsx(thispath,skip=10)%>%
  mutate(station=as_factor(Stationsnummer),
         name=as_factor(Messstelle),
         datetime= ymd_hm(`Datum / Uhrzeit`),
         Wert=as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,Wert,Komponente)
}
# SWS O3
Sws_O3<-BW.all_data$Sws$Sws.O3
BW.all_data$Sws$Sws.O3 %>% head()
Sws_O3<-X.Wert("SWS_4467_O3_00_20.xlsx")%>% 
  dplyr::select(station,datetime,O3=Wert)
head(Sws_O3)
tail(Sws_O3)
head(BW.all_data$Sws$Sws.O3)
tail(BW.all_data$Sws$Sws.O3)
Sws.O3.indx<-seq(from=ymd_h("2000-01-01 01"),to= ymd_h("2020-12-31 20"),by = "1 hours")
length(Sws.O3.indx)
head(Sws.O3.indx)
Diff<-setdiff(Sws_O3$datetime,Sws.O3.indx)%>%
  as.POSIXct(origin="1970-01-01")%>%
  as_vector()
BW.all_data$Sws$Sws.O3%>% filter(datetime==ymd_hm("2021-01-01 00:00") )
NROW(BW.all_data$Sws$Sws.O3)#184100
Sws_O3<-Sws_O3%>% group_by(datetime)%>%
  summarise(O3 = mean(O3,na.rm=TRUE))%>%
  mutate(O3 = na.locf(O3))
left_join(tibble(Sws.O3.indx,Sws_O3),by="datetime")
NROW(Sws_O3)#[1] 184100

BW.all_data$Sws$Sws.O3<-Sws_O3
BW.all_data$Sws$Sws.O3<-BW.all_data$Sws$Sws.O3%>% mutate(datetime=Sws.O3.indx)

save(BW.all_data, file = file.path(path,"BW.RData"))
summary(BW.all_data$Sws$Sws.O3)
head(BW.all_data$Sws$Sws.O3)

# SWS NO2
Sws_NO2<-X.Wert("SWS_4467_NO2_00_20.xlsx")%>% 
  dplyr::select(station,datetime,NO2=Wert)%>%
  mutate(NO2=na.locf(NO2))%>%
  filter(datetime<=ymd_hms("2020-12-31 23:00:00"))
head(Sws_NO2)
NROW(Sws_NO2)#184101
BW.all_data$Sws$Sws.NO2<-Sws_NO2
#Sws NO
Sws_NO<- X.Wert("SWS_4467_NO_00_20.xlsx")%>% 
  dplyr::select(station,datetime,NO=Wert)
head(Sws_NO)
NROW(Sws_NO)#184100
# CO "SWS_4467_CO_00_20.xlsx"
Sws_CO<-X.Wert("SWS_4467_CO_00_20.xlsx")%>%
  dplyr::select(station,datetime,CO=Wert)%>%
  mutate(CO=na.locf(CO))%>%
  filter(datetime<=ymd_hms("2020-12-31 23:00:00"))%>%
  group_by(datetime)%>%
  summarise(CO = mean(CO,na.rm=TRUE))%>% 
  mutate(station=as_factor("4467"),name =as_factor("Sws"),CO=na.locf(CO))%>% 
  dplyr::select(station,name,datetime,CO)
tail(Sws_CO)
head(Sws_CO)# "2016-01-01"
NROW(Sws_CO)
BW.all_data$Sws$Sws.CO<-Sws_CO
# "SWS_4467_Temp_00_20.xlsx" 
Sws_Temp<- read_excel("~/Documents/Luftqualitaet/Daten/BW/SWS_4467_Temp_00_20.xlsx",
                      skip = 10)%>% dplyr::select(1:5)
names(Sws_Temp)<- c("station","name","Komponente","datetime","Temp")
Sws_Temp%>% filter(!is.na(Temp))%>% head() # 2000-01-03 14:30
Sws_Temp%>% filter(!is.na(Temp))%>% tail() # 2020-12-31 23:00
Sws_Temp<-Sws_Temp%>% filter(datetime>= ymd_hm("2000-01-03 14:30")&datetime<=ymd_hm("2020-12-31 23:00"))
NROW(Sws_Temp)
Sws_Temp<-Sws_Temp%>% mutate(station= as_factor(station),
                   name=as_factor(name),
                   datetime=ymd_hm(datetime),
                   Temp= str_replace(Temp,",",".")%>%as.numeric())
Sws_Temp<-Sws_Temp%>% mutate(name=as_factor("Sws"))%>%dplyr::select(-Komponente)
head(Sws_Temp)
Sws_Temp<-Sws_Temp%>% mutate(Temp=na.locf(Temp))
summary(Sws_Temp)
BW.all_data$Sws$Sws.Temp<-Sws_Temp
head(BW.all_data$Sws$Sws.Temp)
save(BW.all_data,file=file.path(path,"BW.RData"))
# Sws Glob
Sws_Glob<-X.Wert("SWS_4467_Glob_00_20.xlsx")%>% 
  dplyr::select(station,datetime,Glob=Wert)
# Nied
Sws_Nied<-X.Wert("SWS_4467_Nied_00_20.xlsx")%>% 
  dplyr::select(station,datetime,Nied=Wert)
Sws_Nied%>%head()
BW.all_data$Sws$Sws.Nied<-Sws_Nied
# WR
read_xlsx(file=file.path(path,"SWS_4467_WR_00_20.xlsx"),skip =10)%>%names()
Sws_WR<-read_xlsx(file.path(path,"SWS_4467_WR_00_20.xlsx"),skip =10)%>% 
  dplyr::select(station="Stationsnummer",
                name= "Messstelle",
                datetime="Datum / Uhrzeit",
                WR="Wert")
Sws_WR%>%head()
BW.all_data$Sws$Sws.WR<-Sws_WR%>% mutate(name="Sws")
# SO2
Sws_SO2<-X.Wert("SWS_4467_SO2_00_20.xlsx")%>% 
  dplyr::select(station,datetime,SO2=Wert)
summary(Sws_SO2)
head(Sws_SO2)
BW.all_data$Sws$Sws.SO2<-Sws_SO2
# Odw
BW.all_data$Odw%>%summary()
BW.all_data$Odw[[6]]<-BW.all_data$Odw[[6]]%>%filter(datetime<ymd_h("2011-01-01 01"))%>% tail()
Odw_WG<-read_xlsx(file.path(path,"Odw_76118_WG_00_20.xlsx"),skip=10)%>%
  dplyr::select(station="Stationsnummer",
                name= "Messstelle",
                datetime="Datum / Uhrzeit",
                WG="Wert")%>%
  mutate(station=as_factor(station),
         name=as_factor("Odw"),
         datetime=ymd_hm(datetime),
        WG=as.numeric(str_replace(WG,",","."))
        )
# Stuttgart Stadtgarten
Stadtgarten_NO2<-rd_xlsx_lubw(path = "~/Documents/Luftqualitaet/Daten/BW/","Stadtg_9999137_NO2_16_18.xlsx" ) 
Stadtgarten_NO2<-Stadtgarten_NO2%>% 
  mutate(name= as_factor("Stg_Stadtg"))%>% 
  filter(datetime> ymd_h("2016-03-01 14"))%>% 
  dplyr::select(station,name,datetime,NO2=Wert)
summary(Stadtgarten_NO2)
Stadtgarten_NO2<-Stadtgarten_NO2%>% left_join(BW_list_tbl$Stg_Stadtg)
BW_list_tbl$Stg_Stadtg<-Stadtgarten_NO2
save(BW_list_tbl,file =file.path(url_Rdat,"BW_list_tbl.RData") )
# save all updates
save(BW.all_data,file= file.path(path,"BW.RData"))
BW.all_data$Odw$Odw.no2%>% .[[2]]%>% last()
# BW List tbl update
BW_list_tbl$Stg_SZ_afu%>% summary()
BW_list_tbl$Stg_Schwz%>% summary()
