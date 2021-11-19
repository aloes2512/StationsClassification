library(tidyverse)
library(lubridate)
library(modelr)
library(xts)
# Data path
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
list.files(dat_path)
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))

#===========================
# Can as example
Can_Temp<- BW_list_tbl$Stg_Can
Can_Temp%>% head()
# group by hour
Can_Temp<-Can_Temp%>% mutate(datehour = format(datetime,"%H" ),Temp= na.locf(Temp),NO2= na.locf(NO2))
mean(Can_Temp$Temp,na.rm = TRUE)# 11.29542
# Heizdaten aus Gradzahlen
HeizDaten_Can <-Can_Temp %>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Can_Temp) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Can_Temp),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = 20-Temp_mittel_Hzg)
HeizDaten_Can #Gesamt_stunden Hz_stunden Anteil_Hzg Temp_mittel_Hzg Gradzahl_Hzg
              # 368063            248525      0.675      6.93         13.1
# 
Can_Temp<- Can_Temp %>% 
      mutate(year = floor_date(datetime,unit = "1 year"),
                    year = as.character(year)%>%str_extract("^.{4}"),
                    year=as_factor(year))
Can_Temp_yr <-Can_Temp%>% group_by(year) %>% summarise (station,datetime = first(datetime), Jahresmittel= mean(Temp,na.rm= TRUE))
Can_Temp_yr %>% 
  subset(datetime <= ymd("2020-12-31"))%>%
  ggplot(aes(x = year, y=15- Jahresmittel))+
        geom_point(col = "red")+
        geom_smooth(data=Can_Temp_yr,
             method = "auto",mapping= aes(x =as.numeric(year) , y=(15- Jahresmittel)),
             col = "red", formula = y~ s(x, k = 5) )+
        ggtitle("Heizwärmebedarf Bad Cannstatt",
          subtitle = "Jahresmittelwerte 
  berechnet aus Gradzahlen")+
  labs(y= "Gradzahl( Jahr)")
#  Alternative Berechnung aus 1-h Werten
Can_Heizg<- Can_Temp%>% mutate(Grdz = ifelse(Temp <15,20-Temp,0),Hzstd = ifelse(Grdz>0,TRUE,FALSE))
Can_Heizg_00_20<- Can_Heizg%>% subset(datetime < ymd("2020-12-31"))

Can_Heizg_00_20%>% summary()
PrzHzg <- Can_Heizg_00_20 %>%
  summarise (Hzzeit = sum(Grdz>0,na.rm = TRUE),Hzg_mittel = mean(Grdz, na.rm=TRUE),
             Ges_zeit = NROW(.)/2,prozHzstd = Ges_zeit/Hzzeit*100)
