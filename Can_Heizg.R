# Can_Temp calculated by 01_NO2_Heizung.R"
#===========================
Can_Temp%>% head()
# group by hour
Can_Temp<-Can_Temp%>% mutate(datehour = format(datetime," %Y-%m-%d %H" )%>% ymd_h())%>% 
  group_by(datehour)%>%
  summarise(station=as_factor(first(station)),Temp= mean(Temp,na.rm=TRUE))
mean(Can_Temp$Temp)# 11.30647
# Heizdaten aus Gradzahlen
HeizDaten_Can <-Can_Temp %>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Can_Temp) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Can_Temp),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = 20-Temp_mittel_Hzg)
HeizDaten_Can #Gesamt_stunden Hz_stunden Anteil_Hzg Temp_mittel_Hzg Gradzahl_Hzg
              # 189722          125873      0.663        6.69         13.3
Can_Temp<- Can_Temp %>% 
  subset()
  mutate(year = floor_date(datehour,unit = "1 year"),
                    year = as.character(year)%>%str_extract("^.{4}"),
                    year=as_factor(year))
Can_Temp<-Can_Temp%>% group_by(year) %>% summarise (station, Jahresmittel= mean(Temp,na.rm= TRUE))
Can_Temp %>% ggplot(aes(x = year, y=15- Jahresmittel))+
  geom_point()+
  #geom_smooth(method = "auto",mapping= aes(x = year, y=(15- Jahresmittel), col = "blue") formula = (15-Jahresmittel)~ s(year, k = 5) )+
  ggtitle("Heizwärmebedarf Bad Cannstatt",
          subtitle = "Jahresmittelwerte 
  berechnet aus Gradzahlen")+
  labs(y= "Gradzahl")
#  Altenative Berechnung
Can_Heizg<- Can_Temp%>% mutate(Grdz = ifelse(Temp <15,20-Temp,0),Hzstd = ifelse(Grdz>0,TRUE,FALSE))
Can_Heizg%>% summary()
PrzHzg <- Can_Heizg %>%
  summarise (Hzzeit = sum(Grdz>0,na.rm = TRUE),Hzg_mittel = mean(Grdz, na.rm=TRUE),
             Ges_zeit = NROW(.)/2,prozHzstd = Ges_zeit/Hzzeit*100)
Can_Heizg_00_20<- Can_Heizg%>% subset(datetime < ymd("2020-12-31"))
