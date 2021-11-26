# Kopie von 01_NO2_Heizung.R
# NO2_Heizungseffekt
library(tidyverse)
library(lubridate)
library(modelr)
library(xts)
dat_path = "~/Documents/Luftqualitaet/Daten/BW_Rdat"
list.files(dat_path)
# load data
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))
# NO2
BW_list_tbl$Stg_Can%>% tail(20)

Can_NO2 <- BW_list_tbl$Stg_Can %>% mutate(NO2 = na.locf(NO2))
mean(Can_NO2$NO2,na.rm = TRUE) # 31.72703
#NO
Can_NO <-BW_list_tbl$Stg_Can %>%
  mutate(NO = na.locf(NO))
mean(Can_NO$NO,na.rm = TRUE) #16.08416
# Heizungsbedarf  als Gradzahl
#Temp
Can_Temp <- BW_list_tbl$Stg_Can %>%
  mutate(Temp = na.locf(Temp))
Can_Temp<-Can_Temp%>% mutate(datehour = format(datetime," %Y-%m-%d %H" )%>% ymd_h())%>%
  group_by(datehour)%>%
  summarise(station=as_factor(first(station)),Temp= mean(Temp,na.rm=TRUE))
mean(Can_Temp$Temp,na.rm = TRUE) # 11.30647
Can_Heizg<- Can_Temp%>%
  mutate(Grdz = ifelse(Temp <15,20-Temp,0),Hzstd = ifelse(Grdz>0,TRUE,FALSE))%>%
  rename("datetime"=datehour)
summary(Can_Heizg)
# Kennziffern Heizg
HeizDaten_Can <-Can_Heizg %>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Can_Heizg) ,
             Hz_stunden = n(),
             Anteil_Hzg = Hz_stunden / Gesamt_stunden,
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = 20-Temp_mittel_Hzg,
             WW_bedarf = Gradzahl_Hzg*0.2)
saveRDS(HeizDaten_Can, file = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat/HeizDatenCan.rds")

# gesamter Zeitraum 2000 bis 2020
Can_data <- left_join(Can_NO2,Can_NO) %>% left_join(Can_Heizg)
# Auswahl Zeitintervall"2015-01-01" bis "2020-12-31"
Can_data_15_20<- Can_data%>% subset(datetime>=ymd("2015-01-01")&datetime <= ymd("2020-12-31"))
PrzHzg <- Can_data %>%
  summarise (Hzstd = sum(Grdz>0,na.rm = TRUE),Hzg_mittel = mean(Grdz, na.rm=TRUE),
             Ges_Std = NROW(.),prozHzstd = Hzstd/Ges_Std*100)
PrzHzg
WW_bedarf <- 0.2*PrzHzg$prozHzstd*PrzHzg$Hzg_mittel/100 # 1.17 Grad
Can_dat_Heizg <-Can_data %>% mutate(Grdz_gesamt = ifelse(Temp <15,20-Temp+WW_bedarf,WW_bedarf))
Can_dat_Heizg<-Can_dat_Heizg%>%
  mutate(NO = na.locf(NO),Grdz=na.locf(Grdz),Grdz_gesamt=na.locf(Grdz_gesamt),Temp = na.locf(Temp))
Can_dat_Heizg %>% names() #"station"  "datetime" "NO2"      "Temp"  "NO"   "Grdz" "Grdz_gesamt"
summary(Can_dat_Heizg)
Can_dat_Heizg_15_20 <- Can_dat_Heizg %>% subset(datetime>=ymd("2015-01-01")&datetime <= ymd("2020-12-31"))
summary(Can_dat_Heizg_15_20)

#===========================
# Can_NO2_model für Zeitraum 15 bis 20

# #Für Plot verkürztes Zeitintervall () Reduzierung der Glättungseffekte am Anfang und Ende)
x_min <- as.POSIXct("2015-07-01")
x_max <- as.POSIXct("2020-06-30")
Can_NO2_15_20_model <- lm (NO2 ~ datetime +1, data = Can_dat_Heizg_15_20)
Can_Heizg_15_20_model <- lm(Grdz_gesamt ~ datetime +1,data = Can_dat_Heizg_15_20 )
Can_NO_15_20_model <- lm(NO ~ datetime +1, data = Can_dat_Heizg_15_20)
# NO2 Model
Can_dat_Heizg_15_20 <- Can_dat_Heizg_15_20 %>%
  add_predictions(Can_NO2_15_20_model)%>%
  add_residuals(Can_NO2_15_20_model)
#Uebersicht 2015 bis 2020
Can_dat_Heizg_15_20 %>% ggplot(aes(x=datetime))+
  geom_smooth(method = "gam", formula= y ~ s(x,bs = "cs"),mapping =aes( y = NO2),col = "red", linetype = 2)+
  geom_smooth(mapping = aes( y = NO2),method = "lm",col = "red", linetype = 1)+
  geom_smooth(method = "gam", formula= y ~ s(x,bs = "cs"),col = "blue", linetype =2,mapping = aes(  y =Grdz_gesamt))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( y =Grdz_gesamt))+
names(Can_dat_Heizg_15_20) <-names(Can_dat_Heizg_15_20) %>%
  str_replace("pred", "prd.no2") %>% str_replace("resid","rsd.no2")
# Heizg Model
Can_dat_Heizg_15_20 <- Can_dat_Heizg_15_20 %>%
  add_predictions(Can_Heizg_15_20_model) %>%
  add_residuals(Can_Heizg_15_20_model)
names(Can_dat_Heizg_15_20) <- names(Can_dat_Heizg_15_20) %>%
  str_replace("pred", "prd.heizg") %>% str_replace("resid","rsd.heizg")
# NO Model
Can_dat_Heizg_15_20 <- Can_dat_Heizg_15_20 %>%
  add_predictions(Can_NO_15_20_model) %>%
  add_residuals(Can_NO_15_20_model)
names(Can_dat_Heizg_15_20) <-names(Can_dat_Heizg_15_20) %>%
  str_replace("pred", "prd.no") %>% str_replace("resid","rsd.no")
  geom_smooth(method = "gam", formula= y ~ s(x,bs = "cs"),col = "green", linetype =2,mapping = aes(  y =NO))+
  geom_smooth(method = "lm",col = "green", linetype =1, aes( y =NO))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2/ NO -Immissionen(rot/ grün) & Heizbedarf(blau)
          Mittelwerte und linearer Trend
          Bad Cannstatt, Gnesenerstr")+
  labs( x="", y ="NO2, NO [μg/m3] / Gradzahl")
#Übersicht 2015 bis 2020 20 Basisfunktionen
Can_dat_Heizg_15_20 %>% ggplot(aes(x=datetime))+
  geom_smooth(method = "gam", formula= y ~ s(x,k=20,bs = "cs"),mapping =aes( y = NO2),col = "red", linetype = 2)+
  geom_smooth(mapping = aes( y = NO2),method = "lm",col = "red", linetype = 1)+
  geom_smooth(method = "gam", formula= y ~ s(x,k=20,bs = "cs"),col = "blue", linetype =2,mapping = aes(  y =Grdz_gesamt))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( y =Grdz_gesamt))+
  geom_smooth(method = "gam", formula= y ~ s(x,k=20,bs = "cs"),col = "green", linetype =2,mapping = aes(  y =NO))+
  geom_smooth(method = "lm",col = "green", linetype =1, aes( y =NO))+
  ggtitle("NO2/ NO -Immissionen(rot/ grün) & Heizbedarf(blau)
          Mittelwerte und linearer Trend
          Bad Cannstatt, Gnesenerstr")+
  labs( x="", y ="NO2,NO[μg/m3] / Gradzahl")
ggsave("Hzg_NO2_NO_Can_15_20.png",device="png",path="~/projects/StationsClassification/reports/figs")
# QQ Plot Grdz/ NO2

Can_dat_Heizg_15_20%>%  ggplot(aes(x=Grdz,y=NO2))+
  geom_point(size = 0.01)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO2-Immissionen ~ Heizwaermebedarf
          Bad Cannstatt (Heiztage 2015 bis 2020)")+
  labs( x = "Heizwärmebedarf", y = " NO2 [μg/m3]")
lm(NO2 ~ Grdz,data =Can_dat_Heizg_15_20 %>% subset(Grdz >0 ))#Coefficients:
#(Intercept)         Grdz
#20.3124       0.9017
# QQ Plot Grdz/ NO
Can_dat_Heizg_15_20%>%   ggplot(aes(Grdz,NO))+
  geom_point(size = 0.01)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO-Immissionen ~ Heizwaermebedarf
  Bad Cannstatt (Heizstunden 2015 bis 2019)")+
  labs( x = "Heizwärmebedarf", y = " NO [μg/m3]")
lm(NO ~ Grdz,data =subset(Can_dat_Heizg_15_20, (Grdz >0 )))#Coefficients:
#(Intercept)         Grdz
#-1.535        1.480
Can_dat_Heizg_15_20 %>% names()# "station",datetime", "NO2", "NO","Grdz","Temp" ,"Grdz_gesamt","prd.no2"
# "rsd.no2"  "prd.heizg"  "rsd.heizg" "prd.no"      "rsd.no"
Can_dat_Heizg_15_20 %>% ggplot(aes(x=datetime))+
  geom_smooth(aes( y = rsd.no2),col = "red", linetype = 2)+
  geom_smooth(mapping= aes( y = rsd.no2),method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime,
                                              y =rsd.heizg))+
  geom_smooth(method = "lm",col = "blue", linetype =1,
              aes( x= datetime, y =rsd.heizg))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
          Abweichungen vom Trend
       Bad Cannstatt, Gnesenerstr")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
# Correlationen
res.no2 <-Can_dat_Heizg_15_20$rsd.no2 %>% na.locf()
res.no <-Can_dat_Heizg_15_20$rsd.no %>% na.locf()
res.heizg <- Can_dat_Heizg_15_20$rsd.heizg%>% na.locf()
length(res.no)
length(res.heizg)
is.na(res.heizg) %>% sum()# 0

cov(res.no2,res.heizg)
cov(res.no,res.heizg)
Can_res_dat <- tibble(no2 = res.no2,
                      no= res.no,
                      grdz = res.heizg)
summary(Can_res_dat%>% filter(grdz >0))
Can_res_dat <- Can_res_dat %>% mutate (distance = (no2-grdz)^2)
Can_res_dat %>% summarise (Abstand = sqrt(sum(distance,na.rm=T)/NROW(.)))
#Can gam Model
Can_NO2_model_gam <- mgcv::gam(NO2 ~ datetime +1, data = Can_dat_Heizg_15_20,
                               span = 0.1,model = T,degree=1)
Can_Temp_model_gam <- mgcv::gam(Grdz ~ datetime +1,data = Can_dat_Heizg_15_20 )
