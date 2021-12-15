library(tidyverse)
library(lubridate)
library(modelr)# add_predictions
library(xts)
library(mgcv)
library(mgcViz)
library(broom)
# Data path
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
save.figs <- paste0("~/projects/StationsClassification","/reports/figs")
list.files(dat_path)
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))
summary(BW_list_tbl$Stg_Can)
#===========================
# Can as example
Can_dat<- BW_list_tbl[[13]]
names(BW_list_tbl)
Can_dat%>% head()
Can_dat<-Can_dat%>% subset(datetime< ymd("2021-01-01"))

# group by hour
##Can_dat<-Can_dat%>% mutate(datehour = format(datetime,"%H" ),Temp= na.locf(Temp),NO2= na.locf(NO2))
##mean(Can_dat$Temp,na.rm = TRUE)# 11.29371
# Heizdaten aus Gradzahlen
HeizDaten_Can <-Can_dat%>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Can_dat) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Can_dat),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = (20-Temp_mittel_Hzg)*Anteil_Hzg)
HeizDaten_Can # Can_dat : 21 years 7671 days
              #Gesamt_stunden Hz_stunden Anteil_Hzg Temp_mittel_Hzg Gradzahl_Hzg 21 years
              # 184103          121404       0.659      6.73         8.75
# Warm Wasser Energ-Bedarf 20% Heizenergie
WW<- HeizDaten_Can$Gradzahl_Hzg*HeizDaten_Can$Hz_stunden*0.2/HeizDaten_Can$Gesamt_stunden

Can_dat<- Can_dat %>% 
  mutate(Yr = as.integer(format(datetime,format= "%Y")),Yr = as_factor(Yr))
Can_dat_Yr <-Can_dat%>% subset(datetime< ymd("2021-01-01"))%>%
  group_by(Yr) %>% summarise (station=first(name),
                                                      datetime = first(datetime), 
                                                      T_Jahr_mittel= mean(Temp,na.rm= TRUE),
                                                      NO2_Jahr_mittel= mean(NO2,na.rm=TRUE),
                                                      Hz_energ_bedarf= (20-T_Jahr_mittel)*1.20)
Can_dat_Yr$Hz_energ_bedarf%>% mean()# 10.4
Can_dat_Yr%>% summary()
Can_dat_Yr%>% tail()
Can_dat_Yr %>% 
  subset(datetime <= ymd("2020-12-31"))%>%
  ggplot(aes(x = Yr, y=Hz_energ_bedarf))+
        geom_point(col = "red")+
        geom_smooth(data=Can_dat_Yr,
             method = "gam",mapping= aes(x =as.numeric(Yr) , y=Hz_energ_bedarf),
             col = "red", formula = y~ s(x, k = 20) )+
  geom_smooth(data=Can_dat_Yr,
              method = "lm",mapping= aes(x =as.numeric(Yr) , y=Hz_energ_bedarf),
              col = "purple", formula = y~ x ,linetype = 2)+
        ggtitle("Heating-Energy Bad Cannstatt",
          subtitle = "Year average Heating + 
   20% Water-Heating")+
  labs(y= "~ Grdz(Jahr)", x = "")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#  Alternative Berechnung aus 1-h Werten
Can_Heizg<- Can_dat%>% mutate(Grdz = ifelse(Temp <=15,20-Temp,0),Hzstd = ifelse(Grdz>0,TRUE,FALSE))
Can_Heizg_00_20<- Can_Heizg%>% subset(datetime < ymd("2020-12-31"))
Gesstd<- NROW(Can_Heizg_00_20)
Can_Heizg_00_20%>% na.omit()%>%summary()
PrzHzg <- Can_Heizg_00_20 %>% 
  summarise (Hzzeit = sum(Grdz>0,na.rm = TRUE),Hzg_mittel = mean(Grdz, na.rm=TRUE),
             Ges_zeit = NROW(.),prozHzstd = Hzzeit/Ges_zeit*100)
# pauschale Berücksichtigung WW Erwärmung ~ 20% Heizwärme
WW_bedarf <- PrzHzg["Hzg_mittel"]*PrzHzg["Hzzeit"] * 0.2/Gesstd # 1.17 Grad
Can_dat_Heizg <-Can_dat %>% mutate (Grdz= ifelse(Temp <=15,20-Temp,0),Grdz_gesamt = Grdz+as.numeric(WW_bedarf))
Can_dat_Heizg<-Can_dat_Heizg%>% na.omit()  
Can_dat_Heizg%>%  ggplot(aes(x = datetime))+
  geom_smooth(method="gam",mapping = aes(y = Grdz_gesamt),col = "red", formula = y ~ s(x, k= 20),data=Can_dat_Heizg)+
  geom_smooth(method = "lm", mapping = aes(x =datetime,y= Grdz_gesamt),col = "purple", linetype =2, data=Can_dat_Heizg)+
  ggtitle("House Energy Bad Cannstatt",
          subtitle = "1-h-Heating + WW-Heating
  smoothing with 20 basis functions")+
  labs(x ="", y = "Gradzahl")
# Select smaller time intervall
Can_dat_Heizg_15_20 <- Can_dat_Heizg %>% subset(datetime>=ymd("2015-01-01")&datetime <= ymd("2020-12-31"))
summary(Can_dat_Heizg_15_20)

#===========================
# 
# detrend using residuals
Can_detrend.no2<-Can_dat_Heizg%>%mutate(NO2= na.locf(NO2))%>%
  mutate(lm(NO2 ~ datetime,data=.)%>% 
           augment()%>% 
           dplyr::select(NO2.resd=.resid, NO2.fit= .fitted) )
Can_detrend.no2%>% head(2)
Can_detrend.no2%>% ggplot(aes(x =datetime))+
  geom_smooth(mapping = aes(y=NO2.resd),method= "gam",formula = y ~ s(x , bs= "cs", k=21),col ="red")+
  geom_line(aes(y= NO2.fit),linetype = 3,col ="red")+
  ggtitle("NO2- trend Bad Cannstatt",
          subtitle= "regression line and mean deviation
   smoothing with 21 basis function ")
ggsave("NO2_trend_resd.png",path=save.figs)
# Can_NO2_model für Zeitraum 15 bis 20
Can_NO2_15_20_model <- lm (NO2 ~ datetime +1, data = Can_dat_Heizg_15_20)
Can_Heizg_15_20_model <- lm(Grdz_gesamt ~ datetime +1,data = Can_dat_Heizg_15_20 )
Can_NO_15_20_model <- lm(NO ~ datetime +1, data = Can_dat_Heizg_15_20)
## NO2 linear Model
Can_dat_Heizg_15_20 <- Can_dat_Heizg_15_20 %>%
  add_predictions(Can_NO2_15_20_model)%>%
  add_residuals(Can_NO2_15_20_model)
names(Can_dat_Heizg_15_20) <-names(Can_dat_Heizg_15_20) %>%
  str_replace("pred", "prd.no2") %>% str_replace("resid","rsd.no2")
## Heizg linear Model
Can_dat_Heizg_15_20 <- Can_dat_Heizg_15_20 %>%
  add_predictions(Can_Heizg_15_20_model) %>%
  add_residuals(Can_Heizg_15_20_model)
names(Can_dat_Heizg_15_20) <- names(Can_dat_Heizg_15_20) %>%
  str_replace("pred", "prd.heizg") %>% str_replace("resid","rsd.heizg")
## NO linear Model
Can_dat_Heizg_15_20 <- Can_dat_Heizg_15_20 %>%
  add_predictions(Can_NO_15_20_model) %>%
  add_residuals(Can_NO_15_20_model)
names(Can_dat_Heizg_15_20) <-names(Can_dat_Heizg_15_20) %>%
  str_replace("pred", "prd.no") %>% str_replace("resid","rsd.no")
#Übersicht 2015 bis 2020 20 Basisfunktionen
Can_dat_Heizg_15_20<-Can_dat_Heizg_15_20 %>% na.omit()
Can_dat_Heizg_15_20%>% summary()
Can_dat_Heizg_15_20 %>% 
  ggplot(aes(x= datetime))+
  geom_smooth(method = "gam", formula= y ~ s(x,k=20,bs = "cs"),mapping =aes( y = NO2),col = "red", linetype = 2)+
  geom_smooth(mapping = aes( y = NO2),method = "lm",col = "red", linetype = 1)+
  geom_smooth(method = "gam", formula= y ~ s(x,k=20,bs = "cs"),col = "blue", linetype =2,mapping = aes(  y =Grdz_gesamt))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( y =Grdz_gesamt))+
  geom_smooth(method = "gam", formula= y ~ s(x,k=20,bs = "cs"),col = "green", linetype =2,mapping = aes(  y =NO))+
  geom_smooth(method = "lm",col = "green", linetype =1, aes( y =NO))+
  ggtitle("NO2/NO-Immissions(red/green)&
           Heating-Energy(blue)",
  subtitle = "Mean and linearer Trend Bad Cannstatt 
  smoothing with 20 basis functions")+
  labs( x="", y ="NO2,NO[μg/m3]/Gradzahl")
ggsave("Hzg_NO2_NO_Can_15_20.png",device="png",path="~/projects/StationsClassification/reports/figs")
# dependence on additional covariants
# # Verwendung mgcv::gam
NO2_dat<- Can_dat_Heizg_15_20%>% 
  dplyr::select(NO2,NO,O3,Temp,datetime)
NO2_dat_gam<-gam(NO2~ s(NO)+ s(O3) + s(Temp)+datetime,data = NO2_dat,method = "REML")
NO2_dat_gam<-getViz(NO2_dat_gam,nsim = 5)
summary(NO2_dat_gam)
## plot
# smooth components using the sm function
plot(sm(NO2_dat_gam,1))+
  ggtitle("Bad Cannstatt: mean effect of NO on NO2")+
  l_fitLine(col = "red")+
  l_ciLine(col = "blue")+
  l_points(size = 0.01)+
  labs( x="NO [μg/m3]")
plot(sm(NO2_dat_gam,2))+ggtitle("Bad Cannstatt:  effect of smoothed O3  on NO2")+
  l_fitLine(col = "red")+
l_ciLine(col = "blue")+
  l_points(size = 0.01,alpha= 0.5)
# Plot all Terms
plot(sm(NO2_dat_gam,3))+ ggtitle("Bad Cannstatt:  effect of smoothed Temperature on NO2")+
  l_fitLine(col = "red")+
  l_ciLine(col = "blue")+
  l_points(size = 0.01)

