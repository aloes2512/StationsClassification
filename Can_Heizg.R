library(tidyverse)
library(lubridate)
library(modelr)# add_predictions
library(xts)
library(mgcv)
library(mgcViz)
library(broom)
library(knitr)
# Data path
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
save.figs <- paste0("~/projects/StationsClassification","/reports/figs")
list.files(dat_path)
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))
summary(BW_list_tbl$Stg_Can)
#===========================

# Can as example
names(BW_list_tbl)# 22 stations

Can_dat<-BW_list_tbl[[13]]%>% subset(datetime< ymd("2021-01-01"))

# Heizdaten aus Gradzahlen
HeizDaten_Can <-Can_dat%>% filter (Temp <= 15) %>%
  summarise (Gesamt_stunden =NROW(Can_dat) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Can_dat),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = (20-Temp_mittel_Hzg)*Anteil_Hzg,
             WW = Gradzahl_Hzg*Hz_stunden*0.2/Gesamt_stunden)# Warm Wasser Energ-Bedarf 20% Heizenergie
HeizDaten_Can # Can_dat : 21 years 7671 days
              #Gesamt_stunden Hz_stunden Anteil_Hzg Temp_mittel_Hzg Gradzahl_Hzg WW 
              # 184103          121404       0.659      6.73         8.75        1.16

Heiz_kenndat<- function(df) {
                   x<- df%>%  subset(Temp != is.na(Temp))%>%
                          group_by(name)%>% 
                               summarise(
                                op.hrs =NROW(.),
                                Temp.ht.mean=mean(Temp,na.rm= TRUE),
                                grdz.ht = mean(ifelse(Temp<= 15,20-Temp,0),na.rm= TRUE),
                                ht.hrs = sum(ifelse (Temp<= 15, TRUE,FALSE )))
}

(Heiz_kenndat(Can_dat))
# Yearly averages
Can_dat<- Can_dat %>% 
          mutate(Yr = as.integer(format(datetime,format= "%Y")),Yr = as_factor(Yr),
                 Hzg = ifelse(15-Temp>=0,TRUE,FALSE),
                Grdz_Hzg = ifelse(Hzg,20-Temp,0))
 
Can_dat_Yr<-  Can_dat %>%  dplyr::select(-datetime)%>%
  group_by(Yr) %>% summarise (station=first(name),
                              T_Jahr_mittel= mean(Temp,na.rm= TRUE),
                              NO2_Jahr_mittel= mean(NO2,na.rm=TRUE),
                              Grdz_Jahr_mittel = mean(Grdz_Hzg,na.rm = TRUE)*1.2,
                              Hz_std= sum(Hzg,na.rm = TRUE),
                              Hz_proz= Hz_std/n())
Can_dat_Yr%>% summary()

Can_dat_Yr %>% 
  ggplot(aes(x = Yr, y=Grdz_Jahr_mittel))+
        geom_point(col = "blue")+
        geom_smooth(data=Can_dat_Yr,
             method = "gam",mapping= aes(x =as.numeric(Yr) , y=Grdz_Jahr_mittel),
             col = "blue", formula = y~ s(x, k = 20) )+
  geom_smooth(data=Can_dat_Yr,
              method = "lm",mapping= aes(x =as.numeric(Yr) , y=Grdz_Jahr_mittel),
              col = "purple", formula = y~ x ,linetype = 2)+
        ggtitle("Heating-Energy Bad Cannstatt",
          subtitle = "Year average Heating + 
   20% Water-Heating")+
  labs(y= "~ Grdz(Jahr)", x = "")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#  Alternative Berechnung aus 1-h Werten
Can_dat%>% head()
summary(Can_dat)
Can_Heizg_00_20<- Can_dat%>% na.omit()
Can_Heizg_00_20%>%summary()
Gesstd<- NROW(Can_Heizg_00_20)#170846

PrzHzg <- Can_Heizg_00_20 %>%         #Hzzeit Hzg_mittel Ges_zeit prozHzstd
                                      #112844       8.74   170846      66.1
  summarise (Hzzeit = sum(Grdz_Hzg>0,na.rm = TRUE),
             Hzg_mittel = mean(Grdz_Hzg, na.rm=TRUE),
             Ges_zeit = NROW(.),
             prozHzstd = Hzzeit/Ges_zeit*100)
# pauschale Berücksichtigung WW Erwärmung ~ 20% Heizwärme
WW_bedarf <- PrzHzg["Hzg_mittel"]*PrzHzg["Hzzeit"] * 0.2/Gesstd # 1.17 Grad
Can_dat_Heizg <-Can_dat %>% mutate (Grdz= ifelse(Temp <=15,20-Temp,0),Grdz_gesamt = Grdz+as.numeric(WW_bedarf))
Can_dat_Heizg<-Can_dat_Heizg %>% na.omit()  
summary(Can_dat_Heizg)
Can_dat_Heizg %>% ggplot(aes(x = datetime))+
  geom_smooth(method="gam",mapping = aes(y = Grdz_gesamt),col = "blue", formula = y ~ s(x, k= 21),data=Can_dat_Heizg)+
  geom_smooth(method = "lm", mapping = aes(x =datetime,y= Grdz_gesamt),col = "purple", linetype =2, data=Can_dat_Heizg)+
  ggtitle("Energy-demand Bad Cannstatt",
          subtitle = "Room-Heating + Water-Heating
  smoothing with 21 basis functions")+
  labs(x ="", y = " ~ Gradzahl")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Select smaller time interval
Can_dat_Heizg_15_20 <- Can_dat_Heizg %>% subset(datetime>=ymd("2015-01-01")&datetime <= ymd("2020-12-31"))
summary(Can_dat_Heizg_15_20)
Can_grdz.yr<-Can_dat_Heizg_15_20%>% group_by(Yr)%>% 
  summarise(Grdz_yr= mean(Grdz_gesamt))
Can_grdz.yr$Jahr<-seq(from= ymd("2015-07-01"),to = ymd("2020-07-01"),by = "1 year")%>% as.POSIXct()
Can_dat_Heizg_15_20 %>% ggplot(aes(x = datetime))+
  geom_smooth(method="gam",mapping = aes(y = Grdz_gesamt),col = "blue", formula = y ~ s(x, k= 21),data=Can_dat_Heizg_15_20)+
  geom_smooth(method = "lm", mapping = aes(x =datetime,y= Grdz_gesamt),col = "purple", linetype =2, data=Can_dat_Heizg_15_20)+
  geom_point(mapping= aes(x=Jahr,y= Grdz_yr),data=Can_grdz.yr)+
  ggtitle("Energy-demand Bad Cannstatt",
          subtitle = "Room-Heating + Water-Heating
  smoothing with 21 basis functions")+
  labs(x ="", y = " ~ Gradzahl")
ggsave("Can_energy_15_20.png",path = save.figs)

#===========================
# 
# create tidy tibble 4 atmospheric components
Can_val_00_20<-Can_Heizg_00_20%>%na.omit()%>% pivot_longer(cols = -c(1,2,3,9),names_to = "Comp",values_to = "values")%>%
  mutate(Comp = as_factor(Comp))
Cmps<- levels(Can_val_00_20$Comp)%>% setdiff(c("Temp","WG")) #"NO2"  "O3"   "NO"   "Grdz"
summary(Can_val_00_20)
# components linear trend
dat_path<- "~/documents/Luftqualitaet/Daten/BW_Rdat"
load(file.path(dat_path,"BW.RData"))
summary(BW.all_data)
Can_data<-BW.all_data$Stg.Can$Stg.Can.no2 %>% left_join(BW.all_data$Stg.Can$Stg.Can.no)
Can_data<-Can_data%>% left_join(BW.all_data$Stg.Can$Stg.Can.temp)
Can_data<-Can_data%>%left_join(BW.all_data$Stg.Can$Stg.Can.o3)
Can_data_list<-BW.all_data$Stg.Can
Can_comps<-names(BW.all_data$Stg.Can)
length(Can_comps) #8
Can_tbl<- BW.all_data$Stg.Can$Stg.Can.no2%>% dplyr::select(-NO2)
for (i  in 1:8) {
  df <- BW.all_data$Stg.Can[[i]]
  Can_tbl <- left_join(Can_tbl,df, by=c("station", "datetime"))
}
Can_tbl<-Can_tbl%>% mutate(Hzg = as_factor(ifelse(Temp<= 15,"heating", "no_heating")))
comps <- names(Can_tbl)%>%setdiff(c("station","datetime","Hzg"))
Can_tidy<- Can_tbl%>% pivot_longer(cols = all_of(comps),names_to = "comp",values_to =   "value" )
# Plot
Can_tidy %>% na.omit()%>% filter(comp!= "WR"& comp != "WG"& comp != "CO"& comp != "SO2") %>%
  ggplot(aes(x = datetime))+
  geom_smooth(method = "lm",mapping = aes(y= value,col= comp))+
  facet_grid(.~Hzg)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(" 21-Year-Trend(regression)
NO, NO2, O3, Temperature",
          subtitle = "Heating hours Stgt.-Bad Cannstatt")+
  labs(x= "", y = "Values(μg/m3,m/s,°C)")
ggsave("Trend_NO2.NO.O3.Tem_Can.png",path = save.figs)
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
# Heating effects
Can_dat_Heizg %>% head(2)# from line 21
range(Can_dat_Heizg$Grdz_Hzg)# 0  to 35.1
Can_dat_Heizg<-Can_dat_Heizg%>% na.omit%>%
  mutate(Hzg_lvl=round(3*Grdz_Hzg/max(Grdz_Hzg))%>% as_factor() )
levels(Can_dat_Heizg$Hzg_lvl)<-c("no.ht","low.ht","medium.ht","max.ht")
summary(Can_dat_Heizg)
Hzg_mass <-Can_dat_Heizg %>% group_by(Hzg_lvl)%>%
  summarise(N= n())
Hzg_mass%>% ggplot(aes(x= Hzg_lvl, y= N))+ 
  geom_histogram(stat = "identity", col = 2)+
  ggtitle("Distribution of Heating Hours
Bad Canstatt 21 years (183960 hours)",
          subtitle = " 4 levels: no-, low-, medium-, maximum-heating ")+
  labs(x="",y= "heating hours")
ggsave("Distrib_heatg_h_Can.png",path=save.figs)
Can_dat_Heizg%>% ggplot(aes(x= Grdz_Hzg))+
  geom_histogram(bins =30,col = "grey")
x<- with(Can_dat_Heizg,Grdz_Hzg)
hist(x,col = "grey", main = "Heating-Hours-Distribution
     Bad Cannstatt",freq= T,xlab= " Gradzahl", breaks= seq(0,36,1),right = F)
# Plot additional stations
NO2_3stn_sel<-BW_list_tbl%>% map_df(~.x)%>% 
  subset(name %in% c("Sws","Stuttgart-Bad Cannstatt","Lbg_Friedr"))%>%
  dplyr::select(1:5)%>% subset(datetime< ymd("2021-01-01"))
NO2_3stn_sel<-NO2_3stn_sel%>% mutate(name= as_factor(name))
(Heiz_kenndat(NO2_3stn_sel%>%subset(name== "Stuttgart-Bad Cannstatt")))
# rename for better plotting
levels(NO2_3stn_sel$name)<- list("Lbg_Friedr"="Lbg_Friedr","Bad-Cannstatt"="Stuttgart-Bad Cannstatt","Schw.Sued"="Sws")
NO2_3stn_sel%>% ggplot(aes(x= datetime,col= name))+
  geom_smooth(method ="lm", mapping = aes(y= NO2), data= NO2_3stn_sel)+
  facet_wrap(.~ name,scales= "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle( "NO2-Immission-Trend",
          subtitle= "Schwarzwald Süd, Stg. Bad- Cannstatt, Luwigsburg Friedrichstr.")+
  labs(x= "",y= "NO2 [μg/m3]")
ggsave("NO2_Trend_exampl.png",path=save.figs)
# Heating effect
NO2_3stn_sel%>% head(1)
NO2_3stn_heatg<-NO2_3stn_sel%>% mutate(heatg= ifelse(Temp<=15,NO2,0))%>% na.omit()
NO2_3stn_heatg%>% ggplot(aes(x= datetime,col= name))+
  geom_smooth(method ="lm", mapping = aes(y= NO2), data= NO2_3stn_heatg, linetype = 1)+
  geom_smooth(method ="lm", mapping = aes(y= NO2-heatg), data= NO2_3stn_heatg, linetype = 2)+
  geom_smooth(method ="lm", mapping = aes(y= heatg), data= NO2_3stn_heatg, linetype = 3)+
  facet_wrap(.~ name,scales= "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle( "NO2-Immission-Trend",
           subtitle= "Schwarzwald Süd, Stg. Bad- Cannstatt, Luwigsburg Friedrichstr.")+
  labs(x= "",y= "NO2 [μg/m3]")
