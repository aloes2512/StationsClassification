library(tidyverse)
library(lubridate)
library(modelr)# add_predictions
library(xts)
library(mgcv)
library(mgcViz)
library(broom)
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
save.figs <- paste0("~/projects/StationsClassification","/reports/figs")
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))
Can_dat<- BW_list_tbl[[13]]
Can_Heizg<- Can_dat%>% mutate(Grdz = ifelse(Temp <=15,20-Temp,0),Hzstd = ifelse(Grdz>0,TRUE,FALSE))
Can_Heizg_00_20<- Can_Heizg%>% subset(datetime <= ymd("2020-12-31"))
# create tidy tibble
Can_val_00_20<-Can_Heizg_00_20%>%na.omit()%>% pivot_longer(cols = -c(1,2,3,10),names_to = "Comp",values_to = "values")%>%
  mutate(Comp = as_factor(Comp))
Cmps<- levels(Can_val_00_20$Comp)%>% setdiff(c("Temp","WG")) #"NO2"  "O3"   "NO"   "Grdz"
summary(Can_val_00_20)
# components linear trend
Can_val_00_20 %>% subset(Comp %in% Cmps)%>%
  ggplot(aes(x = datetime))+
  geom_smooth(method = "lm",mapping = aes(y= values,col= Comp))+
  facet_grid(.~Hzstd)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(" 21-Year-Trend(regression)
NO2, NO, O3, Heating(Grdz)",
          subtitle = "Heating hours Stgt.-Bad Cannstatt")+
  labs(x= "", y = "Values(μg/m3,m/s,°C)")
ggsave("Trend_NO2.NO.O3.Tem_Can.png",path = save.figs)
# componente periodic change
Can_val_15_20<-Can_val_00_20%>% na.omit()%>% subset(datetime>= ymd("2015-01-01"))
Can_val_15_20%>% na.omit() %>% filter(Comp %in% c("Grdz","NO","NO2","O3"))%>% 
  ggplot(aes(x = datetime))+
  geom_smooth(method = "gam",mapping = aes(y= values,col= Comp),formula = y ~ s(x, k = 21))+
  ggtitle("NO2, NO, O3,Temperature, Heating(Grdz)
          Variation 2015 - 2020",
          subtitle = "Stgt. Bad Cannstatt")+
  labs(x= "", y = "Values(μg/m3,C)")+facet_grid(.~ Hzstd)
ggsave("NO2.NO.O3.Tem_Can_6_years.png",path = save.figs)

# NO2 ~ datetime

Can_Heizg_00_20%>% na.omit()%>%ggplot(aes(x= datetime))+
  geom_point(aes(y= NO2), size =0.1, alpha = 0.08)+
  geom_smooth(method = "gam", aes(y= NO2), formula= y ~ s(x, k=21),col = "red")+
  ggtitle("NO2 1-year-periodicity
Stg. Bad Cannstatt 2000 to 2020 ",
          subtitle = "points ~ 1-h-means")+
  labs(x= "", y= "NO2[μg/m3]")
ggsave("NO2_periodicity_00_20.png", path = save.figs)

# selcting smaller time-intervall
Can_Heizg_00_20%>% na.omit()%>% subset (datetime >= ymd("2015-01-01"))%>%
  ggplot(aes(x= datetime))+
  geom_point(aes(y= NO2), size =0.1, alpha = 0.08)+
  geom_smooth(method = "gam", aes(y= NO2), formula= y ~ s(x, k=21),col = "red")+
  ggtitle("NO2 1-year-periodicity
Stg. Bad Cannstatt 2015 to 2020 ",
          subtitle = "points ~ 1-h-means")+
  labs(x= "", y= "NO2[μg/m3]")
ggsave("NO2_periodicity_15_20.png", path = save.figs)  

Can_val_00_20%>% na.omit() %>% subset (datetime >= ymd("2015-01-01")) %>% filter(Comp %in% c("NO2","NO","O3","Grdz"))%>%
  ggplot(aes(x = datetime))+
  geom_smooth(method = "gam",mapping = aes(y= values,col= Comp),formula = y ~ s(x, k = 21))+
  ggtitle(" 6-Years-Variation 2015 -2020
NO2, NO, O3,Heating(Grdz)",
          subtitle = "Stgt. Bad Cannstatt (4452)")+
  labs(x= "", y = "Values(μg/m3,°C)")
ggsave("Period_NO2.NO.O3_Can_15_20.png",path = save.figs)


# Modelling immissions per calendar week
