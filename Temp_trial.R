library(tidyverse)
library(knitr)
library(lubridate)
library(broom)
library(gam)
help("gam")
library(mgcv)# author of mgcv: wood
#===== background information on mgcv
help("mgcv-package")
wood_url<-"https://www.maths.ed.ac.uk/~swood34/"
browseURL(wood_url)
#===========
BW_Rdat_path<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
BW_list_tbl<-readRDS(file.path(BW_Rdat_path,"BW_list_tbl.rds"))# 23 Stationen 
Stationsliste<-readRDS(file.path(BW_Rdat_path,"Stationsliste.rds"))# 31 Stationen mit Koordinaten
BW_list_tbl%>% summary()                     
Stationsliste%>% summary()
# detrend with linear model
Brn_NO2_fit<- lm(NO2~datetime,data = BW_list_tbl$Brn)%>% augment()
dat<-BW_list_tbl$Brn%>% dplyr::select(datetime,NO2)  
dat<-dat%>% mutate(datetime=as.numeric(floor_date(datetime,unit= "1 hours")),dat_h=datetime/3600)
dim(dat)
head(dat)

mgcv::bam(NO2 ~ s(dat_h,k= 10), 
    data = dat, sp = 0.1)%>% plot(residuals = FALSE,
                                  shade= TRUE,shade.col= "red" )
Brn_NO2_detrend<-augment(Brn_NO2_fit)%>% dplyr::select(datetime, NO2resd= .resid,NO2trnd= .fitted)
Brn_NO2_detrend%>% ggplot(aes(x= datetime))+
  geom_line(aes(y= NO2resd),size =0.01)+
  geom_smooth(method = "auto",aes(y= NO2resd),col = "red")
Brn_NO2_detrend%>% head(2)
