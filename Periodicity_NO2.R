# reference file mgcviz.rmd 
library(lubridate)
library(tidyverse)
library(mgcv)
library(car)
library(grid)
library(animation)
library(mgcViz)
#=====================
# try to find "best fit of model to data and avoid over fitting (which is fitting to noise also)sp := smoothing parameter
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))
Can_Dat<- BW_list_tbl$Stg_Can%>% 
  dplyr::select(datetime,NO2)%>% mutate(hr= format(datetime,"%H"), wday= as_factor(format(datetime, "%w")),KWeek= format(datetime,"%W"))
summary(Can_Dat)# NO2 Data from 2000 to 2020
Can_Dat_11 <- Can_Dat %>% subset( datetime > ymd("2011-01-01"))%>% subset(datetime<=ymd("2011-12-31") )
Can_Dat_15_16<-Can_Dat %>% subset( datetime > ymd("2015-01-01"))%>% subset(datetime<=ymd("2016-12-31") )
Can_Dat_11 %>% ggplot(aes(x= datetime, y= NO2))+
  geom_line(alpha = 0.4)+
  geom_smooth(method ="gam",formula=  y~ s(x, k = 6), col = "red", mapping = aes(x=datetime,y=NO2),data = Can_Dat_11)
Can_Dat_15_16 %>% ggplot(aes(x= datetime, y= NO2))+
  geom_line(alpha = 0.2)+
  geom_smooth(method ="gam",formula=  y~ s(x, k = 6), col = "red", mapping = aes(x=datetime,y=NO2),data = Can_Dat_15_16)
Can_Dat_11<-Can_Dat_11%>% mutate(wday= car::recode(wday,"0='Sunday';1='Monday';2='Tuesday';3='Wednesday';4='Thursday';5='Friday';6='Saturday'"))
levels(Can_Dat_11$wday) <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
N<- NROW(Can_Dat_11)# N = 8736= 364*24
wndw <- N/364 # 364 Tage
Can_table <- tibble(NO2 = Can_Dat_11$NO2,
                         HR = as.integer(Can_Dat_11$hr),
                         Weekday= Can_Dat_11$wday,
                         KW =as.numeric(Can_Dat_11$KWeek))
gam_1<- gam(NO2 ~ s(HR, bs= "cr",k=17)+Weekday+s(KW, bs = "ps", k=5),
            data = Can_table, family= "gaussian")
gam_1 <- getViz(gam_1)
plt <-plot(sm(gam_1,1))
listLayers(plt)
plt <-plot(gam_1,shade = TRUE)
plt+l_ciLine(mul = 5, colour = "blue", linetype = 2)+
  l_fitLine(col= "red",linetype =1)+
  l_points(size= 0.1)+
  ggtitle("Can NO2 - Immissions 2011",
          subtitle = "24-h cycle")+
  labs(x= " hour")


sm(gam_1,2)%>% summary()# select 2nd smooth
sm(gam_1,2)%>%class()

plt2<- plot(sm(gam_1,2))+ggtitle("Can: NO2 -Immissions 2011 ",
                                 subtitle = "Calendar week")+ labs(x = "Calendar Week")
plt2+l_fitLine(col = "red") + #  layer add lines graphical parameters as with geom_line
  l_ciLine(col = "blue",mul = 5)  # adds confidence intervalls 95%
# 2D smootheffect
gam_2D_HR_WK<- gam(NO2 ~ s(HR, KW) + Weekday, data = Can_table, method = "REML")
gam_2D_HR_WK<- getViz(gam_2D_HR_WK)
plot(sm(gam_2D_HR_WK, 1)) + l_fitRaster() + l_fitContour() + l_points()+labs(title="Can NO2-Immissions")
listLayers(plot(sm(gam_2D_HR_WK, 1)))
listLayers(plot(pterm(gam_2D_HR_WK,1)))
plot(pterm(gam_2D_HR_WK,1))+ l_points()+l_fitLine(col = "red")#+ l_ciPoly(col = "blue")+labs(title = "Can NO2- Immissions")
plot(gam_2D_HR_WK,allTerms = TRUE,pages =1)
gam_2D_HR_WK%>%summary()
gridPrint(plot(sm(gam_2D_HR_WK, 1))+ l_fitRaster() + l_fitContour() + l_points()+labs(title="Can NO2-Immissions"),
          plot(pterm(gam_2D_HR_WK, 1))+l_ciBar(col = "red")+ l_fitBar(col = "blue")+labs(title = "Can NO2- Immissions"),ncol = 2)
