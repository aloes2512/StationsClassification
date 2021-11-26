
library(lubridate)
library(tidyverse)
library(data.table)
library(mgcv)
install.packages("car")
library(car)
library(ggplot2)
library(grid)
install.packages("animation")
library(animation)
library(mgcViz)
#=====================
# try to find "best fit of model to data and avoid over fitting (which is fitting to noise also)sp := smoothing parameter
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
BW_list_tbl<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))
Dat<- BW_list_tbl$Stg_Can%>% 
  dplyr::select(datetime,NO2)%>% mutate(hr= format(datetime,"%H"), wday= as_factor(format(datetime, "%w")),KWeek= format(datetime,"%W"))
Dat <- Dat %>% subset( datetime > ymd("2011-01-01"))%>% subset(datetime<=ymd("2011-12-31") )
Dat %>% ggplot(aes(x= datetime, y= NO2))+
  geom_line()
Dat<-Dat%>% mutate(wday= car::recode(wday,"0='Sunday';1='Monday';2='Tuesday';3='Wednesday';4='Thursday';5='Friday';6='Saturday'"))
levels(Dat$wday) <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
N<- NROW(Dat)# N = 8736= 364*24
wndw <- N/382 # 364 Tage
matrix_gam <- data.table(NO2 = Dat$NO2,
                         HR = as.numeric(Dat$hr),
                         Weekday= Dat$wday,
                         KW =as.numeric(Dat$KWeek))%>%
            as.data.table()
gam_1<- gam(NO2 ~ s(HR, bs= "cr",k=17)+Weekday+s(KW, bs = "ps", k=5),
            data = matrix_gam, family= "gaussian")
layout(matrix(1:2,nrow = 1))
plt <-plot(gam_1,shade = TRUE)
gam_1%>% summary()
gam_1$sp
gam_1$Ve
gam_1$model%>% head()
gam_11<- getViz(gam_1)
sm(gam_11,2)%>% summary()# select 2nd smooth
plt1<- plot(sm(gam_11,1))+ggtitle("Can NO2 ~ HR(day_hour)")
plt2<- plot(sm(gam_11,2))+ggtitle("Can NO2 ~ Kalender week")
plt2+l_fitLine(col = "red") + #  layer add lines graphical parameters as with geom_line
  l_ciLine(col = "blue")  # adds confidence intervalls 95%
plt1+l_fitLine(col = "red") + #  layer add lines graphical parameters as with geom_line
  l_ciLine(col = "blue")+  # adds confidence intervalls 95%
  l_points(shape = 19, size = 0.5, alpha = 0.1) + theme_classic()
plot(gam_11)
gam_11%>% summary()
# 2D smootheffect
gam_2D_HR_WK<- gam(NO2 ~ s(HR, KW) + Weekday, data = matrix_gam, method = "REML")
gam_2D_HR_WK<- getViz(gam_2D_HR_WK)
plot(sm(gam_2D_HR_WK, 1)) + l_fitRaster() + l_fitContour() + l_points()
plot(gam_2D_HR_WK,allTerms = TRUE,pages =1)
gam_2D_HR_WK%>%summary()

