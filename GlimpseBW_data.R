library(tidyverse)
library(knitr)
library(lubridate)
BW_Rdat_path<- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
list.files(path=BW_Rdat_path)
load(file.path(BW_Rdat_path,"BW_list_tbl.RData"))
summary(BW_list_tbl)
str(BW_list_tbl,levels=2)
station.names<-names(BW_list_tbl)
rural.stations<- c("Alb","Odw","Sws")
trafic.stations <- c("Lbg_Friedr","Nck","Rt_leder")
urban.backgrd.stations<- station.names%>% setdiff(c(rural.stations,trafic.stations))

comp_detect <- function(df,cmp) {
  exst<- cmp %in% names(df)
  return(exst)
}
# plot  function for all stations
plt_NO2_trnd <- function(df) {
  df<-df%>% as_tibble()
  ifelse (comp_detect(df,"NO2"),
          {df <- na.omit(df)
  plt <-ggplot(df,aes(x= datetime,y=NO2))+
    geom_point(size = 0.001)+
    geom_smooth(method= "lm",col = "red")+
    ggtitle("NO2-immissions 20 years",
    subtitle = paste(first(df$name),first(df$station)))},NA
    )
  ggsave(filename=paste0("NO2_trend_20y_",first(df$name),".png"),
         path = "figs/",
         plot = plt)

 
}
plt_NO2_trnd(BW_list_tbl$Stg_Hoh)
BW_list_tbl%>% map(plt_NO2_trnd)
names(BW_list_tbl)
BW_mean_NO2<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"NO2"),
                                                  mean(x$NO2,na.rm=TRUE),NA)))
BW_median_NO2<- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"NO2"),
                                                  median(x$NO2,na.rm=TRUE),NA )))
BW_var_NO2 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"NO2"),
                                                  var(x$NO2,na.rm=TRUE),NA)))
BW_mean_WG <- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"WG"),
                                                  mean(x$WG,na.rm=TRUE),NA )))
BW_var_WG <-map_dbl(BW_list_tbl,function(x) (ifelse(comp_detect (x,"WG"),
                                                  var(x$WG,na.rm=TRUE),NA) ))



# O3
BW_mean_O3<-map_dbl(BW_list_tbl , function(x) ( ifelse (comp_detect(x,"O3"),
                                                         mean(x$O3,na.rm=TRUE),NA)))
BW_median_O3<- map_dbl(BW_list_tbl,function(x) (ifelse (comp_detect (x,"O3"),
                                                         median(x$O3,na.rm=TRUE),NA )))
BW_var_O3 <- map_dbl(BW_list_tbl, function(x) (ifelse (comp_detect (x,"O3"),
                                                        var(x$O3,na.rm=TRUE),NA)))

BW_statistic <- tibble (Station =  names(BW_list_tbl),
                                NO2_mean=  BW_mean_NO2,
                                NO2_median=BW_median_NO2,
                                NO2_var =  BW_var_NO2,
                                WG_mean =  BW_mean_WG,
                                WG_var =   BW_var_WG,
                                O3_mean =  BW_mean_O3,
                                O3_median= BW_median_O3,
                                O3_var =   BW_var_O3)
# summary of mean and median NO2, WG
BW_statistic%>% arrange(NO2_mean)%>% knitr::kable(digits=1)
save(BW_statistic, file= "BW_statistic.RData")
# Linear Regression 
# lineares Regressions- Modell
stat.model <- function (df) {
  lm(NO2 ~ datetime, data = df)
}
BW_station_names<-names(BW_list_tbl)
BW_dat_dfr<-tibble()
for (stnm in BW_station_names) {
  
  dfr <- BW_list_tbl[[stnm]]
  if(NROW(dfr) >1)
    BW_dat_dfr <- bind_rows(BW_dat_dfr,dfr)
}
BW_dat_dfr$name<- as_factor(BW_dat_dfr$name)
BW_dat_dfr<-BW_dat_dfr %>% dplyr::select(name,datetime,NO2)

summary(BW_dat_dfr)
head(BW_dat_dfr)

BW_dat_dfr%>%  ggplot()+
  geom_smooth(method = "lm",mapping = aes(x= datetime,y=NO2,color=name), data =BW_dat_dfr)+
  ggtitle("NO2 Immissionen
  Trend (Regressionsgerade)" ,
          subtitle= " 21 Stationen in BW")

