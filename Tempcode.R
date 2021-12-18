library(tidyverse)
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
comps <- names(Can_tbl)%>%setdiff(c("station","datetime"))
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


