library(tidyverse)
# create tidy tibble
Can_val_00_20<-Can_Heizg_00_20%>%na.omit()%>% pivot_longer(cols = -c(1,2,3,10),names_to = "Comp",values_to = "values")%>%
  mutate(Comp = as_factor(Comp))
Cmps<- levels(Can_val_00_20$Comp)%>% setdiff(c("Temp","WG")) #"NO2"  "O3"   "NO"   "Grdz"
summary(Can_val_00_20)
#=========================
# components linear trend
#===========================
Can_val_00_20 %>% subset(Comp %in% Cmps)%>%
  ggplot(aes(x = datetime))+
  geom_smooth(method = "lm",mapping = aes(y= values,col= Comp))+
  facet_grid(.~Hzstd)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(" 21-Year-Trend(regression)
NO2, NO, O3, Heating(Grdz)",
          subtitle = "Heating hours Stgt.-Bad Cannstatt
Heating = TRUE, NO_Heating = FALSE")+
  labs(x= "", y = "Values(μg/m3,m/s,°C)")
ggsave("Trend_NO2.NO.O3.Tem_Can.png",path = save.figs)
#

Can_dat_Heizg%>% head(2)
Can_val_00_20<- Can_dat_Heizg%>% na.omit()%>% dplyr::select(1:8,10:11,14)%>%
  pivot_longer(-c(1:3,9,11),names_to = "Comp",values_to = "values")%>%
  mutate(Comp = as_factor(Comp))
Cmps<- levels(Can_val_00_20$Comp)%>% setdiff(c("Temp","WG")) #"NO2"  "O3"   "NO"   "Grdz"
Can_val_00_20 %>% subset(Comp %in% Cmps)%>%
  ggplot(aes(x = datetime))+
  geom_smooth(method = "lm",mapping = aes(y= values,col= Comp),se = F)+
  facet_grid(.~Hzg_lvl)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(" 21-Year-Trend(regression)
NO2, NO, O3, Heating(Grdz-heating)",
          subtitle = "Stgt.-Bad Cannstatt
 No-heating(no.ht),Low-heating(low.ht),Medium_heating(medium.ht),Maximum-heating(ma.ht")+
  labs(x= "", y = "Values(μg/m3,m/s,°C)")
