
dat_path = "/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW_Rdat"
Can_dat_Heizg<-readRDS(file.path(dat_path,"BW_list_tbl.rds"))$Stg_Can%>% 
                  mutate (Grdz= ifelse(Temp <=15,20-Temp,0))%>% 
                  na.omit()  
read_add.ht <- function(pth,stn) { 
   readRDS(pth)[[stn]]%>% mutate(Grdz= ifelse(Temp <=15,20-Temp,0))%>%na.omit()
} 
read_add.ht(file.path(dat_path,"BW_list_tbl.rds"),"Stg_Can")
stn.nms<-names(BW_list_tbl)
map_df(stn.nms,read_add.ht(file.path(dat_path,"BW_list_tbl.rds"),`[`))
