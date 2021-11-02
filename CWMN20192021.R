library(ggplot2)
library(gridExtra)
library(dplyr)

windowsFonts(A = windowsFont("Gotham Medium"))  

M19S21<-read.csv("C:/Users/Slocombe/NepRWA/Staff - AllStaff/Water - CWMN/CWMN '21/Data Export/May2019Sept2021.csv")

M19S21$Month <- factor(M19S21$Month, levels = c("May", "June",  "July", "August", "September", "October"))

##Generalized mean for EC (2019-2021)

M19S21GM<-select(M19S21, SiteID, EC, Year)

M19S21GM<-na.omit(M19S21GM)

M19S21GM<-M19S21GM %>% group_by(Year, SiteID)%>% 
  mutate(GM=exp(mean(log(EC))))

M19S21GM<-select(M19S21GM, SiteID, GM, Year)

M19S21GM<-unique(M19S21GM)

ggplot(M19S21GM, aes(y=GM, x=reorder(SiteID, -GM)))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x= element_text(angle = 90))+
  geom_hline(yintercept=126, color="green")+
  facet_wrap(~Year)

MaySepNER002<-filter(MaySep, SiteID == "NER002")
exp(mean(log(MaySepNER002$EC)))
