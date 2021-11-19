library(ggplot2)
library(gridExtra)
library(dplyr)
library(cowplot)

windowsFonts(A = windowsFont("Gotham Medium"))  

##Load whatever CWMN data you need and order factors
##Note that before loading data, add columns for Month, Year
setwd("~/R Projects/CWMNGeneralGraphs")
MayOct <- read.csv("./Data/May2020Oct2021.csv")

MayOct$Month <- factor(MayOct$Month, levels = c("May", "June",  "July", "August", "September", "October"))

#Filter for desired year/months/sites

Oct2021<- filter(MayOct, Year=="2021" & Month=="October")

##Plot E.Coli for all sites

ggplot(MayOct, aes(fill=Month, y=EC, x=reorder(SiteID, -EC)))+
  geom_hline(aes(yintercept= 630, linetype = "Safe for Boating, 630 CFU/100mL"), colour= 'gold', size=1.2) +
  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming, 235 CFU/100mL"), colour= 'green', size=1.2) +
  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
                        guide = guide_legend(override.aes = list(color = c("gold", "green"))))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="E Coli, CFU/100mL", x = " ")+theme(legend.position = c(0.75, 0.75))+
  scale_fill_manual("Month", values = c("September" = "darkgray"))+
  theme(text = element_text(family = "A"))+
  theme(text=element_text(size=20))+ guides(fill = "none")
#+
  geom_text(x=41, y=280, label="No sample collected", angle=90, size=4, color="grey")  ##Adjust/skip as needed

##Plot TP for all sites

#Assign Flowing/Pond to sites. Update the code as new sites are added/rotated through

MayOct <- MayOct %>% 
  mutate(Flow = if_else(SiteID == "WIP002" | SiteID == "WIP003" | SiteID == "NER002" | SiteID =="NER075" | SiteID=="MMB106", "Pond", "Flowing"))

Pond<-filter(MayOct, Flow == "Pond")
Flowing<-filter(MayOct, Flow == "Flowing")

flowing<-ggplot(Flowing, aes(fill=Month, y=TP, x=reorder(SiteID, -TP)))+
  geom_hline(aes(yintercept= .05, linetype = "Flows into Lake/Reservoir"), colour= 'yellow', size=1.2) +
  geom_hline(aes(yintercept= .1, linetype = "Flowing Water"), colour= 'gold', size=1.2) +
  theme(legend.position = c(.75, .8))+ ylim(0,.55)+
  scale_linetype_manual(name = "TP Standards", values = c(1, 1),
                        guide = guide_legend(override.aes = list(color = c("orange", "yellow"))))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Total Phosphorus, mg/L", x = " ")+
  scale_fill_manual("Month", values = c("September" = "darkgray"))+
  theme(text = element_text(family = "A"))+
  theme(legend.margin = margin(0.2, 0.6, 0.2, .2, "cm"))+
  theme(legend.background = element_rect(fill = "white", linetype=0))+
  theme(text=element_text(size=15))+ guides(fill = "none")

pond<-ggplot(Pond, aes(fill=Month, y=TP, x=reorder(SiteID, -TP)))+
  geom_hline(aes(yintercept= .025, linetype = "Pond"), colour= 'green', size=1.2) +
  theme(legend.position = c(.4, 0.85))+ ylim(0,.55)+
  scale_linetype_manual(values = c(1),
                        guide = guide_legend(override.aes = list(color = c("green"))))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y=" ", x = " ")+theme(legend.title = element_blank())+
  scale_fill_manual("Month", values = c("September" = "darkgray"))+
  theme(text = element_text(family = "A"))+
  theme(legend.margin = margin(0.2, 0.3, 0.2, 0, "cm"))+
  theme(text=element_text(size=15))+ guides(fill = "none")

ggdraw() +
  draw_plot(flowing, 0, 0, .83, 1) +
  draw_plot(pond, .83, 0, .17, 1)

##Plot DOMgL for all sites

ggplot(MayOct, aes(fill=Month, y=DOMgL, x=reorder(SiteID, -DOMgL)))+
  geom_hline(aes(yintercept= 6, linetype = "Cold Water Fish"), colour= 'green', size=1.2) +
  geom_hline(aes(yintercept= 5, linetype = "Warm Water Fish"), colour= 'gold', size=1.2) +
  scale_linetype_manual(name = "Dissolved Oxygen Standards", values = c( 1, 1), 
                        guide = guide_legend(override.aes = list(color = c("green", "gold"))))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Dissolved Oxygen, mg/L", x = " ")+theme(legend.position = c(0.75, 0.80))+
  scale_fill_manual("Month", values = c("September" = "darkgray"))+
  theme(text = element_text(family = "A"))+
  theme(text=element_text(size=20))+ guides(fill = "none")
##+
  geom_text(x=41, y=6, label="No Data Collected", angle=90, size=4, color="grey")+
  geom_text(x=40, y=6, label="Awaiting Data", angle=90, size=4, color="grey")+
  geom_text(x=39, y=6, label="Awaiting Data", angle=90, size=4, color="grey")+
  geom_text(x=38, y=6, label="Awaiting Data", angle=90, size=4, color="grey")+
  geom_text(x=37, y=6, label="Awaiting Data", angle=90, size=4, color="grey")+
  geom_text(x=36, y=6, label="Awaiting Data", angle=90, size=4, color="grey")+
  geom_text(x=35, y=6, label="Awaiting Data", angle=90, size=4, color="grey")+
  geom_text(x=34, y=6, label="No Data Collected", angle=90, size=4, color="grey")

Oct2020<-filter(MayOct, Month=="October" & Year=="2020")

##Plot pH for all sites

ggplot(Oct2020, aes(fill=Month, y=pH, x=reorder(SiteID, -pH)))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=8.3, color="green")+
  geom_hline(yintercept=6.5, color = "green")+
  labs(y="pH", x = "Site ID")+theme(legend.position = c(0.6, 0.75))+
  scale_fill_manual("Month", values = c("September" = "darkgray"))+
  theme(text = element_text(family = "A"))

plot2+ geom_hline(aes(yintercept= 8.3, linetype = "Upper Bound"), colour= 'green') +
  geom_hline(aes(yintercept= 6.5, linetype = "Lower Bound"), colour= 'green') +
  theme(legend.position = c(0.78, 0.83))+
  scale_linetype_manual(name = "Dissolved Oxygen Standards", values = c( 1, 1), 
                        guide = guide_legend(override.aes = list(color = c("green", "green"))))


##Plots for E.Coli for different stream stretches with multiple sampling sites 
#HAB MOB MPB NER PQB PTB UNB WIP
#Numbered plots are bar charts of EC (bar for each month)
#Lettered plots are box/whisker plots of EC for each site across 2021 sampling period
#Inactive numbered plots include the legends

HABMaySep <- subset(MaySep, SiteID=="HAB002" | SiteID=="HAB006" | SiteID=="HAB010")
MOBMaySep <- subset(MaySep, SiteID=="MOB001" | SiteID=="MOB032")
MPBMaySep <- subset(MaySep, SiteID=="MPB037" | SiteID=="MPB088")
NERMaySep <- subset(MaySep, SiteID=="NER002" | SiteID=="NER040" | SiteID=="NER075" |
                      SiteID=="NER080" | SiteID=="NER125" | SiteID=="NER150" |
                      SiteID=="NER179" | SiteID=="NER185" | SiteID=="NER200")
PQBMaySep <- subset(MaySep, SiteID=="PQB036" | SiteID=="PQB040")
PTBMaySep <- subset(MaySep, SiteID=="PTB028" | SiteID=="PTB035" | SiteID=="PTB047")
UNBMaySep <- subset(MaySep, SiteID=="UNB002" | SiteID=="UNB014" | SiteID=="UNB016")
WIPMaySep <- subset(MaySep, SiteID=="WIP002" | SiteID=="WIP003")

plot1<-ggplot(HABMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot1<-plotHAB+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot2<-ggplot(MOBMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot2<-plotMOB+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot3<-ggplot(MPBMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+theme(legend.position = "none")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot3<-plotMPB+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot4<-ggplot(NERMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot4<-plotNER+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot5<-ggplot(PQBMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot5<-plotPQB+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot6<-ggplot(PTBMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot6<-plotPTB+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot7<-ggplot(UNBMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+ theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot7<-plotUNB+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
#  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
#  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
#  guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plot8<-ggplot(WIPMaySep, aes(fill=Month, y=EC, x=SiteID))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept=630, color="yellow")+
  geom_hline(yintercept=235, color = "green")+
  labs(y="E Coli, CFU/100mL", x = "Site ID")+ theme(legend.position = "none")+
  scale_fill_manual("Month", values = c("May" = "#24b300", "June" = "#EBEB99", "July" = "grey", "August" = "#008fb3","September" = "darkgray"))+
  theme(text = element_text(family = "A"))

#plot8<-plotWIP+ geom_hline(aes(yintercept= 630, linetype = "Safe for Boating"), colour= 'yellow') +
# geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming"), colour= 'green') +
# scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
# guide = guide_legend(override.aes = list(color = c("yellow", "green"))))

plota<-ggplot(HABMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plotb<-ggplot(MOBMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plotc<-ggplot(MPBMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plotd<-ggplot(NERMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plote<-ggplot(PQBMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plotf<-ggplot(PTBMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plotg<-ggplot(UNBMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
ploth<-ggplot(WIPMaySep, aes(x=SiteID, y=EC)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

grid.arrange(plota, plotb, plotc, plotd, plote, plotf, plotg, ploth, ncol=4)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=4)

summary(aov(EC~SiteID, WIPMaySep))

##Point plot with May-Sep Ecoli

MaySepAvg<-MaySep %>% group_by(Month)%>%
  mutate(MonAvg=mean(EC,na.rm=TRUE))

unique<-select(MaySepAvg, Month,MonAvg)
unique<-unique(unique)

ggplot(MaySep, aes(y=EC, x=reorder(SiteID, -EC), color=Month))+
  geom_hline(aes(yintercept= 630, linetype = "Safe for Boating, 630 CFU/100mL"), colour= 'gold',size=1.2)+
  geom_hline(aes(yintercept= 235, linetype = "Safe for Swimming, 235 CFU/100mL"), colour= 'green', size=1.2)+
  scale_linetype_manual(name = "E.Coli Standard", values = c(1, 1), 
                        guide = guide_legend(override.aes = list(color = c("gold", "green"))))+
  geom_point(stat="identity", size=5, position=position_dodge(width=.4))+
  theme(axis.text.x=element_text(angle=90))+
  labs(y="E Coli, CFU/100mL", x = " ")+theme(legend.position = c(0.8, 0.6))+
  scale_color_manual(values = c("#24b300", "#a2e691", "#89d5e8", "#008fb3","darkgray"))+
  theme(text = element_text(family = "A"))+
  theme(text=element_text(size=15))

#This code adds the monthly average (May-Sep) EC across sites to the plot above
geom_hline(yintercept=113, color="#24b300", size=1)+
  geom_hline(yintercept=303, color = "red", size=1.3)+
  geom_hline(yintercept=3061, color="#89d5e8", size=1)+
  geom_hline(yintercept=287, color = "#008fb3", size=1)+
  geom_hline(yintercept=2661, color = "darkgray", size=1)



##Generalized mean for EC

MaySepGM<-MaySep %>% group_by(SiteID)%>% 
  mutate(GM=exp(mean(log(EC))))

MaySepGM<-select(MaySepGM, SiteID, GM)
MaySepGM<-unique(MaySepGM)

ggplot(MaySepGM, aes(y=GM, x=reorder(SiteID, -GM)))+
  geom_bar(position="dodge", stat="identity", width=0.5)+
  theme(axis.text.x= element_text(angle = 90))+
  geom_hline(yintercept=126, color="green")

MaySepNER002<-filter(MaySep, SiteID == "NER002")
exp(mean(log(MaySepNER002$EC)))

