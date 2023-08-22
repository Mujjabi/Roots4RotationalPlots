# This is a script for analysis root data from the rotational plots. 

## Importing raw data
Roots <- read.csv("Roots2023_Clean.csv", header = TRUE)

#Change variables to numeric or factors
Roots$RootID<- as.factor(Roots$RootID)
Roots$PlotID <- as.factor(Roots$PlotID)
Roots$View <- as.factor(Roots$View)
Roots$FDT <- as.numeric(Roots$FDT)
Roots$FDS <- as.numeric(Roots$FDS)
Roots$LDA <- as.numeric(Roots$LDA)
Roots$RTA <- as.numeric(Roots$RTA)
Roots$SD <- as.numeric(Roots$SD)
Roots$RTW <- as.numeric(Roots$RTW)


## Estimating plots means of roots architectural variables from the 4 views and 3 roots
library(dplyr)
Plotmeans2023 <- Roots %>%
  group_by(PlotID) %>%
  summarise(across(FDT:RTW, list(Mean =mean, SD =sd)))%>%
  arrange(PlotID)
  
write.csv (Means, "Plotmeans2023.csv")

## Add experimental design information to data set with phenotypes using root ID
Fieldbook2023 <- read.csv("Fieldbook2023.csv", header = TRUE)

library(dplyr)
ALL2023 = merge(Fieldbook2023,Plotmeans2023, by="PlotID")

All2023$REP=as.factor(all$All2023)
All2023$Rot=as.factor(All2023$Rot)
All2023$Nitro=as.factor(All2023$Nitro)
All2023$HYB=as.factor(All2023$HYB)
All2023$Range=as.factor(All2023$Range)
All2023$Row=as.factor(All2023$Row)
All2023$Plot=as.factor(All2023$Plot)
All2023$FDT <- as.numeric(All2023$FDT)

All2023$FDS <- as.numeric(All2023$FDS)

All2023$LDA <- as.numeric(All2023$LDA)

All2023$RTA <- as.numeric(All2023$RTA)

All2023$SD <- as.numeric(All2023$SD)

All2023$RTW <- as.numeric(All2023$RTW)




