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


## Estimating root means of roots architectural variables from the 4 views of each root from the 3 roots 

Plotmeans2023 <- Roots %>%
  group_by(PlotID) %>%
  arrange(PlotID)%>%
  summarize(across(FDT:RTW, list(Mean = mean, SD = sd)))  #Summarize drops columns, mutate doesnt.

write.csv (Means, "Plotmeans2023.csv")

## Add experimental design information to data set with phenotypes using root ID
Fieldbook2023 <- read.csv("Fieldbook2023.csv", header = TRUE)

library(dplyr)
All2023 = merge(Fieldbook2023,Plotmeans2023, by="PlotID")

All2023$Rep <- as.factor(All2023$Rep)
All2023$Rotation <- as.factor(All2023$Rotation)
All2023$Nitro <- as.factor(All2023$Nitro)
All2023$HYB <- as.factor(All2023$HYB)
All2023$Range <- as.factor(All2023$Range)
All2023$Row <- as.factor(All2023$Row)
All2023$FDT_Mean <- as.numeric(All2023$FDT_Mean)
All2023$FDT_SD <- as.numeric(All2023$FDT_SD)
All2023$FDS_Mean <- as.numeric(All2023$FDS_Mean)
All2023$FDS_SD <- as.numeric(All2023$FDS_SD)
All2023$LDA_Mean <- as.numeric(All2023$LDA_Mean)
All2023$LDA_SD <- as.numeric(All2023$LDA_SD)
All2023$RTA_Mean <- as.numeric(All2023$RTA_Mean)
All2023$RTA_SD <- as.numeric(All2023$RTA_SD)
All2023$SD_Mean <- as.numeric(All2023$SD_Mean)
All2023$SD_SD <- as.numeric(All2023$SD_SD)
All2023$RTW_Mean <- as.numeric(All2023$RTW_Mean)
All2023$RTW_SD <- as.numeric(All2023$RTW_SD)


library(lattice)  
library(car)
library(agricolae)
graph <- with(All2023, xyplot(RTA_Mean ~ Rotation | Nitro, groups = HYB))
graph


## Analysis of FDT  using split split RCBD 

model.FDT <- with(All2023, ssp.plot(Rep, Rotation, Nitro, HYB, FDT_Mean))

gla<-model.FDT$gl.a 
glb<-model.FDT$gl.b 
glc<-model.FDT$gl.c

Ea<-model.FDT$Ea 
Eb<-model.FDT$Eb 
Ec<-model.FDT$Ec

out1<-with(All2023,HSD.test(FDT_Mean,Rotation,gla,Ea,group=TRUE,console=TRUE))
out2<-with(All2023,HSD.test(FDT_Mean,Nitro,glb, Eb, group=TRUE, console=TRUE))
out3<-with(All2023,HSD.test(FDT_Mean,HYB,glc,Ec,console=TRUE))

par(mfrow=c(1,3),cex=0.6)
plot(out1,xlab="Rotation System",las=1,variation="SE")
plot(out2,xlab="Nitrogen Treatment",variation="IQR")
plot(out3,xlab="Genotype",variation="IQR")

par(mfrow=c(1,3),cex=0.6)
bar.err(out1$means,variation = "SE", xlab="Rotation", ylab="Top Fractal Dimension", ylim=c(0,1.6))
bar.err(out2$means,variation = "SE", xlab="Nitrogen", ylab="Top Fractal Dimension", ylim=c(0,1.6))
bar.err(out3$means,variation = "SE", xlab="Hybrid", ylab="Top Fractal Dimension", las =2, ylim=c(0,1.6))


## Analysis of RTA  using split split RCBD 

model.RTA <- with(All2023, ssp.plot(Rep, Rotation, Nitro, HYB, RTA_Mean))

gla<-model.RTA$gl.a 
glb<-model.RTA$gl.b 
glc<-model.RTA$gl.c

Ea<-model.RTA$Ea 
Eb<-model.RTA$Eb 
Ec<-model.RTA$Ec

out4<-with(All2023,HSD.test(RTA_Mean,Rotation,gla,Ea,group=TRUE,console=TRUE))
out5<-with(All2023,HSD.test(RTA_Mean,Nitro,glb, Eb, group=TRUE, console=TRUE))
out6<-with(All2023,HSD.test(RTA_Mean,HYB,glc,Ec,console=TRUE))

par(mfrow=c(1,3),cex=0.6)
plot(out4,xlab="Rotation System",las=1,variation="SE")
plot(out5,xlab="Nitrogen Treatment",variation="IQR")
plot(out6,xlab="Genotype",variation="IQR")

par(mfrow=c(1,3),cex=0.6)
bar.err(out4$means,variation = "SE", xlab="Rotation", ylab="Root Angle", ylim=c(0,75))
bar.err(out5$means,variation = "SE", xlab="Nitrogen", ylab="Root Angle", ylim=c(0,75))
bar.err(out6$means,variation = "SE", xlab="Hybrid", ylab="Root Angle", las =2, ylim=c(0,75))

## Analysis of Root Weight using split split RCBD 

model.RTW <- with(All2023, ssp.plot(Rep, Rotation, Nitro, HYB, RTW_Mean))

gla<-model.RTW$gl.a 
glb<-model.RTW$gl.b 
glc<-model.RTW$gl.c

Ea<-model.RTW$Ea 
Eb<-model.RTW$Eb 
Ec<-model.RTW$Ec

out7<-with(All2023,HSD.test(RTW_Mean,Rotation,gla,Ea,group=TRUE,console=TRUE))
out8<-with(All2023,HSD.test(RTW_Mean,Nitro,glb, Eb, group=TRUE, console=TRUE))
out9<-with(All2023,HSD.test(RTW_Mean,HYB,glc,Ec,console=TRUE))

par(mfrow=c(1,3),cex=0.6)
plot(out7,xlab="Rotation System",las=1,variation="SE")
plot(out8,xlab="Nitrogen Treatment",variation="IQR")
plot(out9,xlab="Genotype",variation="IQR")

par(mfrow=c(1,3),cex=0.6)
bar.err(out7$means,variation = "SE", xlab="Rotation", ylab="Root Weight", ylim=c(0,250))
bar.err(out8$means,variation = "SE", xlab="Nitrogen", ylab="Root Weight", ylim=c(0,250))
bar.err(out9$means,variation = "SE", xlab="Hybrid", ylab="Root Weight", las =2, ylim=c(0,250))





