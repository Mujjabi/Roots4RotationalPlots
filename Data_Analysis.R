# This is a script for analysis root data from the rotational plots. 

## Importing raw data
Roots <- read.csv("Roots2023_Clean.csv", header = TRUE)

#Change variables to numeric or factors
Roots$BarCode <- as.factor(Roots$BarCode)
Roots$View <- as.factor(Roots$View)
Roots$FDT <- as.numeric(Roots$FDT)
Roots$FDS <- as.numeric(Roots$FDS)
Roots$LDA <- as.numeric(Roots$LDA)
Roots$RTA <- as.numeric(Roots$RTA)
Roots$SD <- as.numeric(Roots$SD)
Roots$RTW <- as.numeric(Roots$RTW)


## Estimating means of roots architectural variables from the 4 views






## Combining root data with fieldbook design variables using root ID





