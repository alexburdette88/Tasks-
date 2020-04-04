setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Project\\data")
TurtData <-read.csv("TR_data.csv", stringsAsFactors=F, header=TRUE)

#For my analysis plan, I am going to compare the Male to Female ratios of the seven major temperature dependent sex determined marine turtles, to the temperature to see if there is link between climate change and extinction rate. 

install.packages("ggplot2", dependencies=TRUE)
require("ggplot2")


#leatherBack Vectors
LeatherBack1<- TurtData[1:3,1:13]
LB_BroodSizes<-TurtData[1:3,4]
LB_MaleHatch<-TurtData[1:3,9]
LB_FemaleHatch<-TurtData[1:3,10]
LB_AvNestT<- TurtData[1:3,7]

LB_MaleHatchTotal<-TurtData[1,9]
LB_MaleHatch98<-TurtData[2,9]
LB_MaleHatch08<-TurtData[3,9]

LB_FemaleHatchTotal<-TurtData[1,10]
LB_FemaleHatch98<-TurtData[2,10]
LB_FemaleHatch08<-TurtData[3,10]


#Green Vectors
Green1<- TurtData[4:6,1:13]
GR_BroodSizes<-TurtData[4:6,4]
GR_MaleHatch<-TurtData[4:6,9]
GR_FemaleHatch<-TurtData[4:6,10]
GR_AvNestT<- TurtData[4:6,7]

#Loggerhead Vectors
Loggerhead1<- TurtData[7:9,1:12]
LO_BroodSizes<-TurtData[7:9,4]
LO_MaleHatch<-TurtData[7:9,9]
LO_FemaleHatch<-TurtData[7:9,10]
LO_AvNestT<- TurtData[7:9,7]

#Flatback Vectors
Flatback1<- TurtData[10:12,1:12]
FB_BroodSizes<-TurtData[1:21,4]
FB_MaleHatch<-TurtData[10:12,9]
FB_FemaleHatch<-TurtData[10:12,10]
FB_AvNestT<- TurtData[10:12,7]

#Hawksbill Vectors
Hawksbill1<- TurtData[13:15,1:12]
HB_BroodSizes<-TurtData[13:15,4]
HB_MaleHatch<-TurtData[13:15,9]
HB_FemaleHatch<-TurtData[13:15,10]
HB_AvNestT<- TurtData[13:15,7]

#Kemps Vectors
Kemps1<- TurtData[16:18,1:12]
KE_BroodSizes<-TurtData[16:18,4]
KE_MaleHatch<-TurtData[16:18,9]
KE_FemaleHatch<- TurtData[16:18, 10]
KE_AvNestT<- TurtData[16:18,7]

#Olive Vectors
Olive1<- TurtData[19:21,1:12]
OK_BroodSizes<-TurtData[19:21,4]
OK_MaleHatch<-TurtData[19:21,9]
OK_FemaleHatch<-TurtData[19:21,10]
OK_AvNestT<- TurtData[19:21,7]

BroodSizes<-TurtData[1:21,4]
MaleHatch<-TurtData[1:21,9] 
AvgNestTemp<-TurtData[1:21,7]
FemaleHatch<-TurtData[1:21,10]

barplot(c(LB_MaleHatchTotal,LB_FemaleHatchTotal,LB_MaleHatch98,LB_FemaleHatch98,LB_MaleHatch08,LB_FemaleHatch08),col=c('red','blue','red','blue','red','blue'),xlab="1998-2016 1998-2008 2008-2016")