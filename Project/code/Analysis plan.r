setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Project\\data")
LizData <-read.csv("SquamateRepoData.csv", stringsAsFactors=F, header=TRUE)

#For my analysis plan, I am going to compare average brood size of the squamates to the average temperature test for a signifigance between temperature and number of eggs.

#Monitor Lizard Vectors
MonLizSpecies<- LizData[834:876, 1]
MonLizBroods<- LizData[834:876, 4]
MonLizTemps<- LizData[834:876, 8]

plot(MonLizTemps,MonLizBroods,main="Monitor Lizard Brood Size vs Temperature(C)",xlab="temperature(C)",ylab="Average Brood Size",pch=20,cex=1)
text(MonLizTemps,MonLizBroods,labels=MonLizSpecies,adj=NULL,pos=2,offset=1, vfont=NULL,cex=0.7,col="blue",font=NULL)
abline(lm(MonLizBroods~MonLizTemps))

#Iguana vectors
IGSpecies<-LizData[193:199,1]
IGBrood<-LizData[193:199,4]
IGTemps<-LizData[193:199,8]

plot(IGTemps,IGBrood,main="Iguana Brood Size vs Temperature(C)",xlab="temperature(C)",ylab="Average Brood Size",pch=20,cex=1)
text(IGTemps,IGBrood,labels=IGSpecies,adj=1,pos=2,offset=1, vfont=NULL,cex=0.7,col="blue",font=NULL)
abline(lm(IGBrood~IGTemps)