setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Task_05")
source("http://jonsmitchell.com/code/fxn05.R")
calcChi()

Pop1<- simPop( Popsize = 50, nGeneration = 100, initial_p = 0.5, h = 1, s= 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0 , 1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft",legend=c("a", "b"), col = c("black", "red"), lwd = 2, bty = "n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)

Expectation<- c(10, 10, 10, 10)
Observed<- c(15, 15, 5, 5)
Chisq<- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation,Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))

Observed<- c(5,0,0,35)
Observed<- c(2,3,10,30)
Chisq<- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation,Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))

Observed<- c(5,0,0,35)
Chisq<- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation,Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))

Observed<- c(10,10,10,10)
Chisq<- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation,Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))

#The X^2 for observed all 10s is 0 
Observed<- c(0,0,0,40)
Chisq<- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation,Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))

#The X^2 for observed when all 40 are in one is 120
#X^2 is related to the closeness you are to expected with the closer being a lower number and the farther away being greater number

setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Task_05")
Data<-read.csv("http://jonsmitchell.com/data/biol112labresults.csv",stringsAsFactors=F)
Data

results<-read.csv("http://jonsmitchell.com/data/biol112labresults.csv",stringsAsFactors=F)
counts<-results[,c("yellow","red","green","blue","black","tan")]
backgrounds<-c("White","Red","Yellow","Green","Blue","Black")
backgroundCol<-c("white","#d53e4f","#fee08b","#abdda4","#3288bd","black")

calcChi(counts[1,])
Chisqs<-apply(counts,1,calcChi)
plotChis(counts)
#The higher the X^2 is the more isolated the idividual population group is distributed, as X^2 gets closer to zero the distributed values are balanced over the individual colors. 

#The avg X^2 is 60.99081, you would have to interpret the plots as only a few colors will have high distribution
Avg<-mean(Chisqs)
#the average X^2 in the packet is more evenly distributed the the actual Avg at 60.99081

backgroundAvgs<-tapply(Chisqs,results[,3],mean)
backgroundAvgs
#The backgrounds X^2 do vary with the change with the colors of certain tooth picks such as red and white 

propSig<-length( which( Chisqs>11.70))/length(Chisqs)
propSig
percSig<-round(100*propSig)
percSig
#the P value did not change that much from the propSig

par(las = 1,mar=c(4,4,1,1), mgp = c(2,0.5,0), tck = -0.01,cex.axis=1)
hist(Chisqs,main="", xlab="chi-squared values",ylab="frequency")

par(las = 1,mar = c(4,4,1,1), mgp = c(2,0.5,0), tck = -0.01,cex.axis=1)
plot(1,1,xlim=c(0,400),ylim=c(1,8.5),xlab="",ylab="",type="n",yaxt="n")

axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)

counter<-1
for (i in backgrounds) {
Data<- Chisqs[which(results[,3] == i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter<-counter+1
}

abline(v = 11.70, lty=2, lwd = 2, col='black')
#The only thing that meaningful difference between the color is the certain peaks are higher and the X^2 values are greater on some 

Simulation<-simDraws(10000)

addHist(Y=7,Dat=Simulation, Color="lightgrey")
mtext(side=2,at=7,line=0,"simulated")
abline(v = 11.70, lty=2,lwd=2)

Fit<-c(1,1,1,1,1,1)
names(Fit)<-1:6
Simulation2<- simDraws(1e4,w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit<-c(0.1,1,1,1,1,1)
names(Fit)<-1:6
Simulation3<- simDraws(1e4,w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))

Fit<-c(0.5,0.6,0.7,1,1,1)
names(Fit)<-1:6
Simulation4<- simDraws(1e4,w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))

Fit<-c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit)<-1:6
Simulation5<- simDraws(1e4,w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))

Fit<-c(0.1,0.1,0.1,0.1,0.1,1)
names(Fit)<-1:6
Simulation6<- simDraws(1e4,w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))

Simulation7<-c(Simulation2,Simulation3,Simulation4,Simulation5,Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))

#
