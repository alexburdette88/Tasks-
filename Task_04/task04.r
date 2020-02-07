trueMean1<-5
trueSD1<-5
population1<-rnorm(1e6,trueMean1,trueSD1)

trueMean2<-4
trueSD2<-5
population2<-rnorm(1e6,trueMean2,trueSD2)

Size<-50
Sample1<-sample(population1,Size)
Sample2<-sample(population2,Size)
boxplot(Sample1,Sample2)
#The populations are different population1 had a much wider distribution than population2
source("http://jonsmitchell.com/code/simFxn04.R")

MatGrandma<-makeFounder("grandma_mom")
MatGrandpa<-makeFounder("grandpa_mom")
PatGrandma<-makeFounder("grandma_da")
PatGrandpa<-makeFounder("grandpa_da")
head(MatGrandma)
nrow(MatGrandma)

Alan<-makeBaby(PatGrandma,PatGrandpa)
head(Alan)

Brenda<-makeBaby(MatGrandma,MatGrandpa)
head(Brenda)
Focus<-makeBaby(Brenda,Alan)

#1/2
ToMom<-length(grep("mom",Focus))/length(Focus)
#1/4
ToMomMom<-length(grep("grandma_mom",Focus))/length(Focus)
ToMomDad<-length(grep("grandpa_mom",Focus))/length(Focus)
ToDadMom<-length(grep("grandma_da",Focus))/length(Focus)
ToDadDad<-length(grep("grandpa_da",Focus))/length(Focus)
ToMomDad
ToMomMom
ToDadMom
ToDadDad
#no the focus is not equally related to both sets of grandparents,and is quite skewed it appears,not what I expected Hes related to PatGrandma,PatGrandpa=47% and Maternal by 28 and 22%

Sibling_01<-makeBaby(Brenda,Alan)
#1/2 it was close to what I Guessed
ToSib<-length(intersect(Focus,Sibling_01))/length(Focus)
ToSib
#he should share close to 50% with each sibling 
ManySibling<-replicate(1e3,length(intersect(Focus,makeBaby(Brenda,Alan)))/length(Focus))
ManySibling
mean(ManySibling)

quantile(ManySibling)
mean(ManySibling)
plot(density(ManySibling),main="",xlab="proportion shared genes")
#The range of values in the plot are average points for the concentration of genes the focus shares with the siblings.

HWE<-function(p){
aa<-p^2
ab<-2*p*(1-p)
bb<-(1-p)^2
return(c(aa=aa,ab=ab,bb=bb))
}
HWE(0.5)

plot(1,1,type="n", xlim=c(0,1),ylim=c(0,1),xlab="freq. allele a",ylab="geno.freq")
p<-seq(from=0,to=1,by=0.01)
GenoFreq<-t(sapply(p,HWE))
lines(p,GenoFreq[,"aa"],lwd=2,col="red")
#If aa increases a allele increases, if a allele decreases the aa will decrease, niether time nore space is in this plot it is frequency of an allele and a genotypic frequency 

lines(p,GenoFreq[,"ab"],lwd=2,col="purple")
lines(p,GenoFreq[,"bb"],lwd=2,col="blue")
legend("top",legend=c("aa","ab","bb"),col=c("red","purple","blue"),lty=1,lwd=2,bty="n")

Pop<-simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500,pch=21,bg="red")
#It doesnt match Hardy Weinberg completely but is very close 
Pop<-simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50,pch=22,bg="red")
#The frequency of the aa genotype increased and fits hardy weinberg even less with a smaller population 

install.packages("learnPopGen")
library(learnPopGen)
x<-genetic.drift(Ne=200,nrep=5,pause=0.01)
x<-genetic.drift(Ne=800,nrep=5,pause=0.01)
x<-genetic.drift(Ne=5000,nrep=5,pause=0.09)
x<-genetic.drift(Ne=200,nrep=5,pause=0.01)

# run simulation
PopSizes <- 5:50
Samples <- rep(PopSizes,5)
tExt <- sapply(Samples,function(x) nrow(simPop(x,500)))

# fit line
Line <- lm(tExt ~ Samples)
summary( Line )
Line$coef

# make plot
plot(Samples,tExt)
abline(Line)

# fit alternate line
Line2<-lm(tExt ~ Samples+0)

#It means that extinction is closer to the HWE when there is a finite population size an as it increases it fits less. 
