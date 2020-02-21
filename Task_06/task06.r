setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Task_06")
install.packages("learnPopGen")
library(learnPopGen)
?coalescent.plot()
coalescent.plot()

pdf("R06-coalescent1.pdf", height=4,width=4)
coalescent.plot(n=10,ngen=30)
dev.off()

pdf("R06-coalescent2.pdf", height=4,width=4)
coalescent.plot(n=10,ngen=30)
dev.off()

pdf("R06-coalescent3.pdf", height=4,width=4)
coalescent.plot(n=10,ngen=30)
dev.off()	

#1 = Each simulation begins with a set number of individuals, when you give it n= some number it changes the alleles you start with

#2 = my average number of generations it took to for a said allele to go to fixation was 20 generations 

#3 = Each individual has one offspring with the varience being (+/- 1 individual)

#4 = It doesnt play a role in this simulation because the only thing acting on the population is drift

#5 = Yes the focal locus can be but probably isnt in generation zero

install.packages("coala")
library(coala)

install.packages("phytools")
library(phytools)

model<-coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) + feat_mutation(10) + feat_recombination(10) +sumstat_trees() + sumstat_nucleotide_div()

stats<-simulate(model, nsim = 1)
Diversity<-stats$pi
Diversity
#All numbers in Diversity are not the same, this can be caused by 

Nloci<-length(stats$trees)
t1<-read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#6 = because the individuals are diploid so there will be double the number of individuals we test due to the fact inviduals each have two loci_length

Age1<-max(nodeHeights(t1))

t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
#~0.43 is the most recent common ancestor for this SNP, with the first SNP most recent common ancestor being ~0.65

par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
#7 = The plots do not match

compare.chronograms(t1,t2)

t1_1<-read.tree(text=stats$trees[[1]][1])
t1_2<-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1,t1_2)

for(locus in 1:Nloci) {
ntrees<-length(stats$trees[[locus]])
for (n in 1:ntrees) {
if (locus ==1 && n ==1) {
outPhy<-read.tree(text=stats$trees[[locus]][n])
}
else {
outPhy<-ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
}
}
}

par(mfrow=c(1,1))
densityTree(outPhy)
outPhy1 <- outPhy

model_2<-coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) + feat_mutation(100) + feat_recombination(100) +sumstat_trees() + 
sumstat_nucleotide_div()
# My prediction for this plot is that it will differ heavily due to the increased mutation events in the locus. 

stats<-simulate(model_2, nsim = 1)
Diversity2<-stats$pi

Nloci<-length(stats$trees)
for(locus in 1:Nloci) {
	ntrees<-length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus ==1 && n ==1) {
			outPhy<-read.tree(text=stats$trees[[locus]][n])
		}
		else {
			outPhy<-ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
		}
	}
}
outPhy2 <- outPhy
par(mfrow=c(1,2))
densityTree(outPhy1)
densityTree(outPhy2)

model_2<-coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) + feat_mutation(100) + feat_recombination(1000) +sumstat_trees() + 
sumstat_nucleotide_div()
# My prediction for this plot is that it will differ heavily due to the increased mutation events in the locus. 

stats<-simulate(model_2, nsim = 1)
Diversity2<-stats$pi

Nloci<-length(stats$trees)
for(locus in 1:Nloci) {
	ntrees<-length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus ==1 && n ==1) {
			outPhy<-read.tree(text=stats$trees[[locus]][n])
		}
		else {
			outPhy<-ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
		}
	}
}
outPhy2 <- outPhy
par(mfrow=c(1,2))
densityTree(outPhy1)
densityTree(outPhy2)

model3<- coal_model(10,50) +
	feat_mutation(par_prior("theta", sample.int(100,1)))+
	sumstat_nucleotide_div()
stats<- simulate(model3, nsim = 40)

mean_pi<- sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars[["theta"]])

plot(mean_pi, theta, pch=16, cex=1.3)
lm(mean_pi~theta)
abline(lm(mean_pi~theta))



