setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Task_08")
library(phytools)
library(ape)

text.string<-"(((((((cow, pig), whatle), (bat,(lemur,human))),(robin, iguana)), coelacanth),(gold_fish, trout)),shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)

nodelabels(frame="circle", bg='white', cex=1)
#Q1 A goldfish is more closely related to a shark in the diagram. 

vert.tree
#Q2 There are no branch lengths

str(vert.tree)

tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)

tree$tip.label

tree$edge

AnolisTree<- force.ultrametric(read.tree("http://jonsmitchell.com/data/anolis.tre"))

par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0,6))

tipEdges<- which(AnolisTree$edge[,2]<= Ntip(AnolisTree))
Lengths<- AnolisTree$edge.length
names(Lengths)<- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs<- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

?plot.phylo

#Q3 show.tip.label=FALSE
#Q4 rotate.tree="fan"
#Q5 tip.color='red'

plot(AnolisTree, type="fan", tip.color='red', show.tip.label=FALSE, cex=0.25)
plot(AnolisTree, type="fan", tip.color='red', cex=0.25)





ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)

#Q9 I notice that the line increases very rapidly and stabilizes when we get close to the end, it never goes down, because are always increasing but dont increase at constant rates. 

#Q10 0.8031

fit.bd(AnolisTree, rho = 0.2)