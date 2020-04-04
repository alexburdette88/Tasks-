setwd("C:\\Users\\burde\\Desktop\\Evolution\\Tasks\\Task_09")
library(phytools)

tree <- read.tree("http://www.phytools.org//Cordoba2017//data//Anolis.tre")
plot(tree, type="fan")

tiplabels(frame="circle", bg='lightblue', cex=1)
tree$tip.label
head(tree)
edgelabels(tree$edge.length, bg="black", col="white", font=2)
#Q1=Their are 100 tips and the branches are present, 198 branch length

data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)

head(data)
dim(data)
#Q2=Data is an object weith dimenison of species with its snoutvent length

svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)

#Q3=Theyre stored in the ace element, CI95 is the 95 percent confidence interval.  

?fastAnc
#Q4=the two assumptions art the estimation of uncertainity around ancestral state is small and large

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("aliniger", "aliniger", "occultus", "christophei", "cristatellus", "occultus"), tip2=c("chlorocyanus", "coelestinus", "monticola", "cybotes", "angusticeps", "angusticeps"))


#Q5
fossilNodes <- c()
nodeN <- c()

Node <- fastMRCA(tree, for (variable in vector) {
  
fossilData[i, "tip1"], fossilData[i in, "tip2"])
}
fossilNodes[i] <- fossilData[i in "svl")]
nodeN[i] <- Node
