setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_08')
library('phytools')
library('ape')
library('maps')
tree <- read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type='fan')
tree$tip.label
tree$edge.length
 ### Question 1: There are 82 tips and branch length are present (162)

data <- read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
data
 ### Question 2:
class(data)
str(data)
typeof(data)
 ### "data.frame", It is list Data which represent 82 species of lizard and their snout-vent lengths.

dim(data)
 ### Dimension -> 82 rows  1 column 

svl <- setNames(data$svl, rownames(data))
typeof(svl)

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
 ###Quesion 3: ace is a list containing ancestral state estimate. CI95 is the 95 % confidence interval. 

 ###Quesion 4:"state computed for the root node of the tree during Felsenstein's contrasts algorithm is also the MLE of the root nodethe function re-roots the tree at all internal nodes and computes the contrasts state at the root each time. The function can also (optionally) compute variances or 95-percent confidence intervals on the estimates." (Revell 2012)

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type='fan', lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj <- contMap(tree, svl, plot=F)
plot(obj, type='fan', legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))


fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c('Anolis_aliniger', 'Anolis_aliniger', 'Anolis_occultus', 'Anolis_ricordii', 'Anolis_cristatellus', 'Anolis_occultus' ), tip2=c('Anolis_chlorocyanus', 'Anolis_coelestinus', 'Anolis_hendersoni', 'Anolis_cybotes', 'Anolis_angusticeps', 'Anolis_angusticeps'))
fossilData

?fastAnc

 ### Question 5:
fossilNodes<-c()
nodeN<-c()
for (i in 1:nrow(fossilData)) {
  Node <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
  fossilNodes[i] <- fossilData[i , "svl"]
  nodeN[i] <- Node
}

 ### Corrected the loop and uploaded again on 04/03/2021.

?fastMRCA

Node <- fastMRCA(tree, fossilData[i, 'tip1'], fossilData[i, 'tip2'])
Node
fossilNodes[i] <- fossilData[i, 'svl']
fossilNodes[i]
nodeN[i] <- Node
names(fossilNodes) <- nodeN

Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes[i], CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_withoutFossils
Ancestors_withFossils$ace
Ancestors_withoutFossils$ace
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace,xlab='with fossils',ylab='without fossils', pch=12, cex=1, col='orange')
 ##Question 7: Data with fossils increase with the estimated ancestral sizes. 

 ### Questions -> 8-10
install.packages('geiger')
library('geiger')
?fitContinuous
fitContinuous(tree, svl, model="BM")
  ### AIC = -6.512019
fitContinuous(tree, svl, model="OU")
  ### 	AIC = -4.517014
fitContinuous(tree, svl, model="EB") 
  ### 	AIC = -7.235181
fitContinuous(tree, svl, model="kappa") 
  ### 	AIC = -4.512019
fitContinuous(tree, svl, model="rate_trend") 
  ### 	AIC = -6.981431

 ###Lowest AIC presents the best fit model. So, EB is the best model from above.

?fastAnc
#fastAnc uses BM to determine what it assumes, and in fitContinuous we can use different types of models and find the best model (Lowest AIC presents the best fit model). 