library(phytools)
library(ape)
text.string <- "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree (text=text.string)
plot (vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
 ### Q.1. Shark is more closely related to the gold fish.
vert.tree
str (vert.tree)
 ### Q.2. There were no branch lenghts. 
tree <- read.tree (text="(((A,B), (C,D)), E);")
plotTree (tree, offset=1)
tiplabels (frame="circle", bg='lightblue', cex=1)
nodelabels (frame="circle", bg='white', cex=1)
tree$tip.label
 ### Edge Matrix gives us the starting and ending nodes. 
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par (las=1)
hist (AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge [,2] <= Ntip (AnolisTree))
tipEdges
Lengths <- AnolisTree$edge.length
Lengths
names (Lengths) <- AnolisTree$tip.label
names (Lengths) [which(Lengths==min(Lengths))]

plot (AnolisTree, cex=0.25)
Labs <- sapply (AnolisTree$edge.length, round, digits=2)
edgelabels (text=Labs, cex=0.25)
?plot.phylo
 ###Q.3.
tree <- read.tree(text='(((A,B), (C,D)), E);')
plot.phylo(tree, type='phylogram', show.tip.label=FALSE, edge.color='purple')
 ###Q.4.
plot.phylo(tree, type='radial')
 ###Q.5.
plot.phylo(tree, tip.color = 'red')
# Question 6: Find which living, named species has the shortest edge length.
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
which(Lengths == min(Lengths))
 ###Anolis occultis has the shortest edge length 
names(Lengths)
AnolisTree2 <- drop.tip(AnolisTree, 'Anolis_occultus')
plot(AnolisTree2, cex=0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)

 ###Q.9. The line is increasing exponentially. Indicating the species are reaching to fixation. The slope is always the same. The curve of the lizards shows the lizards are going towards fixation. 
fit.bd (AnolisTree, b, d, rho = 0.2)
 ###Q.10. b=0.8031, d=0, and log(L) = 132.9163  indicating Species are heading towards fixation.

  ### Extra Credit 
setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_07')

install.packages("treebase")
library("treebase")

Dolphins <- search_treebase("Delphinus", by='taxon', max_trees = 10)
WillWork <- sapply(Dolphins, function(x) try(is.ultrametric(x)))
which(WillWork == TRUE)
DolphinPlot <- plot.phylo(Dolphins[[9]], cex = 0.35)
is.ultrametric(Dolphins[[7]])
dolphin <- fit.bd(Dolphins[[7]], rho = 0.2)
dolphin


Tigers <- search_treebase("tigris", by='taxon', max_trees=3)
WillWork <- sapply(Tigers, function(x) try(is.ultrametric(x)))
TigersPlot <- plot.phylo(Tigers[[2]], cex=0.1)
bdtigers <- fit.bd(Tigers[[2]], rho = 0.2)


Warblers <- search_treebase("Basileuterus", by='taxon', max_trees=20)
length(Warblers)
WarblersPlot <- plot.phylo(Warblers[[1]], cex=0.35)
WillWork <- sapply(Warblers, function(x) try(is.ultrametric(x)))
bdwarblers <- fit.bd(Warblers[[1]], rho =0.2)


Elephants <- search_treebase('elephas', by='taxon', max_trees=25)
ElephantsPlot <- plot.phylo(Elephants[[12]], cex=0.35)
WillWork <- sapply(Elephants, function(x) try(is.ultrametric(x)))
which(WillWork == TRUE)
fit.bd(Elephants[[12]], rho = 0.2)

Capybara <- search_treebase('Hydrochoerus', by='taxon', max_trees=25)
length(Capybara)
CapybaraPlot <- plot.phylo(Capybara[[8]], cex=0.5)
WillWork <- sapply(Capybara, function(x) try(is.ultrametric(x)))
which(WillWork == TRUE)
fit.bd(Capybara[[8]], rho = 0.2)

Tegu <- search_treebase("salvator", by='taxon', max_trees=2)
WillWork <- sapply(Tegu, function(x) try(is.ultrametric(x)))
TeguPlot <- plot.phylo(Tegu[[1]], cex=0.4)
fit.bd(Tegu[[1]], rho = 0.2)

armadillo <- search_treebase("dasypus", by='taxon', max_trees=15)
ArmadilloPlot <- plot.phylo(armadillo[[1]], cex=0.35)
WillWork <- sapply(armadillo, function(x) try(is.ultrametric(x)))
fit.bd(armadillo[[1]], rho = 0.2)

killwhale <- search_treebase("orcinus", by='taxon', max_trees=10)
killwhalePlot <- plot.phylo(killwhale[[1]], cex=0.25)
WillWork <- sapply(killwhale, function(x) try(is.ultrametric(x)))
fit.bd(killwhale[[1]], rho = 0.2)

TreeFrog <- search_treebase("hyla", by='taxon', max_trees=2)
length(TreeFrog)
TreeFrogPlot <- plot.phylo(TreeFrog[[2]], cex=0.6)

WillWork <- sapply(TreeFrog, function(x) try(is.ultrametric(x)))
fit.bd(TreeFrog[[2]], rho = 0.2)
species9 <- TreeFrog[[2]]$tip

Bluebird <- search_treebase("Sialia", by='taxon', max_trees=10)
BluebirdPlot <- plot.phylo(Bluebird[[1]], cex=0.5)
bdbird <- fit.bd(Bluebird[[1]], rho = 0.2)

WillWork <- sapply(Bluebird, function(x) try(is.ultrametric(x)))
species10 <- Bluebird[[1]]$tip



install.packages("diversitree")
library("diversitree")
install.packages("TreeSim")
library("TreeSim")

bdModel <- make.bd(Bluebird[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
coef(fit)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(TreeFrog[[2]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(killwhale[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(armadillo[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(Tegu[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(Capybara[[8]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(Elephants[[12]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(Warblers[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(Tigers[[2]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

bdModel <- make.bd(Dolphins[[7]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)

