library(phytools)
    ### Question 1-3:
setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_09')
trees<-vector("list",1)
births<-c()
fractions<-c()
r<-c()
s<-c()
branch<-c()
?pbtree

for(i in 1:100) {
  births[i]<- runif(1)
  fractions[i]<- runif(1)
  trees[[i]] <- pbtree(b=births[i], d=(fractions[i]*births[i]),n=100)
  r[[i]]<- (births[i]- (fractions[i]*births[i]))
  s[[i]]<-births[i]
  branch[[i]]<-mean(trees[[i]]$edge.length)
}

str(trees)
pdf("q1-3_ i'th tree plot",height=4,width=4)
plot(trees[[i]])
dev.off()
trees[[100]]$tip.label
?sapply
sapply(trees, Ntip)

 ### Question 4 
tips<-log(sapply(trees, Ntip))
tips
r1<-unlist(r)
pdf("q4_ log tips vs diversification",height=4,width=4)
plot(tips,r1, xlab="log of number of tips" , ylab= "diversification", col="orange", pch=18)
abline(lm(tips ~ r1))
dev.off()
cor(tips,r1)
  ### 0.21 indicating a positive relation between log of # of tips and diversification rate. 
 
  ### Question 5
s1<-unlist(s)
branch <-unlist(branch)
pdf("q5_ Avg Branch Length vs speciation rate",height=4,width=4)
plot(branch,s1, xlab="Average branch length" , ylab= "speciaiton rate", col="orange", pch=1)
abline(lm(branch ~ s1))
dev.off()

  ### Question 6
cor(branch,s1)
  ### -0.50 indicating a negative relationship between branch length and speciation rate

  ### Question 7

Tree<-trees[[which.max(tips)]]

pdf("q7_ largest tree",height=4,width=4)
plot(Tree)
dev.off()

rates<-c()
traits<-vector("list",1)
meantraits<-c()
vartraits<-c()
for(i in 1:100) {
  rates[i]<- runif(1)
  traits[[i]]<-fastBM(tree = Tree, sig2 = rates[i])
  meantraits[[i]]<-mean(traits[[i]])
  vartraits[[i]]<-var(traits[[i]])
}

  ### Question 8
meantraits<-unlist(meantraits)

pdf("q8_ meantraits vs rates",height=4,width=4)
plot(meantraits, rates)
dev.off()
cor(meantraits, rates)
  ### 0.052, indicating a slightly postive relationship. 

 ### Question 9 
vartraits<-unlist(vartraits)

pdf("q9_ variance of traits vs rates",height=4,width=4)
plot(vartraits, rates)
dev.off()

cor(vartraits, rates)
 ### 0.67, indicating a positive relationship
 ### Question 10
cor(traits[[1]], traits[[2]])
 ### 0.639 positive relation b/w trait 1 and 2, yes it is very significant. 
pdf("q10_ trait 1 vs trait 2",height=4,width=4)
plot(traits[[1]], traits[[2]])
dev.off()

traitMat<-cbind(traits[[1]], traits[[2]])

 ### Extra Credit 

?phylomorphospace
pdf("Extra Credit plot",height=20,width=20)
phylomorphospace(Tree, traitMat, xlab= "trait 1", ylab= "trait 2")
dev.off()
