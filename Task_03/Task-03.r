trueMean1<-5
trueSD1<-5
population1<-rnorm(1e6,trueMean1,trueSD1)
population1
?rnorm
trueMean2<-4
trueSD2<-5
population2<- rnorm(1e6,trueMean2,trueSD2)
Size<-50
Sample1<-sample(population1,Size)
Sample2<-sample(population2,Size)
Sample1
Sample2
Sample1 == Sample2
population1 == population2
boxplot(Sample1,Sample2)
boxplot(population1,population2)
  ### There is not a lot of difference in effective size between sample 1 and 2. Similarly there is not a  big effective size difference between population 1 and 2.
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma-mom")
MatGrandpa <- makeFounder("grandpa-mom")
PatGrandma <- makeFounder("grandma-da")
PatGrandpa <- makeFounder("grandpa-da")
head(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
head(PatGrandpa)
nrow(MatGrandma)
nrow(MatGrandpa)
nrow(PatGrandma)
nrow(PatGrandpa)
Alan<-makeBaby(PatGrandma,PatGrandpa)
Brenda<-makeBaby(MatGrandma,MatGrandpa)
Alan
Brenda
Focus<-makeBaby(Brenda,Alan)
head(Focus)
?grep
head(Focus)
head(Brenda)
grep(Brenda,Focus)
length(Focus)
nrow(Focus)
ncol(Focus)

  ### It should be 50%.

length(grep("mom",Focus))
ToMom<-length(grep("mom",Focus))/length(Focus)
ToMom

 ### These number can be anythig but the sum of all should be equal to 100 % (1).

ToMomMom<-length(grep("grandma-mom",Focus))/length(Focus)
ToMomDad<-length(grep("grandpa-mom",Focus))/length(Focus)
ToMomDad
ToMomMom
ToDad<-length(grep("da",Focus))/length(Focus)
ToDad
ToDadMom<-length(grep("grandma-da",Focus))/length(Focus)
ToDadDad<-length(grep("grandpa-da",Focus))/length(Focus)
ToDadDad
ToDadMom
ToDadDad+ToDadMom+ToMomDad+ToMomMom

 ### Focus is realated 50% to Pat. Grandparents and 50% to the Mat. Grandparents. Focus is not related to all grandparent equally, Focus is 5.5% related to DadsDad 44.5% related to Dad'sMom. 19% related to Mom'sDad and 31% related to Mom'sMom. This kind of distribution was expected due to crossing over and other genetic changes. 
  ### Average relatedness for all grand parents is 25%.

Sibling_01<-makeBaby(Brenda, Alan)
ToSib<- length(intersect(Focus,Sibling_01))/length(Focus)
ToSib
 ### Focus shared 20.2 % DNA with his sibling 1. Any amount of share was expected due to genetic mix ups.   
ManySiblings<-replicate(1e3,length(intersect(Focus,makeBaby(Brenda,Alan)))/length(Focus))
ManySiblings
quantile(ManySiblings)
?quantile
mean(ManySiblings)
 ### On Average focus shared 50.24% of DNA with 1000 siblings. 
plot(density(ManySiblings),main="",xlab="proportion shared genes")

  ### We saw a range of values because there is no fix value of DNA shared between focus and siblings and it is dependent on different factors such as crossing over, recombination etc. 

HWE<-function(p) {
  aa<-p^2
  ab<-2*p*(1-p)
  bb<-(1-p)^2
  return(c(aa=aa,ab=ab,bb=bb))
}
HWE(0.5)
plot(1,1,type="n",xlim=c(0,1),ylim=c(0,1),xlab="freq. allele a",ylab="geno. freq")
p<-seq(from=0,to=1,by=0.01)
GenoFreq<-t(sapply(p,HWE))
p
GenoFreq
lines(p,GenoFreq[,"aa"],lwd=2,col="red")


### the frequency of aa individuals is increasing as the frequency of the a allele increasing the population. Similarly, as freq. a decreases aa also decreases. Both time and geographic space is shown in the figure. 

lines(p,GenoFreq[,"ab"],lwd=2,col="purple")
lines(p,GenoFreq[,"bb"],lwd=2,col="blue")
legend ("top",legend=c("aa","ab","bb"),col=c("red","purple","blue"),lty=1,lwd=2,bty="n")
Pop<-simPop(500)
Pop
??simPop
Pop1<-simPop(4)
Pop1
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/500,pch=21,bg="red")
  ### Yes it matches. 
Pop<-simPop(50)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/50,pch=22,bg="red")
 ### When the population size is small fixation is reached, while in large sample size there is no fixation present. Because in smaller population fixation is reached early.
install.packages("learnPopGen")
library(learnPopGen)
x<-genetic.drift(Ne=200,nrep=5,pause=0.01)
x<-genetic.drift(Ne=20,nrep=5,pause=0.01)
x<-genetic.drift(Ne=2,nrep=5,pause=0.01)
x<-genetic.drift(Ne=50,nrep=5,pause=0.01)
x<-genetic.drift(Ne=10,nrep=5,pause=0.01)

  ## As the population size is decreasing the alleles are getting fixed in less time.
PopSizes<-5:50
PopSizes
Samples<-rep(PopSizes,5)
Samples
tExt<-sapply(Samples,function(x)nrow(simPop(x,500)))
tExt
Line<-lm(tExt~Samples)
Line
summary(Line)
Line$coef
plot(Samples,tExt)
abline(Line,col="red")
Line2<-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
plot(Samples,tExt)
abline(Line2,col="blue")

 ### +0 tells that for x=0 y is also 0 and estimates the slope b/w x and y. As sample is not linked to tExt the data without +0 is more suitable. 

 ### As the population size is increase the distance between the points and the size is also increasing. Indicating that extinction will take longer time with increase in population. 


### Extra Credit

install.packages("lmtest")
library(lmtest)
bptest(Line)
  ### p value is 0.0008692 which is less than 0.05 indicating there is heteroscedasticity.
install.packages("sandwich")
library(sandwich)
robust<-coeftest(Line, vcov=vcovHC)
 ### This gives the robust values of Est std, std. error, tvalue for the intercept and samples.

install.packages("robustbase")
library(robustbase)
robustLine<-lmrob(tExt~Samples)
setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_03')
pdf("Heteroskedasticity.pdf", height=8, width=8)
plot(Samples, tExt, main="samples vs tExt")
abline(Line, col="blue")
abline(robustLine, col="red")
legend ("top", legend=c("Regression Linear","Robust"),col=c("blue","red"),lty=1,lwd=2,bty="n")
dev.off()

  ### The slope for robust line is less as compared to the linear regression. 
