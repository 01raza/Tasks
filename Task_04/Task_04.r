 ### Most fit allele always don't go to fixation. 
source("http://jonsmitchell.com/code/fxn05.R")
Pop1<-simPop(Popsize=50, nGenerations=100,initial_p=0.5,h =1,s=0)
test<-simPop(Popsize=10, nGenerations=10,initial_p=0.1,h =1,s=0)
test
plot(1:nrow(Pop1),Pop1[,1],ylim=c(0,1), type ="l" , xlab="generation", ylab="allele freq.",lwd=2)
lines(1:nrow(Pop1),Pop1[,2],lwd=2, col="red" )
legend ("topleft", legend = c ("a","b"),col= c ( "black" , "red") , lwd =2 , bty="n")
plotFit(nruns=3, n=50,ngens=500,init_p=0.5,h=1000,s=1)
Expectation<-c(10,10,10,10)
Observed<-c(15,15,5,5)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
Chisq
barplot(rbind(Expectation,Observed),beside=T,main=bquote(chi^2~"="~.(Chisq)),legend.text=c("expected","observed"))

Expectation1<-c(2,3,10,30)
Observed1<-c(5,0,0,35)
Chisq1<-sum(((Expectation1-Observed1)^2)/Expectation1)
Chisq1
barplot(rbind(Expectation1,Observed1),beside=T,main=bquote(chi^2~"="~.(Chisq1)),legend.text=c("expected1","observed1"))
Expectation1<-c(2,3,10,30)
Observed1<-c(10,10,10,10)
Chisq1<-sum(((Expectation1-Observed1)^2)/Expectation1)
Chisq1
 ### Chisquare value is 61.67 with all observations at 10.
Expectation1<-c(2,3,10,30)
Observed1<-c(40,0,0,0)
Chisq1<-sum(((Expectation1-Observed1)^2)/Expectation1)
Chisq1
 ### Chisquare moved further away from zero and is 765.
 ### As the chisquare value is increasing the expected and observed bars evenness is decreasing.
setwd("C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_04")
results<-read.csv("http://jonsmitchell.com/data/biol112labresults.csv",stringsAsFactors=F)
write.csv(results,"Biol112results.csv",quote=F)
counts<-results[,c("yellow","red","green","blue","black","tan")]
counts
backgrounds<-c("White","Red","Yellow","Green","Blue","Black")
backgrounds
backgroundCol<-c("white","#d53e4f","#fee08b","#abdda4","#3288bd","black")
backgroundCol
calcChi(counts[1,])
counts[1,]
Chisqs<-apply(counts,1,calcChi)
plotChis(counts)
 ### The higher the chi square value is the lower the evenness between the bars. At low chi-square values bars are even. the higher the chi-square value is the more observed values are far away from the expected values. 
Avg<-mean(Chisqs)
Avg
 ### The average chi square value is 60.9981
 ### The graphs with high chi-square values have uneven bars and far from our expected results. Graphs with low and zero chi-square values have even bars and they are close to our expected results. 
 ### Avg. chi square is larger than the critical value so we can say that difference between observed and expected values are clear.
backgroundAvgs<-tapply(Chisqs,results[,3],mean)
backgroundAvgs
  ### The values are different for different backgrounds, as there is different selction on sticks on each background. 
propSig<-length(which(Chisqs>11.70))/length(Chisqs)
percSig<-round(100*propSig)
propSig
percSig
 ### 92% is a big number and surprised was expecting around 40-50%.
 ### Due to the high number it is suspected there are other factors than natural selection. There can be biasness from the predators.  
par(las=1, mar = c(4,4,1,1),mgp = c(2,0.5,0),tck=-0.01,cex.axis=1)
hist(Chisqs ,main="",xlab=" chi-squared values",ylab="frequency")
par(las=1,mar=c(4,4,1,1),mgp=c(2,0.5,0),tck=-0.01,cex.axis=1)
plot(1,1,xlim=c(0,400),ylim=c(1,8.5),xlab="",ylab="",type="n",yaxt="n" )
axis(2,at=1:length(backgrounds),labels=backgrounds)
mtext(side=1,expression(chi^2),cex=1.75,line=2.5)
counter<- 1
for ( i in backgrounds) {
  Data<-Chisqs[which(results[,3] == i)]
  addHist(Y=counter,Dat=Data,Color=backgroundCol[counter])
  counter<- counter+1
}
abline(v=11.70,lty=2,lwd=2,col="black")
 ### Not a lot of difference from visual perspective.
 ### selection-free simulation produced final count different because in the initial count there was based on the selection due to camoflauge.
names(Fit)<-1:6
Simulation2<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation2,Color=rgb(0,0,0,0.25))
Fit<-c(0.1,1,1,1,1,1)
names(Fit)<-1:6
Simulation3<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation3,Color=rgb(0,0,0,0.25))
Fit<-c(0.5,0.6,0.7,1,1,1)
names(Fit)<-1:6
Simulation4<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation4,Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit)<-1:6
Simulation5<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation5,Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.1,0.1,0.1,0.1,1)
names(Fit)<-1:6
Simulation6<-simDraws(1e4,w=Fit)
addHist(Y=8,Dat=Simulation6,Color=rgb(0,0,0,0.25))
mtext(side = 2, at=8, line=0, "sel.sim.")
Simulation7<-c(Simulation2,Simulation3,Simulation4,Simulation5,Simulation6)
addHist(Y=8,Dat=Simulation7,Color=rgb(0,0,1,0.25))
  
### The mixture created is very similar to the student's data.
 ### There is no strong selection in the student's group. There was natural selection involved in picking up the certain colors and certain colors were ignored. 
 ### The evolutionary process with the stimulation is Natural selection.
 ### Stimulation is based on the data and was only factor involved was natural selection, while for the student's data i believe there were other factors involved.
  ### Mutations will  increase the chi-square values as we will observe mutations which were not expected which will increase the chi-square value. 


 ### Extra Credit 

