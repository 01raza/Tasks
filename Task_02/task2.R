setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_02')
Data <- read.csv("http://jonsmitchell.com/data/beren.csv",stringsAsFactors=F)
write.csv(Data,"rawdata.csv",quote=F)
length(Data)
nrow(Data)
ncol(Data)
print(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3,4]
Data[1:5, 1:3]
Data[257, 1:3]
Feeds<-which(Data[,9]=="bottle")
berenMilk <- Data[Feeds,]
head(berenMilk)
nrow(berenMilk)
print(berenMilk)
	### There are 323 rows in the berenMilk and reach row present the bottle event. 
Feeds<-which(Data[,"event"]=="bottle")
berenMilk1<-Data[Feeds,]
Feeds<-which(Data$event=="bottle")
berenMilk2<-Data[Feeds,]
 ### Is all the 3 ways generated same results ?
berenMilk == berenMilk1 
berenMilk1 == berenMilk2
berenMilk == berenMilk2
 ### I get TRUE for all three so i'm convinced that they are same. 
dayID<- apply(Data,1,function(x)paste(x[1:3],collapse="-"))
dateID<- sapply(dayID,as.Date,format="%Y-%m-%d",origin="2019-04-18")
Data$age<- dateID - dateID[which(Data$event=="birth")]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
head(Data)
head(beren2)
head(beren3)
write.csv(beren3,"berennew.csv",quote=F,row.names=FALSE)

### Task 2b

 
Feeds<-which(beren3$event=="bottle")
avgMilk<-mean(beren3$value[Feeds])
print(avgMilk)

 ### What are the units for this avgMilk value ?
  ### oz
### Why did I u see the value column from the beren3 object ? What is that?
	### It is the amount of milk given. 

### What does the s e t o f s q u a r e b r a c k e t s with  Feeds i n s i d e i t do h e r e ? I s i t imp o r t an t ? Why?
  ###  It is important because it will instruct R to get mean  of the value of rows in the object feed. 

avgFeed <-tapply(beren3$value[Feeds],beren3$age[Feeds],mean)
avgFeed
varFeed<-tapply(beren3$value[Feeds],beren3$age[Feeds],var)
varFeed
totalFeed<-tapply(beren3$value[Feeds],beren3$age[Feeds],sum)
totalFeed
numFeeds<-tapply(beren3$value[Feeds],beren3$age[Feeds],length)
numFeeds
?cor
cor(beren3$value[Feeds],beren3$age[Feeds])
cor.test(beren3$value[Feeds],beren3$age[Feeds])
berenCor<-cor.test(beren3$value[Feeds],beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds]~beren3$age[Feeds])
berenANOVA
boxplot(beren3$value[Feeds]~beren3$caregiver[Feeds],xlab="who gave the bottle",ylab="amount of milk consumed (oz)")
?par
par (las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck =-0.01)
plot(as.numeric(names(totalFeed)),totalFeed,type="b",pch=16,xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed),lty=2,col="red")
pdf("r02b-totalMilkByDay.pdf",height=4,width=4)
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalFeed)),totalFeed,type="b",pch=16,xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed),lty=2,col="red")
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf",height=8,width=8)
source("http://jonsmitchell.com/code/plotFxn02b.R")
dev.off()
unique(beren3$event)


###extra credit 

####
Naps <- which(beren3$event=="nap")
beren4 <- beren3[Naps,]
startHour<-(beren4$start_hour)
startMin<-(beren4$start_minute)
stopHour<-(beren4$end_hour)
stopMin<-(beren4$end_minute)
startHour
startMin
stopMin
stopHour
beren4$sleepTime<-((stopHour-startHour)*60)+(stopMin-startMin)
beren4
totalNap<-tapply(beren4$sleepTime,beren4$age,sum)
totalNap
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalNap)),totalNap,type="b",pch=16,xlab="age in days",ylab="Nap time in minutes")
cor.test(beren4$start_hour,beren4$sleepTime)
 ### There is a negative correlation between the Nap time and the time it is started.

  ### Question 1:
      ###Hypothesis 1 : The amount of food is unknown, there are no units of how much food was consumed. 
      ###Hypothesis 2 is incomplete because it only says a relationship and does not specify the type of relationship. 
  ### Question 2: The graph is hard to interpret because there is no specific trend in the dots it's going zig zag. 



 ### Part 2c 


### Hypothesis: the total sum of milk consumed in a day is positively correlated with the total number of wet diapers in a day




wet <- which(beren3$event=="wet")
beren3$NumberOfWets <- beren3$age/beren3$age
berenWets <- beren3[wet,]
totalWet<-tapply(beren3$NumberOfWets[wet],beren3$age[wet],sum)
totalWet

Milk<-which(beren3$event=="bottle")
totalMilk<-tapply(beren3$value[Milk],beren3$age[Milk],sum)
berenMilk <- beren3[Milk,]
totalMilk
berenMilk 
daysInCommon <- intersect(berenMilk$age, berenWets$age)
daysInCommon

totalWet
totalMilk
daysInCommon
totalWet[as.character(daysInCommon)]
totalMilk[as.character(daysInCommon)]
pdf("2c-graph.pdf",height=10,width=10)
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(totalMilk[as.character(daysInCommon)],totalWet[as.character(daysInCommon)],type="p",pch=20,main = "Number of Wets vs the consumption of milk",xlab="Milk consumed each day (oz)",ylab="Number of Wets each day")
dev.off()
cor.test(totalMilk[as.character(daysInCommon)],totalWet[as.character(daysInCommon)])

 ### Results showed a positive correlation b/w milk consumed in a day and number of wets each day. 

