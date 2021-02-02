setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_02')
Data <− read.csv("http://jonsmitchell.com/data/beren.csv",stringsAsFactors=F)
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
Feeds<−which(Data[,9]=="bottle")
berenMilk <− Data[Feeds,]
head(berenMilk)
nrow(berenMilk)
print(berenMilk)
	### There are 323 rows in the berenMilk and reach row present the bottle event. 
Feeds<−which(Data[,"event"]=="bottle")
berenMilk1<−Data[Feeds,]
Feeds<−which(Data$event=="bottle")
berenMilk2<−Data[Feeds,]
 ### Is all the 3 ways generated same results ?
berenMilk == berenMilk1 
berenMilk1 == berenMilk2
berenMilk == berenMilk2
 ### I get TRUE for all three so i'm convinced that they are same. 
dayID<−apply(Data,1,function(x)paste(x[1:3],collapse="-"))
dateID<−sapply(dayID,as.Date,format="%Y−%m−%d",origin="2019−04−18")
Data$age<−dateID−dateID[which(Data$event=="birth")]
head(Data)
beren2 <− Data
beren3 <− beren2[order(beren2$age),]
head(Data)
head(beren2)
head(beren3)
write.csv(beren3,"berennew.csv",quote=F,row.names=FALSE)