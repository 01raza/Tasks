Voltage<-c(1,2,3,4,5,6,7,8,10,15,20)
Current<-c(0.48,1,1.53,1.97,2.60,3.10,3.48,4.09, 5.20, 7.75,10.20)
### P=VI
Power<-Voltage*Current 


setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Physics Power lab')

pdf("Power vs Voltage Relation.pdf", height =4, width = 4)
plot(Voltage, Power, xlab="Voltage (V)", ylab="Power (W)", col="purple")
 ### Exponential relation observed. 
dev.off()

pdf("Power vs Current Relation.pdf", height =4, width = 4)
plot(Current, Power, xlab="Current (A)", ylab="Power (W)", col="Orange")
### Exponential relation observed. 
dev.off()
