Voltage<-c(1,2,3,4,5,6,7,8)
Current<-c(0.48,1,1.53,1.97,2.60,3.10,3.48,4.09)
 ### V=IR
Resistance<-Voltage/Current 

mean(Resistance)

setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Physics Resistance lab')

pdf("Finding Resisrance.pdf", height =4, width = 4)
plot(Voltage, Current, xlab="Voltage (V)", ylab="Current (A)", col="red")
abline(lm(Current~Voltage))
dev.off()
