len<-c(0.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5)
pi<-3.14
g<-9.8
time<-(2*pi)*(len/g)^0.5
time
  
setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Physics Pendulum lab')

pdf("Length vs time (Pendulum).pdf", height =4, width = 4)
plot(len, time, xlab="Length (m)", ylab="Time (s)", col="red")
### Exponential relation observed. 
dev.off()
