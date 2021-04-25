install.packages('diversitree')
library('diversitree')
transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_0 <- 0.2
extinction_0 <- 0.15

speciation_1 <- 0.4
extinction_1 <- 0.1	

maxN <- 1e3
maxT <- 50

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)

simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
#?tree.bisse()

stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)

setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_10')


 ###Q.1. There was an inverse relation observed between the state 1 frequency and net diversification. According to the plots the state 1 frequency will be higher as compared to state 0 even at very low net diversification.


Frequencies <- c('State 0', 'State 1')
Data <- matrix(c(0.68, 0.69, 0.57, 0.647, 0.642, 0.43, 0.32, 0.3, 0.43, 0.35, 0.35, 0.568), nrow = 2, ncol = 6, byrow=TRUE)
Data
Difference <- c(0.15, 0.1, 0.05, 0.03, 0.02, 0.01)
Freq1 <- c(0.32, 0.3, 0.43, 0.35, 0.35, 0.568)
Freq0 <- c(0.68, 0.69, 0.57, 0.647, 0.642, 0.43)

pdf('Q1.pdf', height =6, width=6)
barplot(Data, names.arg=Difference,main = 'Changes in Frequency of States based on Variation in R Values',xlab = 'Difference in Diversification Rate',ylab = 'Frequency',beside=TRUE,col = c('red', 'black'))
legend('top', Frequencies, fill = 'red', 'black')
dev.off()


 ### Q.2. State 1 was never zero. When the net diversification of state 0 was increased, the state 1 was close to zero but was never zero. 

Frequencies <- c('State 0', 'State 1')
Data <- matrix(c(0.82, 0.8, 0.96, 0.85, 0.63, 0.9, 0.926, 0.923, 0.959, 0.955, 0.945, 0.968, 0.977, 0.963, 0.978, 0.984, 0.973, 0.18, 0.2, 0.04, 0.14, 0.37, 0.088, 0.074, 0.077, 0.041, 0.045, 0.055, 0.032, 0.023, 0.037, 0.022, 0.016, 0.027), nrow = 2, ncol = 17, byrow=TRUE)
Difference <- c(0.05, 0.05, 0, 0, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.45, 0.45, 0.45)
pdf('Q2.pdf', height = 8, width = 8)
barplot(Data, names.arg=Difference, main='How Close to Zero State 1 Gets When Transition Rate is Nonzero',xlab='Difference in Diversification Rate',ylab='Frequencies',col=c('orange', 'green'))
legend('top', Frequencies, fill = 'orange', 'green')
dev.off()


 ###Q.3. A small variation was observed when the parameters were same for state 0 and state 1 but the net diversification rate was changing. 

Data <- read.csv('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_10\\q3_data.csv',stringsAsFactors=F)
head(Data)
Freq1_Trial1 <- Data[,2]
Freq1_Trial2 <- Data[,5]
Freq1_Trial3 <- Data[,8]
Var1 <- var(Freq1_Trial1)
Var2 <- var(Freq1_Trial2)
Var3 <- var(Freq1_Trial3)

VarianceMatrix <- c(Var1, Var2, Var3)
VarianceMatrix
Trial <- c(1, 2, 3)
pdf('Q3.pdf', height=8, width=8)
barplot(VarianceMatrix, names.arg=Trial,main='Variance of Frequency (Each Trial)',ylim= c(0, 0.5),xlab='Trial Number',ylab='Variance in Frequencies',col='blue')
dev.off()

 ###Q.4. Factors such as Selection, Drift, and inbredding can play a huge role in altering the frequency of state 1. 

 ### Some Trends 

 ### Effects on frequency when net diversification rate is increased. 

Data <- read.csv('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_10\\Trend.csv',stringsAsFactors=F)
head(Data)
Freq_0 <- Data[,2]
Freq_0
NDR_0 <- Data[,1]
pdf('Trend1.pdf', height=8, width=8)
plot(NDR_0, Freq_0, xlab='Net Diversification Rate of State 0', ylab='Frequency of State 0', main='How Net Diversification Rate Influences Frequency')
abline(lm(Freq_0~NDR_0), col='pink')
dev.off()
Freq_1 <- Data[,7]
NDR_1 <- Data[,5]

pdf('Trend2.pdf', height=8, width=8)
plot(NDR_1, Freq_1, xlab='Net Diversification Rate of State 1', ylab='Frequency of State 1', main='How Net Diversification Rate Influences Frequency')
abline(lm(Freq_1~NDR_1), col='red')
dev.off()
