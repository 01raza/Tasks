setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_11')

x<-rnorm(100, mean = 5, sd=2)
var(x)
mean(x)
y<-((x*5)+2)+runif(100,0,0.1)
pdf('graph1.pdf', height = 4, width = 4)
plot(x,y)
abline(lm(y~x), col='orange')
dev.off()
coef(lm(y~x))
  ### (Intercept) y = 2.053240    x= 4.998827, It's not perfect 2 and 5 because the mean and variance was not exactly 2 and 5 respectively, furthermore there was also addition of small numbers. 
z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
  z[i] <- runif(1)
  y <- (x * z[i]) + 2 + (rnorm(100, 0:0.1))
  l <- coef(lm(z[1:100]~y))
}
pdf('graph2.pdf', height = 4, width = 4)
plot(z[1:100], y)
abline(lm(y~z[1:100]))
dev.off()

pdf('graph3.pdf', height = 4, width = 4)
plot(c(z, -0.029))
dev.off()



 ### MEME

install.packages("meme")
library('meme')
dir()
u <- 'https://lh3.googleusercontent.com/3X6FNXp5av39izZlAEQUUn6S5V_01wEmejYZj7BqvoIf6lVZNY40ThEGzWtqpegxUif3=s168'

png("meme.png", height = 6, width = 6)

if (.Platform$OS.type == "windows") {
  windowsFonts(
    Impact = windowsFont("Impact"),
    Courier = windowsFont("Courier")
  )
}
meme(u,"R is evolving too!!", "Species are not the only thing evolving", size=1.8, color='blue', vjust=1, bgcolor='white')
dev.off()
