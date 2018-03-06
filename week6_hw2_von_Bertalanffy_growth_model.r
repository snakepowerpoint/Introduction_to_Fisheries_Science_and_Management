setwd('D:/MyGitHub/Introduction_to_Fisheries_Science_and_Management')

##### exercise 1
# see doc file


##### exercise 2
data = read.table("age_length.txt", header=T)

### 1
plot(Preanal.length.mm ~ mean, data=data, pch=19, xlab="Age", ylab="Length",
     main="Length-Age", xlim=c(0, 20), ylim=c(0, 400))

abline(lm(Preanal.length.mm ~ mean, data), col="red")
AIC(lm(Preanal.length.mm ~ mean, data))

### 2
summary(lm(Preanal.length.mm ~ mean, data))

coeff <- round(lm(Preanal.length.mm ~ mean, data)$coeff, 2)
matrix(c(rep(1, 20), 1:20), nrow=20)%*%matrix(coeff)

### 3
data1 = read.table("age_length_red_drum.txt", header=T)

plot(x=data1[1:14, 2], y=data1[2:15, 2], xlab="Lt", ylab="Lt+1", pch=19,
     main="Ford-Walford method", xlim=c(0, 1000), ylim=c(0, 1000))
abline(lm(data1[2:15, 2] ~ data1[1:14, 2]), col="red")
abline(a=0, b=1, col="blue")

coeff1 <- lm(data1[2:15, 2] ~ data1[1:14, 2])$coeff
K = -log(coeff1[2])  # 0.4597
Linf = coeff1[1]/(1-coeff1[2])  # 934.094

### 4
source("http://www.rforge.net/FSA/InstallFSA.R")

library(FSA)
library(NCStats)
library(nlstools)
library(sciplot)

vbStarts(Fork.length ~ Age,data=data1, type="original", dynamicPlot=TRUE)  # get Linf, K

svb = list(Linf = 934, K = 0.46, to = 0)
nl1 = nls(Fork.length ~ Linf*(1-exp(-K*(Age-to))), start=svb, data=data1)
summary(nl1)

Linf = coef(nl1)[1] 
K = coef(nl1)[2]
to = coef(nl1)[3]
s = 0:23

plot(Fork.length ~ Age, data=data1, pch=19, xlab="Age", ylab="Length",
     main="Lenght-Age")
lines(s, Linf*(1-exp(-K*(s-to))), type="l", lwd=2, xlim=c(0,23))

