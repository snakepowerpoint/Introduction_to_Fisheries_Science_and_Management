setwd('D:/MyGitHub/Introduction_to_Fisheries_Science_and_Management')

##### exercise 1
data1 <- data.frame(Catch=rep(0,5), N=c(10000,0,0,0,0), 
                    F=c(0.45,0.45,0.6,0.6,0.6), M=rep(0.2,5), 
                    Survival=rep(0,5))

# total mortality Z=F+M
data1$Z = data1$F + data1$M

# A=1-exp(-z)
data1$A = 1-exp(-data1$Z)

# fill survival and remaining population in each year
# by using exponential decay model
for (i in 1:5) {
  data1[i, 5] = data1[i, 2]*exp(-data1[i, 6])
  data1[(i+1), 2] = data1[i, 5]
}

data1 = data1[-6, ]

# compute catch by using baranov's catch equation
data1$Catch = with(data1, N*A*F/Z)

data1$Catch
data1$N



##### exercise 2
data2 <- read.table("q2.txt", header=T)

# apply the relationship between Z and effort
fit2 <- lm(Z~Effort, data=data2)
summary(fit2)

plot(Z~Effort, type="b", pch=19, data=data2)
abline(fit2, col="red")

# M
fit2$coeff[1]

# q
fit2$coeff[2]



##### exercise 3
data3 <- read.table("q3.txt", header=T)

# age based catch curve (exponential decay model)
fit3 <- lm(log(data3[3:7, 2])~data3[3:7,1])
summary(fit3)

# total mortality Z=-slope
-fit3$coeff[2]
plot(log(data3[3:7, 2])~data3[3:7,1])

library(FSA)
library(NCStats)
library(nlstools)
library(sciplot)
library(relax)

vbStarts(Length~Age, data=data3, type="original", 
         dynamicPlot=TRUE)  
# get Linf=750, K=0.0972

# ln(M)=0.55-1.61*ln(L)+1.44*ln(Linf)+ln(K)
data3$M = with(data3, exp(0.55-1.61*log(Length)+1.44*log(750)+log(0.0972)))
mean(data3$M)



##### exercise 4
# number of age 3 individuals survived to age 4
exp(log(3000000)-1.1664)

# number of catch (it's wrong because the number of dead fish is not 
# equal to the combination of that due to natural mortality and that
# due to fishing mortality)
3000000*(1-exp(-0.9432))

3000000*(1-exp(-1.1664)) == 3000000*(1-exp(-0.9432))+3000000*(1-exp(-0.2232))



##### exercise 5
data4 <- read.table("heter2006.txt", header=T)
data5 <- read.table("otolith.txt", header=T)

plot(INCREMENT~LENGTH, data=data5)

fit5 <- lm(INCREMENT~LENGTH, data=data5)
summary(fit5)
abline(fit5, col="red")

# predicted value
predict(object=fit5, newdata=data.frame(LENGTH=data4$length), interval="prediction")[1]
# or
coef(fit5)[1] + coef(fit5)[2]*data4$length[1]  # the same

# predict age
data4$age = predict(fit5, data.frame(LENGTH=data4$length))
data4$age = round(data4$age)

data4.1 <- data.frame(table(data4$age))
colnames(data4.1) = c("age", "N")
data4.1$age <- as.integer(rownames(table(data4$age)))

plot(log(N)~age, data=data4.1, pch=19)
which.max(data4.1$N)  # 14

fit5.1 <- lm(log(data4.1[-c(1:13), 2])~data4.1[-c(1:13), 1])
# total daily mortality
coef(fit5.1)[2]  # 0.1251
