setwd('D:/MyGitHub/Introduction_to_Fisheries_Science_and_Management')

##### exercise 1, Maturation patterns
### (1) maturity vs. age
library(faraway)

SB <- read.table("SBay.txt", header=T, na.strings="*")
# transform maturity into 0 and 1
SB$maturity <- ifelse(SB$maturity==2, 1, 0)

# subset the data
male <- subset(SB, sex==1)
female <- subset(SB, sex==2)

plot(maturity~age, data=male, pch=2, col="blue", xlim=range(SB$age, na.rm=T))
points(x=female$age+0.2, y=female$maturity, pch=3, col="red")

fit1 <- glm(maturity~age, family=binomial, data=male)
fit2 <- glm(maturity~age, family=binomial, data=female)

s = seq(-1, 13, by=0.1)
lines(s, ilogit(fit1$coef[1]+fit1$coef[2]*s), col="blue", lwd=2, lty=1)
lines(s, ilogit(fit2$coef[1]+fit2$coef[2]*s), col="red", lwd=2, lty=2)
legend(x=10, y=0.8, lwd=2, legend=c("male","female"), col=c("blue","red"),
       lty=c(1,2))

-fit1$coef[1]/fit1$coef[2]  # A50 for male
-fit2$coef[1]/fit2$coef[2]  # A50 for female
# male individuals mature earlier than female ones


### (2) length vs. age
plot(len~age, data=male, pch=2, col="blue", xlim=range(SB$age, na.rm=T),
     ylim=range(SB$len, na.rm=T), ylab="length")
points(x=female$age+0.2, y=female$len, pch=3, col="red")

library(FSA)
library(NCStats)
library(nlstools)
library(sciplot)
library(relax)
library(tkrplot)

### fit von Bertalanffy for male
vbStarts(len~age, data=male, type="original", dynamicPlot=TRUE) 
# start points: Linf=627, K=0.3193

svb = list(Linf = 627, K = 0.3193, to = 0)
nl1 = nls(len ~ Linf*(1-exp(-K*(age-to))), start=svb, data=male)
summary(nl1)

Linf = coef(nl1)[1] 
K = coef(nl1)[2]
to = coef(nl1)[3]

lines(s, Linf*(1-exp(-K*(s-to))), type="l", lwd=2, lty=1, col="blue")

### fit von Bertalanffy for female
# remove na
female <- na.omit(female)
vbStarts(len~age, data=female, type="original", dynamicPlot=TRUE) 
# start points: Linf=730, K=0.2429

svb = list(Linf = 730, K = 0.2429, to = 0)
nl1 = nls(len ~ Linf*(1-exp(-K*(age-to))), start=svb, data=female)
summary(nl1)

Linf = coef(nl1)[1] 
K = coef(nl1)[2]
to = coef(nl1)[3]

lines(s, Linf*(1-exp(-K*(s-to))), type="l", lwd=2, lty=2, col="red")
legend(x=10, y=400, lwd=2, legend=c("male","female"), col=c("blue","red"),
       lty=c(1,2))

# K is smaller in female, but larger in male, which means the 
# growth rate in male is larger than female's. It meets the
# answer in question 1 that male mature earlier than female.
# Moreover, Linf of female is larger than male, which coincides
# the reproductive strategies that larger female have higher fecundity.



##### exercise 2
SBegg <- read.table("SBeggsize.txt", header=T)

### (1) 
# test if eggwgt1 is equal to eggwgt2
t.test(x=SBegg$eggwgt1, y=SBegg$eggwgt2, paired=T)  # p-value = 0.3869


### (2)
# since eggwgt1 is not significantly different from eggwgt2, 
# we only use eggwgt1

# egg weight vs. length
plot(eggwgt1~TLength, data=SBegg, pch=20, xlab="Length", ylab="Egg dry weight")  # no pattern?

fit <- lm(eggwgt1~TLength, data=SBegg)
summary(fit)
abline(fit, col="red", lwd=2)
text(x=550, y=1.2, labels="weight = 0.85 + 0.00017length, p-value=0.052", col="red")

# egg weight vs. age
plot(eggwgt1~Age, data=SBegg, pch=20, xlab="Age", ylab="Egg dry weight")  # no pattern?

fit <- lm(eggwgt1~Age, data=SBegg)
summary(fit)
abline(fit, col="red", lwd=2)
text(x=14, y=1.2, labels="weight = 0.93 + 0.0025age, p-value=0.215", col="red")



##### exercise 3
cutlass <- read.table("Cutlassfish_biologicaldata.txt", header=T)

# remove immature individuals
cutlass.1 <- subset(cutlass, subset=cutlass$maturity=="M")

# separate data into male and female
cutlass.male <- subset(cutlass.1, subset=cutlass.1$sex=="M")
cutlass.female <- subset(cutlass.1, subset=cutlass.1$sex=="F")


### (1)
# GSI
cutlass.male$GSI <- with(cutlass.male, 
                         as.numeric(as.character(gonad_weight))/as.numeric(as.character(Total_weight)))
cutlass.female$GSI <- with(cutlass.female, 
                           as.numeric(as.character(gonad_weight))/as.numeric(as.character(Total_weight)))

# mean GSI according to month
table.m <- data.frame(aggregate(cutlass.male$GSI, by=list(cutlass.male$Month), FUN=mean))
table.f <- data.frame(aggregate(cutlass.female$GSI, by=list(cutlass.female$Month), FUN=mean))

plot(x~Group.1, table.m, pch=20, type="b", xlab="Month", ylab="GSI", 
     ylim=range(c(table.m$x, table.f$x)))
points(x=table.f$Group.1, y=table.f$x, col="red", pch=20, type="b", lty=2)
legend("topright", lty=c(1,2), legend=c("Male","Female"), col=c(1,2))


### (2)
plot(preanal_length~Total_weight, data=cutlass.1)
# length is correlated with weight

with(data.frame(aggregate(cutlass.male$preanal_length, by=list(cutlass.male$Month), FUN=mean)),
     plot(x~Group.1, xlab="Month", ylab="Length", pch=20, type="b", ylim=c(130, 300)))
with(data.frame(aggregate(cutlass.female$preanal_length, by=list(cutlass.female$Month), FUN=mean)),
     points(x=Group.1, y=x, pch=20, type="b", col="red", lty=2))
legend("topright", lty=c(1,2), legend=c("Male","Female"), col=c(1,2))

