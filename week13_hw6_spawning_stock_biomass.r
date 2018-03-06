setwd('D:/MyGitHub/Introduction_to_Fisheries_Science_and_Management')

##### exercise 1
# use excel
data1 <- scan()
93030.6806 146.29
62930.2126 345.38
35897.26298 554.9666667
14325.55498 84.55

data1 <- data.frame(matrix(data1, nrow=2))
data1 <- data.frame(t(data1))
colnames(data1) <- c("N", "CPUE")

plot(CPUE~N, data=data1, pch=20)

fit1 <- lm(CPUE~N, data=data1)
summary(fit1)  # q is biological nonsense



##### exercise 2
### (1)
data2 <- read.table("exercise2.txt", header=T)

# assume 1000 recruit
data2$N <- c(1000, rep(0,10))

### for F=0.2
# total mortality
data2$Z <- c(0.2, 0.2, rep(0.2+0.2, 9))

# population size
for (i in 1:10) {
    data2[(i+1), 5] = data2[i, 5]*exp(-data2[i, 6])
}

# fishing death
data2$f.death <- 0

for (i in 3:10) {
    data2[i, 7] = (data2[i,5]-data2[(i+1),5])*0.2/0.4
}

data2$f.death[11] = (data2$N[11]-data2$N[11]*exp(-0.4))*0.2/0.4

# catch(kg)
data2$catch <- data2$f.death*data2$Weight_kg


### for F=0.4~1.2
# write a function to compute catch data
catch.fun <- function(f=0.2) {
    data2$Z <- c(0.2, 0.2, rep(0.2+f, 9))
    for (i in 1:10) {
        data2[(i+1), 5] = data2[i, 5]*exp(-data2[i, 6])
    }
    data2$f.death <- 0
    for (i in 3:10) {
        data2[i, 7] = (data2[i,5]-data2[(i+1),5])*f/(0.2+f)
    }
    data2$f.death[11] = (data2$N[11]-data2$N[11]*exp(-0.2-f))*f/(0.2+f)
    data2$f.death*data2$Weight_kg
}

catch.data <- apply(matrix(seq(0.2,1.2,0.2)), 1, FUN=catch.fun)
result <- apply(catch.data, 2, FUN=sum)/1000

plot(y=result, x=seq(0.2,1.2,0.2), pch=20, type="l", 
     xlab="Fishing Mortality", ylab="YPR (kg)")


### (2)
# write a new function 
catch.fun1 <- function(f=0.2, age=4) {
    data2$Z <- c(rep(0.2, age-2), rep(0.2+f, 11-age+2))
    for (i in 1:10) {
        data2[(i+1), 5] = data2[i, 5]*exp(-data2[i, 6])
    }
    data2$f.death <- 0
    n = age-2+1
    for (i in n:10) {
        data2[i, 7] = (data2[i,5]-data2[(i+1),5])*f/(0.2+f)
    }
    data2$f.death[11] = (data2$N[11]-data2$N[11]*exp(-0.2-f))*f/(0.2+f)
    data2$f.death*data2$Weight_kg
}

# F=0.5
catch.data <- apply(matrix(c(4,5,7,9)), 1, FUN=function(x){
    catch.fun1(f=0.5, age=x)
})
result <- apply(catch.data, 2, FUN=sum)/1000

# F=1
catch.data1 <- apply(matrix(c(4,5,7,9)), 1, FUN=function(x){
    catch.fun1(f=1, age=x)
})
result1 <- apply(catch.data1, 2, FUN=sum)/1000

result.data <- rbind(result, result1)

plot(y=result.data[, 1], x=c(0.5, 1), pch=20, type="b", ylim=range(result.data),
     xlab="Fishing Mortality", ylab="YPR (kg)")
points(y=result.data[, 2], x=c(0.5, 1), pch=20, type="b", col=2)
points(y=result.data[, 3], x=c(0.5, 1), pch=20, type="b", col=3)
points(y=result.data[, 4], x=c(0.5, 1), pch=20, type="b", col=4)
legend("topleft", legend=c("> 50cm", "> 60cm", "> 70cm", "> 80cm"), col=1:4, lty=rep(1,4))


### (3)
data2$GSI <- data2$Age*0.05
data2$gonad.kg <- with(data2, N*Weight_kg*Maturity*GSI)

# write a function to compute SSB
ssb.fun <- function(f=0.2, age=4) {
    data2$Z <- c(rep(0.2, age-2), rep(0.2+f, 11-age+2))
    for (i in 1:10) {
        data2[(i+1), 5] = data2[i, 5]*exp(-data2[i, 6])
    }
    data2$GSI <- data2$Age*0.05
    with(data2, N*Weight_kg*Maturity*GSI)
}

# F=0.5
gonad.data <- apply(matrix(c(4,5,7,9)), 1, FUN=function(x){
    ssb.fun(f=0.5, age=x)
})
result.ssb <- apply(gonad.data, 2, FUN=sum)/1000

# F=1
gonad.data1 <- apply(matrix(c(4,5,7,9)), 1, FUN=function(x){
    ssb.fun(f=1, age=x)
})
result.ssb1 <- apply(gonad.data1, 2, FUN=sum)/1000

result.data.ssb <- rbind(result.ssb, result.ssb1)

plot(y=result.data.ssb[, 1], x=c(0.5, 1), pch=20, type="b", ylim=range(result.data.ssb),
     xlab="Fishing Mortality", ylab="SSB/R (kg)")
points(y=result.data.ssb[, 2], x=c(0.5, 1), pch=20, type="b", col=2)
points(y=result.data.ssb[, 3], x=c(0.5, 1), pch=20, type="b", col=3)
points(y=result.data.ssb[, 4], x=c(0.5, 1), pch=20, type="b", col=4)
legend(x=0.9, y=2.2, legend=c("> 50cm", "> 60cm", "> 70cm", "> 80cm"), col=1:4, lty=rep(1,4))
