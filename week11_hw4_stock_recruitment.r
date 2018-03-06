setwd('D:/MyGitHub/Introduction_to_Fisheries_Science_and_Management')

##### exercise 1
Ice <- read.table("Iceland.txt", header=T)
Celtic <- read.table("Celtic.txt", header=T)

# Iceland stock
plot(Recruitment~Stock, data=Ice, pch=19, main="Iceland Stock")  # Ricker

# Celtic stock
plot(Recruitment~Stock, data=Celtic, pch=19, main="Celtic Stock")  # Ricker

### model fitting
library(FSA)
library(NCStats)
library(nlstools)
library(plotrix)

### for Celtic stock
### fit Beverton-Holt model
plot(Recruitment~Stock, data=Celtic, pch=19, main="Celtic Stock")

# compute initial coefficients of Beverton-Holt model
cbhs = srStarts(Recruitment~Stock, data=Celtic, type="BevertonHolt", param=1)
# formula
cbh = log(Recruitment)~log((a*Stock)/(1+b*Stock))
cbh
# nonlinear least squares fitting
cbhnls = nls(cbh, data=Celtic, start=cbhs)
# plot curve
curve((coef(cbhnls)[1]*x)/(1+coef(cbhnls)[2]*x), from=0, to=25000, 
      col="red", lwd=2, lty=1, add=TRUE)
overview(cbhnls)


### fit Ricker model
# formula
cr = srFuns(type="Ricker", param=1)

# compute initial coefficients of Ricker model
crs = srStarts(Recruitment~Stock, data=Celtic, type="Ricker", param=1)
crs
# nonlinear least squares fitting
crnls <- nls(log(Recruitment)~log(cr(Stock,a,b)), data=Celtic, start=crs,
             algorithm="port", lower=c(0))
# plot curve
curve(cr(x,coef(crnls)[1],coef(crnls)[2]), from=0, to=25000, col=4,
      lwd=2, lty=4, add=TRUE)
legend("topright", legend=c("Beverton-Holt model","Ricker model"),
       col=c(2,4), lwd=2, lty=c(1, 4))
overview(crnls)

# comparison between 2 models
AIC(cbhnls, crnls)  # Ricker model is better

# or see R2 = 1-RSS/TSS
# Beverton-Holt model
a = coef(cbhnls)[1]
b = coef(cbhnls)[2]
fitted = log((a*Celtic$Stock)/(1+b*Celtic$Stock))
RSS = sum((log(Celtic$Recruitment)-fitted)^2)
TSS = sum((log(Celtic$Recruitment)-mean(log(Celtic$Recruitment)))^2)
R2 = 1-RSS/TSS
R2  # 0.0947

# Ricker model
a = coef(crnls)[1]
b = coef(crnls)[2]
fitted = log(a*Celtic$Stock*exp(-b*Celtic$Stock))
RSS = sum((log(Celtic$Recruitment)-fitted)^2)
TSS = sum((log(Celtic$Recruitment)-mean(log(Celtic$Recruitment)))^2)
R2 = 1-RSS/TSS
R2  # 0.1012, better, but both are small


### for Iceland stock
### fit Beverton-Holt model
plot(Recruitment~Stock, data=Ice, pch=19, main="Iceland Stock")

# compute initial coefficients of Beverton-Holt model
ibhs = srStarts(Recruitment~Stock, data=Ice, type="BevertonHolt", param=1)
# formula
ibh = log(Recruitment)~log((a*Stock)/(1+b*Stock))
ibh
# nonlinear least squares fitting
ibhnls = nls(ibh, data=Ice, start=ibhs)
# plot curve
curve((coef(ibhnls)[1]*x)/(1+coef(ibhnls)[2]*x), from=0, to=10000000, 
      col="red", lwd=2, lty=1, add=TRUE)
overview(ibhnls)


### fit Ricker model
# formula
ir = srFuns(type="Ricker", param=1)

# compute initial coefficients of Ricker model
irs = srStarts(Recruitment~Stock, data=Ice, type="Ricker", param=1)
irs
# nonlinear least squares fitting
irnls <- nls(log(Recruitment)~log(ir(Stock,a,b)), data=Ice, start=irs,
             algorithm="port", lower=c(0))
# plot curve
curve(ir(x,coef(irnls)[1],coef(irnls)[2]), from=0, to=10000000, col=4,
      lwd=2, lty=4, add=TRUE)
legend("topright", legend=c("Beverton-Holt model","Ricker model"),
       col=c(2,4), lwd=2, lty=c(1, 4))
overview(irnls)

### comparison between 2 models
AIC(ibhnls, irnls)  # Ricker model is better

# or see R2 = 1-RSS/TSS
# Beverton-Holt model
a = coef(ibhnls)[1]
b = coef(ibhnls)[2]
fitted = log((a*Ice$Stock)/(1+b*Ice$Stock))
RSS = sum((log(Ice$Recruitment)-fitted)^2)
TSS = sum((log(Ice$Recruitment)-mean(log(Ice$Recruitment)))^2)
R2 = 1-RSS/TSS
R2  # 0.1139

# Ricker model
a = coef(irnls)[1]
b = coef(irnls)[2]
fitted = log(a*Ice$Stock*exp(-b*Ice$Stock))
RSS = sum((log(Ice$Recruitment)-fitted)^2)
TSS = sum((log(Ice$Recruitment)-mean(log(Ice$Recruitment)))^2)
R2 = 1-RSS/TSS
R2  # 0.1248, better, but both are small



##### exercise 2
# for Iceland stock
# stock size giving the maximum average recruitment
1/coef(irnls)[2]  # 639256
# Rmax
coef(irnls)[1]/(coef(irnls)[2]*exp(1))  # 216967



##### exercise 3
### for Iceland stock
# Based on question 2,
# as the present stock is 400000 Mt or 600000 Mt, we should reduce
# fishing mortality in order to recover the stock. However,
# as the present stock is 1000000 Mt, the recruitment at this stock size
# is not maximum. We should increase fishing mortality.


### for Celtic Sea stock
# stock size giving the maximum average recruitment
1/coef(crnls)[2]  # 23652
# Rmax
coef(crnls)[1]/(coef(crnls)[2]*exp(1))  # 4316

# As the present stock is 5000 Mt or 15000 Mt, we should reduce
# fishing mortality in order to recover the stock. However,
# as the present stock is 25000 Mt, the recruitment at this stock size
# is not maximum. We should increase fishing mortality.



##### exercise 4, (need to modify; add data point and redo everything)

Celtic[37, ] = c(2007, 23000, 852)
Celtic[38, ] = c(2008, 21820, 1945)

plot(Recruitment~Stock, data=Celtic, pch=19, main="Celtic Stock")

### fit Beverton-Holt model
# compute initial coefficients of Beverton-Holt model
cbhs = srStarts(Recruitment~Stock, data=Celtic, type="BevertonHolt", param=1)
# formula
cbh = log(Recruitment)~log((a*Stock)/(1+b*Stock))
cbh
# nonlinear least squares fitting
cbhnls = nls(cbh, data=Celtic, start=cbhs)
# plot curve
curve((coef(cbhnls)[1]*x)/(1+coef(cbhnls)[2]*x), from=0, to=25000, 
      col="red", lwd=2, lty=1, add=TRUE)
overview(cbhnls)


### fit Ricker model
# formula
cr = srFuns(type="Ricker", param=1)

# compute initial coefficients of Ricker model
crs = srStarts(Recruitment~Stock, data=Celtic, type="Ricker", param=1)
crs
# nonlinear least squares fitting
crnls <- nls(log(Recruitment)~log(cr(Stock,a,b)), data=Celtic, start=crs,
             algorithm="port", lower=c(0))
# plot curve
curve(cr(x,coef(crnls)[1],coef(crnls)[2]), from=0, to=25000, col=4,
      lwd=2, lty=4, add=TRUE)
legend("topright", legend=c("Beverton-Holt model","Ricker model"),
       col=c(2,4), lwd=2, lty=c(1, 4))
overview(crnls)

# comparison between 2 models
# Beverton-Holt model
a = coef(cbhnls)[1]
b = coef(cbhnls)[2]
fitted = log((a*Celtic$Stock)/(1+b*Celtic$Stock))
RSS = sum((log(Celtic$Recruitment)-fitted)^2)
TSS = sum((log(Celtic$Recruitment)-mean(log(Celtic$Recruitment)))^2)
R2 = 1-RSS/TSS
R2  # 0.0347

# Ricker model
a = coef(crnls)[1]
b = coef(crnls)[2]
fitted = log(a*Celtic$Stock*exp(-b*Celtic$Stock))
RSS = sum((log(Celtic$Recruitment)-fitted)^2)
TSS = sum((log(Celtic$Recruitment)-mean(log(Celtic$Recruitment)))^2)
R2 = 1-RSS/TSS
R2  # 0.0787, better, but both are small

# stock size giving the maximum average recruitment
1/coef(crnls)[2]  # 13096
# Rmax
coef(crnls)[1]/(coef(crnls)[2]*exp(1))  # 3248



##### exercise 5
