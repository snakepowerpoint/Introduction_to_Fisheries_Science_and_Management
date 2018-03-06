setwd('D:/MyGitHub/Introduction_to_Fisheries_Science_and_Management')

##### exercise 1
data1 <- read.table("data1.txt", header=T)

### 1)
cor(data1$N, data1$CPUE_10)
cor.test(data1$N, data1$CPUE_10)  # p-value < 0.001
plot(data1$N, data1$CPUE_10)

### 2)
cor(data1$N, data1$CPUE_20)
cor.test(data1$N, data1$CPUE_20)  # p-value < 0.001
plot(data1$N, data1$CPUE_20)

### 3)
cor(data1$N, data1$CPUE_40)
cor.test(data1$N, data1$CPUE_40)  # p-value = 0.45
plot(data1$N, data1$CPUE_40)

# different with regression?
lm(N~CPUE_10, data=data1)$coeff
lm(N~CPUE_20, data=data1)$coeff
lm(N~CPUE_40, data=data1)$coeff


##### exercise 2
# q cannot be assumed constant due to stochasticity


##### exercise 3
data2 <- read.table("data2.txt", header=T)
library(lattice)

data2.1 <- data.frame(data2[, 3:4])
rownames(data2.1) <- data2$Bay

barplot(t(data2.1), beside=T, col=c("#FFFADC", "lightgreen"), las=1, ylab="CPUE", 
        legend=T, ylim=c(0,max(data2.1)))
box(bty="l")

### test if mean CPUE is different between vegetated and non-vegetated areas
t.test(x=data2[, 3], y=data2[, 4], alternative=c("two.sided"), paired=T)  # p-value = 0.3575


##### exercise 4
data3 <- read.table("data3.txt", header=T)
data3.1 <- rbind(data2.1, data3[, 3:4])
rownames(data3.1) <- c(paste(data2$Bay, rep(1, 7), sep=""), paste(data2$Bay, rep(2, 7), sep=""))

barplot(t(data3.1), beside=T, col=c("#FFFADC", "lightgreen"), las=1, ylab="CPUE",
        legend=T, ylim=c(0,max(data3.1)))
box(bty="l")

### test if mean CPUE is different between vegetated and non-vegetated areas
t.test(x=data3.1[, 1], y=data3.1[, 2], alternative=c("two.sided"))  # p-value = 0.5127

### test if mean CPUE is different between that on January and on February
# for non-vegetated areas
t.test(x=data3.1[1:7, 1], y=data3.1[8:14, 1], alternative=c("two.sided"))  # p-value = 0.008963

# for vegetated areas
t.test(x=data3.1[1:7, 2], y=data3.1[8:14, 2], alternative=c("two.sided"))  # p-value = 0.01047


##### exercise 5
data4 <- read.table("data4.txt", header=T)
summary(data4)

boxplot(data4)
plot(Effort.No.boat.~Year, data4, type="b", lwd=2, ylab="Effort (No. boat)", main="Annual fishing effort")
plot(Catch.tons.~Year, data4, type="b", lwd=2, ylab="Catch (tons)", main="Annual catch weight")
plot(TotalCatch.tons.~Year, data4, type="b", lwd=2, ylab="Total catch (tons)", main="Annual total catch weight")


##### exercise 6
data4$CPUE = with(data4, Catch.tons./Effort.No.boat.)
data4$CPUEt = with(data4, TotalCatch.tons./Effort.No.boat.)

plot(CPUE~Year, data4, type="b", lwd=2, main="Annual CPUE for ¥Õ±a³½")
plot(CPUEt~Year, data4, type="b", lwd=2, main="Annual CPUE for fishery")


##### exercise 7
plot(Catch.tons.~Effort.No.boat., data4, type="b", xlab="Effort", ylab="Catch (tons)",
     ylim=c(0.98*min(Catch.tons.), 1.02*max(Catch.tons.)), main="Catch vs. Effort")
text(x=data4$Effort.No.boat., y=data4$Catch.tons., labels=data4$Year, pos=3, cex=0.7)
plot(TotalCatch.tons.~Effort.No.boat., data4, type="b", xlab="Effort", ylab="Total catch (tons)",
     ylim=c(0.98*min(TotalCatch.tons.), 1.02*max(TotalCatch.tons.)), main="Total catch vs. Effort")
text(x=data4$Effort.No.boat., y=data4$TotalCatch.tons., labels=data4$Year, pos=3, cex=0.7)
