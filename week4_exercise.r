data1 = read.table("NYdata.txt", header=T, na.string="*")

x = data1$Age
y = data1$TLength

# data exploration
plot(x, y, type="p", main="plot", xlab="Age", ylab="Length")
hist(data1$eggwgt1)
boxplot(data1$eggwgt1)
boxplot(eggwgt1~Age, data1)  # grouped by age
range(data1$Age)
qqnorm(data1$eggwgt1)  # normality

plot(data1$TLength[1:11]~data1$Age[1:11], ylab="Total length (mm)", xlab="Age (days)", 
     main="larval hairtail", pch=19, col="black")
legend('topright', legend=c("2013.04.08","2004.01.10","2000.08.16","2009.05.26","2001.12.09","2004.11.24","2006.03.28"), 
       col=c("black","red","purple","blue","green","orange","pink"), pch=19, lwd=1.5)

# t-test
t.test(data1$eggwgt1, data1$eggwgt2, paired=T)

data2 = data.frame(data1$TLength, data1$Age, data1$eggwgt1, data1$eggwgt2, data1$meandiam)

# correlation test
cor(data2)
cor(data1$TLength, data1$Age)
cor.test(data1$TLength, data1$Age)

# linear regression
mod = lm(data1$TLength~data1$Age)  # fitting linear model
summary(mod)  # summarizing linear model fits

plot(data1$TLength~data1$Age, xlab="Age (year)", ylab="Total Length (mm)")
abline(coef(mod[1], coef(mod[2])), col=2)

