data(ToothGrowth)
str(ToothGrowth)

from this we learn we have two llevels

to get a better feeling for the data we do box plots with the two levels


VC = subset(ToothGrowth, supp == "VC" , c(len, dose))
VC = as.data.frame(VC)
VC
VC$dose <- factor(VC$dose)
boxplot(len ~ dose, data = VC, main="Dosis vs. Length", xlab="Dosis ", ylab="Length", color="red")

VCt = subset(VC, dose == c(0.5,2) , c(len, dose))





mns = 0
for (i in 1 : 1000) mns = c(mns, mean(rexp(40, 0.2)-5))
hist(mns, freq = FALSE, col = "grey")


vals = rexp(1000, 0.2)
hist(vals, freq = FALSE, col = "grey")


t.test(len ~ dose, paired = FALSE, val.equal=TRUE, data = VCt)

str(ToothGrowth)

x = subset(ToothGrowth, supp,  c(len, supp))
x = [c(ToothGrowth$len, ToothGrowth$supp), ]

ToothGrowth

VCOJt <- subset(ToothGrowth,select=c(len, supp)) 
t.test(len ~ supp, paired = FALSE, val.equal=TRUE, data = VCOJt)

sum(is.na(ToothGrowth$len))
sum(is.na(ToothGrowth$dose))
sum(is.na(ToothGrowth$supp))

boxplot(VC2)

plot(VC["dose"], VC["len"], main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19) 


VC

xx = subset(ToothGrowth, supp == "VC" , c(len, dose))
yy = subset(VC, dose == c(0.5,2) , c(len, dose))
yy
plot(xx)
plot(VC$dose, VC$len)
with(xx, plot(dose, len))



