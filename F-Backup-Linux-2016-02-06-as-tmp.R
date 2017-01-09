
library(Hmisc)
data(mtcars)

# 32 observations of 11 variables. no N.A. values
str(mtcars) 

mtcars

mtcars2 = mtcars[ which(mtcars$cyl == 8 |mtcars$cyl == 4) , ]
mtcars2


fit1 <- lm(mpg ~  factor(mtcars2$cyl) + mtcars2$wt, data = mtcars2)
fit1
fit2 <- lm(mpg ~  factor(mtcars2$cyl) , data = mtcars2)
fit2
anova(fit1, fit2)

fit = lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
fit 



x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit <- lm(y ~ x)
round(hatvalues(fit)[1 : 5], 4)
round(dfbetas(fit)[1 : 5, 2], 4)

plot(x,y)

1.344/11.72





plot(fit)

pairs(mtcars)