#load data
require(stats)
data(mtcars)

#define columns as factor
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)

#report descriptive statistics
summary(mtcars)
library(Hmisc)
describe(mtcars)

#visualization of factor variables
hist(mtcars$mpg, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of mpg',
     xlab = 'mpg')
hist(mtcars$disp, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of disp',
     xlab = 'disp')
hist(mtcars$hp, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of hp',
     xlab = 'hp')
hist(mtcars$drat, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of drat',
     xlab = 'drat')
hist(mtcars$wt, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of wt',
     xlab = 'wt')
hist(mtcars$qsec, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of qsec',
     xlab = 'qsec')
hist(mtcars$gear, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of gear',
     xlab = 'gear')
hist(mtcars$carb, 
     breaks = 10, 
     col = "deepskyblue4",
     main = 'Histogram of carb',
     xlab = 'carb')


#pairwise scatterplots
library(gclus)
dta <- mtcars[c(1,3,4,5,6,7,10,11)]
dta.r <- abs(cor(dta))
dta.col <- dmat.color(dta.r)
dta.o <- order.single(dta.r)
cpairs(dta,
       dta.o,
       panel.colors=dta.col,
       gap=.5,
       main='Variables Ordered and Colored by Correlation')
#compute correlation coefficients
dta.r

#t.test
boxplot(mpg ~ am, data=mtcars, main = 'Comparison of MPG by type of Transmission',
                    xlab = 'Type of Gear',
                    ylab ='Car consumption (MPG)',
                    ylim = c(10, 35),
                    col = c('deepskyblue4', 'deeppink4'),
                    names=c('automatic', 'manual'))


library(nortest)
#normality for manual
ad.test(mtcars[mtcars$am == '0', 'mpg'])
#normality for automatic
ad.test(mtcars[mtcars$am == '1', 'mpg'])

#equality of variances
res.ftest <- var.test(mtcars[mtcars$am == '0', 'mpg'], mtcars[mtcars$am == '1', 'mpg'])
res.ftest

t.test(mtcars[mtcars$am == '0', 'mpg'], mtcars[mtcars$am == '1', 'mpg'], alternative = "two.sided", var.equal = TRUE)

#anova
boxplot(mpg~cyl,
        data=mtcars,
        main='Mpg vs number of cylinders',
        xlab='Number of cylinders',
        ylab='Miles per gallon',
        col = colfunc(3))

fit <- aov(mpg~cyl,data=mtcars)
fit
summary(fit)

# Test the normality assumption in the residuals
qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit$residuals)

# Test the assumption of homogeneity of variances: not ok
bartlett.test(mpg~cyl,data=mtcars)
fligner.test(mpg~cyl,data=mtcars)

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(fit)

#non paraetric test that also fails which means that variances are not equal
attach(mtcars)
kruskal.test(mpg~cyl)

#bic
fit_all <- lm(mpg ~ ., data = mtcars)
fit_all
summary(fit_all)

fit_bic <- step(fit_all, direction = "both", k = log(32))
fit_bic
summary(fit_bic)
anova(fit_bic)

#choose model
anova(fit_bic, fit_all)

#assumptions
# Test the normality assumption in the residuals:not ok
qqnorm(fit_bic$residuals,main="NPP for residuals")
qqline(fit_bic$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit_bic$residuals)
# Diagnostic Plots
par(mfrow = c(2, 2))
plot(fit_bic)


