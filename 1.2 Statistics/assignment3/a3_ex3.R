#observations from before and after the treatment
before <- c(121.5, 122.4, 126.6, 120.0, 129.1, 124.9, 138.8, 124.5, 116.8, 132.2)
after <- c(117.3, 128.6, 121.3, 117.2, 125.6, 121.5, 124.2, 121.6, 117.9, 125.4)

df <- data.frame(before, after)

#plot densities to visualize difference in means
plot(density(df$after), 
     col='deepskyblue4', 
     main='Distribution before & after',
     xlim=c(100, 150),
     xlab='Blood pessure level')
lines(density(df$before), col='deeppink4')
legend(x='topright', legend=c('before', 'after'),
       col=c('deeppink4', 'deepskyblue4'), lty=1:1, cex=0.8)

#second plot to better show difference in means
boxplot(df, main='Before and after boxplots', col=c('deeppink4', 'deepskyblue4'))

#report summary statistics
summary(df)

#paired t-test to examine difference in means
#asuumptions: independent and normally distribted
library(nortest)
shapiro.test(df$before)
ad.test(df$before)
qqnorm(df$before, main="QQ plot for before drug")
qqline(df$before, col="red",lty=1,lwd=2)

shapiro.test(df$after)
ad.test(df$after)
qqnorm(df$after, main="QQ plot for after drug")
qqline(df$after, col="red",lty=1,lwd=2)

#check equality of variances on normally disributed data
var.test(df$after, df$before)
#bartlett test
bartlett.test(list(df$after, df$before))

#ttest
t.test(df$after, df$before, paired = TRUE, alternative = "greater")

