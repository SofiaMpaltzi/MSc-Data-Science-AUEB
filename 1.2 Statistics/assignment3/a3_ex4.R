#create dataframe of data
novice <- c(22.1, 22.3, 26.2, 29.6, 31.7, 33.5, 38.9, 39.7, 43.2, 43.2)
advanced <- c(32.5, 37.1, 39.1, 40.5, 45.5, 51.3, 52.6, 55.7, 55.9, 57.7)
proficient <- c(40.1, 45.6, 51.2, 56.4, 58.1, 71.1, 74.9, 75.9, 80.3, 85.3)

df <- data.frame(novice, advanced, proficient)
df

#inspect data
summary(df)

#visualize data
colfunc <- colorRampPalette(c("deeppink4", "deepskyblue4"))
boxplot(df, main='Boxplots of categories', col=colfunc(3))

plot(density(df$novice), 
     col=colfunc(3),
     lwd = 2, 
     main='Distribution before & after',
     xlim=c(0, 100),
     xlab='Score')
lines(density(df$advanced),
      col=colfunc(3)[2],
      lwd = 2)
lines(density(df$proficient),
      col=colfunc(3)[3],
      lwd = 2)
legend(x='topright',
       legend=c('novice', 'advanced', 'proficient'),
       col=colfunc(3),
       lty=1:1,
       cex=0.8,
       lwd = 2)

dev.off()

#create dataframe of data
score <- c(novice, advanced, proficient)
level <-rep(c('novice', 'advanced', 'proficient'), c(10, 10, 10))
df1 <- data.frame(score, level)
df1

#test for difference between levels:
fit <- aov(score ~ factor(level), data = df1)
fit
summary(fit)

#assumptions
#normally distributed residuals
qqnorm(fit$residuals,main="QQplot for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit$residuals)

#homogeneity of variances
bartlett.test(score ~ factor(level), data = df1)
fligner.test(score ~ factor(level), data = df1)
library(car)
leveneTest(score ~ factor(level), data = df1)

#diagnostic plots
dev.off()
layout(matrix(1:4,2,2))
plot(fit)

#kruskal wallis
kruskal.test(score ~ factor(level), data = df1)

#pairwise t test
pairwise.t.test(score, factor(level), p.adjust.method="none")

#paiwise t test with ??=0.05/3
pairwise.t.test(score, factor(level), p.adjust.method="bonferroni")

