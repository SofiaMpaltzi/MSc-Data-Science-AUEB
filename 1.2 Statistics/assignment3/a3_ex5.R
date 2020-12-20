#read txt file and put to data frame
grades <- read.table("assignment_grades_data.txt", header = TRUE, sep=" ")
grades

#inspect data
summary(grades)

#visualize data
hist(grades$size,
     breaks = 10,
     col = "deepskyblue4",
     main = "Histogram of Size",
     xlab = "Size",
     freq = FALSE)
lines(density(grades$size), 
      col='red', 
      lwd = 2)

boxes <- cbind(grades$assignment1, grades$assignment2)
colnames(boxes)[c(1,2)] <- c("assignment1", "assignment2")
colfunc <- colorRampPalette(c("deeppink4", "deepskyblue4"))
boxplot(boxes, main='Boxplot per assignment', col=colfunc(2))

#linear regression
#calculate correlation 
cor(grades$assignment1, grades$assignment2)
cor.test(grades$assignment1, grades$assignment2)
#correlation Spearman
cor(grades$assignment1, grades$assignment2, method="spearman")
cor.test(grades$assignment1, grades$assignment2, method="spearman")
#correlation matrix
cor(cbind(grades$assignment1, grades$assignment2))

#linear model
model <- lm(assignment2 ~ assignment1, data=grades)
print(model)
summary(model)

#Scatter plot
plot(grades$assignment1, grades$assignment2, main="Assignmnet 1 vs Assignment 2", xlab="Assignment 1", ylab="Assignment 2")
abline(model, col="red")

assignment1_theoretical <- seq(0, 10, by=0.05)
conf_interval <- predict(model,
                         newdata=data.frame(assignment1=assignment1_theoretical),
                         interval="confidence",
                         level = 0.9)
lines(assignment1_theoretical, conf_interval[,2], col="blue", lty=2)
lines(assignment1_theoretical, conf_interval[,3], col="blue", lty=2)

pred_interval <- predict(model,
                         newdata=data.frame(assignment1=assignment1_theoretical),
                         interval="prediction",
                         level = 0.9)
lines(assignment1_theoretical,  pred_interval[,2], col="orange", lty=2)
lines(assignment1_theoretical,  pred_interval[,3], col="orange", lty=2)

#test assumptions
par(mfrow = c(2, 2))
plot(model)

#normally distributed residuals
shapiro.test(model$residuals)

#linear model.1
model1 <- lm(assignment2 ~ assignment1 + size, data=grades)
print(model1)
summary(model1)

#test assumptions
par(mfrow = c(2, 2))
plot(model1)

#normally distributed residuals
shapiro.test(model1$residuals)

#anova between models
anova(model, model1)

#estimate
predict(model1, data.frame(assignment1 = 6, size = 10), interval = 'confidence', level=0.9)

#predict
predict(model1, data.frame(assignment1 = 6, size = 10), interval = 'predict', level=0.9)

#better model
model2 <- lm(assignment2 ~ sqrt(size), data=grades)

print(model2)
summary(model2)

#test assumptions
par(mfrow = c(2, 2))
plot(model2)

#find best model
BIC(model)
BIC(model1)
BIC(model2)

