#read txt file and put to data frame
drug_res_time <- read.table("drug_response_time.txt", header = TRUE, sep=" ")

#separate based on type (A or B) of drug
a <- drug_res_time[drug_res_time$drug == "A", "time"]
b <- drug_res_time[drug_res_time$drug == "B", "time"]

#import nortest and check for normality with Anderson Darling
#Shapiro WIlk
shapiro.test(a)
qqnorm(a, main="QQ plot for treatment A")
qqline(a, col="red",lty=1,lwd=2)

shapiro.test(b)
qqnorm(b, main="QQ plot for treatment B")
qqline(b, col="red",lty=1,lwd=2)

#Anderson Darling test
library(nortest)
ad.test(a)
ad.test(b)

#check equality of variances on normally disributed data
var.test(a, b)
#bartlett test
bartlett.test(list(a, b))

#check equality of means on normally disributed data
t.test(a, b, alternative = "two.sided", var.equal = TRUE)

