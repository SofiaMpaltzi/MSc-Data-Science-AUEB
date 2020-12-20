##################################    1. Libraries    ########################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

rm(list = ls())
Sys.setlocale("LC_TIME", "C")

if ("ggplot2" %in% rownames(installed.packages()) == F){
  install.packages("ggplot2")}
require(ggplot2)
if ("dplyr" %in% rownames(installed.packages()) == F){
  install.packages("dplyr")}
require(dplyr)
if ("readxl" %in% rownames(installed.packages()) == F){
  install.packages("readxl")}
require(readxl)
if ("forecast" %in% rownames(installed.packages()) == F){
  install.packages("forecast")}
require(forecast)
if ("gridExtra" %in% rownames(installed.packages()) == F){
  install.packages("gridExtra")}
require(gridExtra)
if ("urca" %in% rownames(installed.packages()) == F){
  install.packages("urca")}
require(urca)
if ("MASS" %in% rownames(installed.packages()) == F){
  install.packages("MASS")}
require(MASS)
if ("fGarch" %in% rownames(installed.packages()) == F){
  install.packages("fGarch")}
require(fGarch)

##################################    2. Load Data    ########################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

# Set working directory
setwd("C:/Users/sofia.baltzi/OneDrive - Accenture/Desktop/Time Series")

# Load
data = read_excel("Data_Assignment.xlsx", skip = 1)

# Fix colnames and transform to uppercase
colnames(data) = gsub("[.]", "_", colnames(data))
colnames(data) = gsub("-", "_", colnames(data))
colnames(data) = gsub("\\(", "", colnames(data))
colnames(data) = gsub("\\)", "", colnames(data))
colnames(data) = gsub("__", "_", colnames(data))

# Manipulate data
data = data %>% 
  dplyr::select(-2) %>% 
  dplyr::rename(x1=RUS_Rf,
                x2=RUS_1_Rf_1,
                x3=MXUS_Rf,
                x4=MEM_Rf,
                x5=SMB,
                x6=HML,
                x7=MOM,
                x8=SBGC_Rf,
                x9=SBWG_Rf,
                x10=LHY_Rf,
                x11=DEFSPR,
                x12=FRBI_Rf,
                x13=GSCI_Rf,
                x14=VIX,
                x15=Rf) %>% 
  data.frame()


# Choose time series
ts1 = data %>% 
  dplyr::select(date, EMN) %>% 
  dplyr::mutate(date=as.Date(date)) %>% 
  data.frame()

##################################    3. Analyze Time Series    ##############################
#                                                                                            #
#                                                                                            #
#                                                                                            #

##### 3.1 Identification Step #####

# Plot Time Series
ggplot(data=ts1, aes(x=date, y=EMN, group=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Time Series plot (EMN)") +
  xlab("Time") + 
  ylab("Returns") +
  geom_line(size=0.7, color="Sky Blue 3") +
  scale_x_date(date_labels="%Y", date_breaks="1 year")


# Test normality
ggplot(data=ts1, aes(x=EMN)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(color="Grey 4 ", fill="Grey", bins=15, binwidth=0.005) +
  geom_density(alpha=.2, color="Sky blue 3", fill="Sky blue 3") +
  ggtitle("Histogram of Time Series")

# QQ-Line params
y = quantile(ts1$EMN[!is.na(ts1$EMN)], c(0.25, 0.75))
x = qnorm(c(0.25, 0.75))
slope = diff(y)/diff(x)
int = y[1L] - slope * x[1L]

ggplot(data=ts1, aes(sample=EMN)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = slope, intercept = int, color="Grey 4", lwd=1.1) +
  stat_qq(color="Sky Blue 4") +
  ggtitle("QQ-Plot of Time Series")


shapiro.test(ts1$EMN)


# Test Autocorrelation
a = ggAcf(ts1$EMN, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle("Autocorrelation Plot")

b = ggPacf(ts1$EMN, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot")

grid.arrange(a,b)

Box.test(ts1$EMN, 6, type="Box-Pierce")
Box.test(ts1$EMN, 6, type="Ljung-Box")
Box.test(ts1$EMN, 10, type="Box-Pierce")
Box.test(ts1$EMN, 10, type="Ljung-Box")
Box.test(ts1$EMN, 60, type="Box-Pierce")
Box.test(ts1$EMN, 60, type="Ljung-Box")

# Test Stationarity
adf=ar(ts1$EMN)
summary(ur.df(ts1$EMN, type="drift", lags=adf$order-1))

##### 3.2 Estimation Step #####

tser = ts(ts1$EMN, frequency=12, start = c(1990,4))
# Fit ma(1)
ma1 = arima(tser, order=c(0,0,1))
ma1

# Fit ma(6)
ma6 = arima(tser, order=c(0,0,6), fixed=c(0,0,0,0,0,NA,NA))
ma6

# Fit ar(6)
ar6 = arima(tser, order=c(6,0,0), fixed=c(0,0,0,0,0,NA,NA))
ar6

# Fit arma(6,1)
arma61 = arima(tser, order=c(6,0,1), fixed=c(0,0,0,0,0,NA,NA,NA))
arma61

# Fit arma(6,6)
arma66 = arima(tser, order=c(6,0,6), fixed=c(0,0,0,0,0,NA,0,0,0,0,0,NA,NA))
arma66


##### 3.3 Diagnostic Plots #####

residuals = ar6$residuals

# Plot Residuals
acf = ggAcf(residuals, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Autocorrelation Plot of Residuals")

pacf = ggPacf(residuals, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot of Residuals")

sacf = ggAcf(residuals^2, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Autocorrelation Plot of Squared Residuals")

spacf = ggPacf(residuals^2, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot of Squared Residuals")

# QQ-plot
# QQ-Line params
y = quantile(residuals[!is.na(residuals)], c(0.25, 0.75))
x = qnorm(c(0.25, 0.75))
slope = diff(y)/diff(x)
int = y[1L] - slope * x[1L]

qq = ggplot(data=ts1, aes(sample=residuals)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = slope, intercept = int, color="Grey 4", lwd=0.9) +
  stat_qq(color="Sky Blue 3") +
  ggtitle("Normal QQ-Plot of Residuals")

# Predict
forecast=predict(ar6,20)
UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se

ts = autoplot(cbind(tser,forecast$pred,UL,LL)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  ggtitle("Plot Forecasts and Confidence Intervals") +
  xlab("Time") + 
  ylab("Returns") +
  labs(color="Legend") +
  scale_color_manual(labels = c("Historicity", "Predictions", "Upper Interval", "Lower Interval"),
                     values=c("Grey 4", "Sky Blue 3","tomato2", "tomato2"))

grid.arrange(acf, pacf, sacf, spacf, qq, ts, ncol=2)

# Test residuals
# Normality
shapiro.test(residuals)
# Autocorrelation
Box.test(residuals, 60, type="Ljung-Box")
# Heteroscedasticity
Box.test(residuals^2, 60, type="Ljung-Box")


##################################    4. Regression    #######################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

# Dependent variable
y1 = data$EMN

# Fit model
fit = lm(y1~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15,data=data)
# Find best model
model = stepAIC(fit, direction="both")
model$anova
model


# Plot Residuals
residuals = residuals(model)

acf = ggAcf(residuals, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Autocorrelation Plot of Residuals")

pacf = ggPacf(residuals, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot of Residuals")

sacf = ggAcf(residuals^2, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Autocorrelation Plot of Squared Residuals")

spacf = ggPacf(residuals^2, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot of Squared Residuals")

# QQ-plot
# QQ-Line params
y = quantile(residuals[!is.na(residuals)], c(0.25, 0.75))
x = qnorm(c(0.25, 0.75))
slope = diff(y)/diff(x)
int = y[1L] - slope * x[1L]

qq = ggplot(data=ts1, aes(sample=residuals)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = slope, intercept = int, color="Grey 4", lwd=0.9) +
  stat_qq(color="Sky Blue 3") +
  ggtitle("Normal QQ-Plot of Residuals")


grid.arrange(acf, pacf, sacf, spacf, qq, ncol=2)

shapiro.test(residuals) # Rejected - no noramlity
Box.test(residuals, 6, type="Box-Pierce") # Rejected - There is autocorrelation
Box.test(residuals, 6, type="Ljung-Box") # Rejected - There is autocorrelation
Box.test(residuals, 20, type="Box-Pierce") # Not Rejected - There is no autocorrelation
Box.test(residuals, 20, type="Ljung-Box") # Rejected - There is autocorrelation
Box.test(residuals, 60, type="Box-Pierce") # Not Rejected - There is no autocorrelation
Box.test(residuals, 60, type="Ljung-Box") # Not Rejected - There is no autocorrelation
Box.test(residuals^2, 6, type="Box-Pierce") # Rejected - There is heteroscedasticity
Box.test(residuals^2, 6, type="Ljung-Box") # Rejected - There is heteroscedasticity
Box.test(residuals^2, 20, type="Box-Pierce") # Rejected - There is heteroscedasticity
Box.test(residuals^2, 20, type="Ljung-Box") # Not Rejected - There is no heteroscedasticityy
Box.test(residuals^2, 60, type="Box-Pierce") # Not Rejected - There is no heteroscedasticity
Box.test(residuals^2, 60, type="Ljung-Box") # Not Rejected - There is no heteroscedasticity


##################################    5. Regression + Arima   ################################
#                                                                                            #
#                                                                                            #
#                                                                                            #


# Try to fix autocorrelation problem
residuals_reg = residuals

arma61 = arima(residuals_reg, order=c(6,0,1), fixed=c(0,0,0,0,0,NA,NA,NA))
residuals_arma = arma61$residuals

acf = ggAcf(residuals_arma, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Autocorrelation Plot of Residuals")

pacf = ggPacf(residuals_arma, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot of Residuals")

sacf = ggAcf(residuals_arma^2, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Autocorrelation Plot of Squared Residuals")

spacf = ggPacf(residuals_arma^2, lag.max = 60) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Partial Autocorrelation Plot of Squared Residuals")

# QQ-plot
# QQ-Line params
y = quantile(residuals_arma[!is.na(residuals_arma)], c(0.25, 0.75))
x = qnorm(c(0.25, 0.75))
slope = diff(y)/diff(x)
int = y[1L] - slope * x[1L]

qq = ggplot(data=ts1, aes(sample=residuals_arma)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = slope, intercept = int, color="Grey 4", lwd=0.9) +
  stat_qq(color="Sky blue 3") +
  ggtitle("Normal QQ-Plot of Residuals")


grid.arrange(acf, pacf, sacf, spacf, qq, ncol=2)

shapiro.test(residuals_arma) # Not Rejected - Noramlity
Box.test(residuals_arma, 6, type="Box-Pierce") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 6, type="Ljung-Box") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 10, type="Box-Pierce") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 10, type="Ljung-Box") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 20, type="Box-Pierce") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 20, type="Ljung-Box") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 60, type="Box-Pierce") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma, 60, type="Ljung-Box") # Not Rejected - There is not autocorrelation
Box.test(residuals_arma^2, 6, type="Box-Pierce") # Rejected - There is heteroscedasticity
Box.test(residuals_arma^2, 6, type="Ljung-Box") # Rejected - There is heteroscedasticity
Box.test(residuals_arma^2, 10, type="Box-Pierce") # Not Rejected - There is no heteroscedasticity
Box.test(residuals_arma^2, 10, type="Ljung-Box") # Not Rejected - There is no heteroscedasticity
Box.test(residuals_arma^2, 60, type="Box-Pierce") # Not Rejected - There is no heteroscedasticity
Box.test(residuals_arma^2, 60, type="Ljung-Box") # Not Rejected - There is no heteroscedasticity


# Try to fix heteroscedasticity problem
# Estimate GARCH(1,0) model
garch6=garchFit(~garch(6,0),data=residuals_arma,trace=F) # trace = F reduces the summary
summary(garch6)
plot(garch6)


4
5
10
11
0


