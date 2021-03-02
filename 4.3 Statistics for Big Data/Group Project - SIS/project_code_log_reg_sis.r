##################################        1. Libraries    #######################################
#                                                                                               #
#                                                                                               #
#                                                                                               #

rm(list = ls())
options(scipen=10000)

if ("ggplot2" %in% rownames(installed.packages()) == F){
  install.packages("ggplot2")}
require(ggplot2)
if ("dplyr" %in% rownames(installed.packages()) == F){
  install.packages("dplyr")}
require(dplyr)
if ("readxl" %in% rownames(installed.packages()) == F){
  install.packages("readxl")}
require(readxl)
if ("stringr" %in% rownames(installed.packages()) == F){
  install.packages("stringr")}
require(stringr)
if ("tidyselect" %in% rownames(installed.packages()) == F){
  install.packages("tidyselect")}
require(tidyselect)
if ("SIS" %in% rownames(installed.packages()) == F){
  install.packages("SIS")}
require(SIS)
if ("lsplsGlm" %in% rownames(installed.packages()) == F){
  install.packages("lsplsGlm")}
require(lsplsGlm)
if ("glmnet" %in% rownames(installed.packages()) == F){
  install.packages("glmnet")}
require(glmnet)
if ("Metrics" %in% rownames(installed.packages()) == F){
  install.packages("Metrics")}
require(Metrics)
if ("InformationValue" %in% rownames(installed.packages()) == F){
  install.packages("InformationValue")}
require(InformationValue)
if ("boot" %in% rownames(installed.packages()) == F){
  install.packages("boot")}
require(boot)
if ("reshape2" %in% rownames(installed.packages()) == F){
  install.packages("reshape2")}
require(reshape2)

##################################        2. Functions    #######################################
#                                                                                               #
#                                                                                               #
#                                                                                               #

# accuracy for CV
costfunc = function(obs, pred) {
  pred_bin = (sign(pred - 0.5) + 1)/2
  return(accuracy(obs, pred_bin))
}


##################################        3. Load Data    #######################################
#                                                                                               #
#                                                                                               #
#                                                                                               #

# Set working directory
setwd("C:\\Users\\sofia.baltzi\\OneDrive - Accenture\\Desktop\\Statistics for BD\\Group Project - SIS")

# Read data
data = read_excel("alldata together.xls")

##################################        4. Data Preprocessing    ##############################
#                                                                                               #
#                                                                                               #
#                                                                                               #

# Create a gene dictionary for reference
gene_dictionary = data[2:nrow(data),1] %>% 
  dplyr::rename(gene_name = `...1`) %>% 
  dplyr::mutate(column_name = paste0("X", row_number())) %>% 
  data.frame()

gene_dictionary = rbind(c("age","age"), gene_dictionary) 

# Delete unwanted columns
data = data[2:nrow(data),3:ncol(data)]

# Replace character "Nan" with NA
data[data=="NaN"] = NA
data = sapply(data, as.numeric)

# Store the Useful Columns to Be extracted
useful_columns = colnames(data)


# Extract life expectancy (and y), age and erp from data labels
life_expectancy = str_extract(useful_columns, ">5|<5")
y = gsub(">5", 1, life_expectancy)
y = gsub("<5", 0, y)
y = as.numeric(y)

age = as.numeric(sub(".*?age .*?(\\d+).*", "\\1", useful_columns))


# Combine life_expectancy, age, erp and the transposed data
data = t(data) %>% data.frame()
data = cbind(y, age, data)

# Reindex the Dataframe
rownames(data) = 1:nrow(data)

# Count NA values per column (patient)
na_count_patient = rowSums(is.na(data)) %>% data.frame()

# Remove patient 54(10896 missing genes) and patient 53(2397 missing genes).
data = data %>% 
  dplyr::filter(row_number() != 53 & row_number() != 54)

# Count NA values per row (gene)
na_count_gene = colSums(is.na(data)) %>% data.frame()

# Remove genes that are missing on more than 1 patient.
na_count_gene=na_count_gene %>% 
  dplyr::mutate(geneIndex = rownames(na_count_gene)) %>% 
  dplyr::filter(`.` < 1)

genesToKeep = as.vector(na_count_gene$geneIndex)

data = data %>% 
  dplyr::select(tidyselect::all_of(genesToKeep))

# Check if everything is OK
setdiff(colnames(data), genesToKeep)


##################################        5.1 Perform SIS and then fit glm    ###################
#                                                                                               #
#                                                                                               #
#                                                                                               #

###### Apply SIS - N=100 ------------------------------------------------------------------------

# Bring data to desired form
X = data.matrix(data[,2:ncol(data)])
X = scale(X)
Y = data[,1]

# Run SIS
Xsis = SIS::SIS(x=X, y=Y, family="binomial", nsis=100, iter=FALSE, standardize=FALSE)
X = data.frame(X[,Xsis$sis.ix0])

# Fit glm model
model1= glm(Y~., data = X, family = "binomial")
summary(model1)

# Fit cv glm
cvmodel1 = cv.glm(cbind(Y,X), model1, K=5, cost=costfunc)
cvmodel1$delta[2]

# a = gene_dictionary %>% filter(column_name %in% colnames(X))


###### Apply SIS - N=15 -------------------------------------------------------------------------

# Bring data to desired form
X = data.matrix(data[,2:ncol(data)])
X = scale(X)
Y = data[,1]

# Run SIS
Xsis = SIS::SIS(x=X, y=Y, family="binomial", nsis=15, iter=FALSE, standardize=FALSE)
X = data.frame(X[,Xsis$sis.ix0])

# Fit glm model
model2 = glm(Y~., data = X, family = "binomial")
summary(model2)

# Fit cv glm
cvmodel2 = cv.glm(cbind(Y,X), model2, K=5, cost=costfunc)
cvmodel2$delta[2]

# a = gene_dictionary %>% filter(column_name %in% colnames(X))
# (!)Once again, even with less features the model is unstable. Next, try with less features.

###### Apply SIS - N=10 -------------------------------------------------------------------------

# Bring data to desired form
X = data.matrix(data[,2:ncol(data)])
X = scale(X)
Y = data[,1]

# Run SIS
Xsis = SIS::SIS(x=X, y=Y, family="binomial", nsis=10, iter=FALSE, standardize=FALSE)
X = data.frame(X[,Xsis$sis.ix0])

# Fit glm model
model3 = glm(Y~., data = X, family = "binomial")
summary(model3)

cvmodel3 = cv.glm(cbind(Y,X), model3, K=5, cost=costfunc)
cvmodel3$delta[2]

# a = gene_dictionary %>% filter(column_name %in% colnames(X))

# Plot correlation
cormat = round(abs(cor(X)),3)

# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]= NA
  return(cormat)
}
upper_tri = get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat = melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  labs(title = "SIS - m=10")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Absolute Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


##################################        5.2 Perform ISIS and then fit glm    ##################
#                                                                                               #
#                                                                                               #
#                                                                                               #

###### Apply ISIS - N=20 -----------------------------------------------------------------------
# Bring data to desired form
X = data.matrix(data[,2:ncol(data)])
X = scale(X)
Y = data[,1]

# Run ISIS with max N=20
Xisis = SIS::SIS(x=X, y=Y, 
                 family="binomial", 
                 penalty='lasso',
                 nsis=20,
                 iter=TRUE, 
                 standardize=FALSE, 
                 perm=FALSE, 
                 tune="cv",
                 nfolds=5,
                 type.measure="class",
                 seed=666)

# Accuracy using model of ISIS
costfunc(Y,as.numeric(predict(Xisis, X, type="response")))

# Coefficients of model
Xisis$coef.est

# Lambdas of model
Xisis$fit


X = data.frame(X[,Xisis$ix])
a = gene_dictionary %>% filter(column_name %in% colnames(X))

# Plot correlation
cormat = round(abs(cor(X)),3)

# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]= NA
  return(cormat)
}
upper_tri = get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat = melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  labs(title = "ISIS - m=16")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Absolute Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()



###### Apply ISIS - N=10 -----------------------------------------------------------------------
# Bring data to desired form
X = data.matrix(data[,2:ncol(data)])
X = scale(X)
Y = data[,1]

# Run ISIS with max N=10
Xisis2 = SIS::SIS(x=X, y=Y, 
                 family="binomial", 
                 penalty='lasso',
                 nsis=10,
                 iter=TRUE, 
                 standardize=FALSE, 
                 perm=FALSE, 
                 tune="cv",
                 nfolds=5,
                 type.measure="class",
                 seed=666)

# Accuracy using model of ISIS
costfunc(Y,as.numeric(predict(Xisis2, X, type="response")))

# Coefficients of model
Xisis2$coef.est

# Lambdas of model
Xisis2$fit

X = data.frame(X[,Xisis2$ix])
a = gene_dictionary %>% filter(column_name %in% colnames(X))


# Plot correlation
cormat = round(abs(cor(X)),3)

# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]= NA
  return(cormat)
}
upper_tri = get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat = melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  labs(title = "ISIS - m=10")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Absolute Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


