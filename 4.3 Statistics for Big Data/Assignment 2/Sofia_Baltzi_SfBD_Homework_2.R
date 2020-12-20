##################################    1. Libraries    ########################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

rm(list = ls())
options(scipen=10000)
set.seed(666)

if ("dplyr" %in% rownames(installed.packages()) == F){
  install.packages("dplyr")}
require(dplyr)
if ("readxl" %in% rownames(installed.packages()) == F){
  install.packages("readxl")}
require(readxl)
if ("InformationValue" %in% rownames(installed.packages()) == F){
  install.packages("InformationValue")}
require(InformationValue)
if ("Metrics" %in% rownames(installed.packages()) == F){
  install.packages("Metrics")}
require(Metrics)
if ("fastDummies" %in% rownames(installed.packages()) == F){
  install.packages("fastDummies")}
require(fastDummies)
if ("data.table" %in% rownames(installed.packages()) == F){
  install.packages("data.table")}
require(data.table)
if ("splitTools" %in% rownames(installed.packages()) == F){
  install.packages("splitTools")}
require(splitTools)

##################################    2. Load Data    ########################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

# Set working directory
setwd("C:/Users/sofia.baltzi/OneDrive - Accenture/Desktop/Statistics for BD/Assignment 2/")

# Read data
data = read_excel("project 2 stat for big data.xls")
data = data %>% 
  dplyr::select(job,
                marital,
                education,
                previous,
                month,
                cons.price.idx,
                cons.conf.idx,
                euribor3m,
                nr.employed,
                SUBSCRIBED) %>%
  dplyr::distinct() %>% 
  data.frame()

any(duplicated(data))

##################################    3. Data Manipulation    ################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

# Check for missing in pedendent/y variable
any(is.na(data$SUBSCRIBED))

# Make y variable 0/1
data = data %>% 
  dplyr::mutate(SUBSCRIBED = dplyr::case_when(SUBSCRIBED=="yes"~1,
                                              SUBSCRIBED=="no"~0)) %>% 
  fastDummies::dummy_cols(., select_columns = c("job",
                                                "marital",
                                                "education",
                                                "month")) %>%
  dplyr::select(-job,
                -job_unknown,
                -marital,
                -marital_unknown,
                -education,
                -education_unknown,
                -month,
                -month_sep) %>%
  data.frame()

# Split data to train/test
# Shuffle data
rows = sample(nrow(data))
data = data[rows,]

# Create training/test set
data = data %>% 
  dplyr::mutate(ID = row_number()) %>% 
  data.frame()

train = data %>%
  dplyr::sample_frac(.80) %>% 
  data.frame()

test = anti_join(data, train, by = 'ID')

data = data %>% 
  dplyr::select(-ID) %>% 
  data.frame()

train = train %>% 
  dplyr::select(-ID) %>% 
  data.frame()

test = test %>% 
  dplyr::select(-ID) %>% 
  data.frame()

# Class imbalance check
round(sum(data$SUBSCRIBED)/nrow(data),2)
round(1-sum(data$SUBSCRIBED)/nrow(data),2)

round(sum(train$SUBSCRIBED)/nrow(train),2)
round(1-sum(train$SUBSCRIBED)/nrow(train),2)

round(sum(test$SUBSCRIBED)/nrow(test),2)
round(1-sum(test$SUBSCRIBED)/nrow(test),2)

##################################    4. Model    ############################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

model = glm(SUBSCRIBED ~ previous + cons.price.idx + cons.conf.idx + euribor3m
                           + nr.employed + job_admin.
                           + job_blue.collar + job_entrepreneur + job_housemaid + job_management
                           + job_retired + job_self.employed + job_services + job_student
                           + job_technician + job_unemployed + marital_divorced + marital_married
                           + marital_single + education_basic.4y + education_basic.6y + education_basic.9y
                           + education_high.school + education_illiterate + education_professional.course + education_university.degree
                           + month_apr + month_aug + month_dec + month_jul
                           + month_jun + month_mar + month_may + month_nov
                           + month_oct, 
             data = train, family = "binomial")

coefs_all = as.data.frame(t(as.matrix(data.frame(coef(model)))))
rownames(coefs_all) = "coefficients"
coefs_all1 = t(coefs_all) %>% data.frame()

predicted = data.frame(predict(model, test, type="response"))
colnames(predicted) = "prediction_probability"

predicted = predicted %>% 
  dplyr::mutate(prediction = dplyr::case_when(prediction_probability>=0.5 ~ 1,
                                              TRUE ~ 0),
                true = test$SUBSCRIBED) %>% 
  data.frame()

# Metrics
true = predicted$true
prediction = predicted$prediction
prob = predicted$prediction_probability

print(paste("Accuracy: ",accuracy(true, prediction)))
print("Confusion Matrix:")
confusionMatrix(true, prediction)

print(paste("Area Under Curve: ",auc(true, prediction)))

precision = precision(true, prediction)
recall = recall(true, prediction)
F1 = 2 * (precision * recall) / (precision + recall)
print(paste("F1 score: ",F1))

##################################    5. Model (10 splits)   #################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

splits = create_folds(train$SUBSCRIBED, k = 10)
coefs10 = data.frame()
for (split in splits) {
  data_split = train[split,]
  model = glm(SUBSCRIBED ~ previous + cons.price.idx + cons.conf.idx + euribor3m
             + nr.employed + job_admin.
             + job_blue.collar + job_entrepreneur + job_housemaid + job_management
             + job_retired + job_self.employed + job_services + job_student
             + job_technician + job_unemployed + marital_divorced + marital_married
             + marital_single + education_basic.4y + education_basic.6y + education_basic.9y
             + education_high.school + education_illiterate + education_professional.course 
             + education_university.degree + month_apr + month_aug + month_dec + month_jul
             + month_jun + month_mar + month_may + month_nov + month_oct,
             data = data_split, family = 'binomial')
  
  coefs_i  = as.data.frame(t(as.matrix(data.frame(coef(model)))))
  coefs10 = rbind(coefs10, coefs_i)
}

avg_coefs_10 = data.frame(colMeans(coefs10, na.rm=TRUE))
values = avg_coefs_10$colMeans.coefs10..na.rm...TRUE.
names(values) = rownames(avg_coefs_10)

diffs10 = data.frame(abs(coefs_all1-avg_coefs_10))

##################################    5.1 Model (10 splits) - Custom weights   ###############
#                                                                                            #
#                                                                                            #
#                                                                                            #

model$coefficients = values

predicted = data.frame(predict(model, test, type="response"))
colnames(predicted) = "prediction_probability"

predicted = predicted %>% 
  dplyr::mutate(prediction = dplyr::case_when(prediction_probability>=0.5 ~ 1,
                                              TRUE ~ 0),
                true = test$SUBSCRIBED) %>% 
  data.frame()

# Metrics
true = predicted$true
prediction = predicted$prediction
prob = predicted$prediction_probability

print(paste("Accuracy: ",accuracy(true, prediction)))
print("Confusion Matrix:")
confusionMatrix(true, prediction)

print(paste("Area Under Curve: ",auc(true, prediction)))

precision = precision(true, prediction)
recall = recall(true, prediction)
F1 = 2 * (precision * recall) / (precision + recall)
print(paste("F1 score: ",F1))

##################################    6. Model (20 splits)   #################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

splits = create_folds(train$SUBSCRIBED, k = 20)
coefs20 = data.frame()
for (split in splits) {
  data_split = train[split,]
  model = glm(SUBSCRIBED ~ previous + cons.price.idx + cons.conf.idx + euribor3m
              + nr.employed + job_admin.
              + job_blue.collar + job_entrepreneur + job_housemaid + job_management
              + job_retired + job_self.employed + job_services + job_student
              + job_technician + job_unemployed + marital_divorced + marital_married
              + marital_single + education_basic.4y + education_basic.6y + education_basic.9y
              + education_high.school + education_illiterate + education_professional.course 
              + education_university.degree + month_apr + month_aug + month_dec + month_jul
              + month_jun + month_mar + month_may + month_nov + month_oct,
              data = data_split, family = 'binomial')
  
  coefs_i  = as.data.frame(t(as.matrix(data.frame(coef(model)))))
  coefs20 = rbind(coefs20, coefs_i)
}


avg_coefs_20 = data.frame(colMeans(coefs20, na.rm=TRUE))
values = avg_coefs_20$colMeans.coefs20..na.rm...TRUE.
names(values) = rownames(avg_coefs_20)

diffs20 = data.frame(abs(coefs_all1-avg_coefs_20))

##################################    6.1 Model (20 splits) - Custom weights   ###############
#                                                                                            #
#                                                                                            #
#                                                                                            #

model$coefficients = values

predicted = data.frame(predict(model, test, type="response"))
colnames(predicted) = "prediction_probability"

predicted = predicted %>% 
  dplyr::mutate(prediction = dplyr::case_when(prediction_probability>=0.5 ~ 1,
                                              TRUE ~ 0),
                true = test$SUBSCRIBED) %>% 
  dplyr::select(-prediction_probability) %>% 
  data.frame()

# Metrics
true = predicted$true
prediction = predicted$prediction
prob = predicted$prediction_probability

print(paste("Accuracy: ",accuracy(true, prediction)))
print("Confusion Matrix:")
confusionMatrix(true, prediction)

print(paste("Area Under Curve: ",auc(true, prediction)))

precision = precision(true, prediction)
recall = recall(true, prediction)
F1 = 2 * (precision * recall) / (precision + recall)
print(paste("F1 score: ",F1))
