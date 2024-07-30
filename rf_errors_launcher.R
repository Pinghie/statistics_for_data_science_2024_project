#
# Implementation of Chapters 4 and 5.
#
# This script is divided into two parts. In the first part, datasets are selected and prepared.
# In the second part, the prepared data is fed into the error estimation functions defined in rf_errors.R.
# These functions return the error and the out-of-bag error for both constructed forests, and the selection error.
#

library(ODRF)
library(randomForest)

setwd("data")
source("../rf_errors.R")

SEED <- 8
set.seed(SEED)

###       DATASET PREPARATION        ###
### Breast cancer
X_breast <- read.csv("breast_cancer/breast-cancer-wisconsin.data", header = FALSE)
X_breast$V7 <- as.integer(X_breast$V7) 
X_breast$V7[is.na(X_breast$V7)] <- round(mean(X_breast$V7[!(is.na(X_breast$V7))])) # replace the missing values with the mean of that column
y_breast <- as.factor(X_breast$V11) 
X_breast <- X_breast[ ,2:(ncol(X_breast)-1)] # the first column is an id

### Glass
X_glass <- read.csv("glass/glass.data", header = FALSE)
y_glass <- as.factor(X_glass$V11)
X_glass <- X_glass[ , 2:(ncol(X_glass)-1)]

### Sonar
X_sonar <- read.csv("sonar/sonar.all-data", header = FALSE)
y_sonar <- as.factor(X_sonar$V61)
X_sonar <- X_sonar[ , 1:(ncol(X_sonar)-1)]

### Satellite
sat_train <- read.csv("satellite/sat.trn", sep=' ', header = FALSE)
sat_test <- read.csv("satellite/sat.tst", sep=' ', header = FALSE)
sat_train$V37 <- as.factor(sat_train$V37)
sat_test$V37 <- as.factor(sat_test$V37)

### Letter
raw_letter <- read.csv("letter/letter-recognition.data", header = FALSE)
raw_letter$V1 <- as.factor(raw_letter$V1)
letter_train_i <- sample(1:nrow(raw_letter), 0.75 * nrow(raw_letter), replace = FALSE)
letter_train <- raw_letter[letter_train_i, 1:ncol(raw_letter)]
letter_test <- raw_letter[-letter_train_i, 1:ncol(raw_letter)]


########          FOREST RI             ##########

#####   Small datasets   #####

### Breast Cancer
res_breast_ri <- test_randomforest(X_breast, y_breast, F_=1, test_size=0.1, iteration=100, otherF=floor(log2(ncol(X_breast))+1), to_combine=1, fixed_seed = SEED)
print(res_breast_ri)

### Glass
res_glass_ri <- test_randomforest(X_glass, y_glass, F_=1, test_size=0.1, iteration=100, otherF=floor(log2(ncol(X_glass))+1), to_combine = 1, fixed_seed = SEED)
print(res_glass_ri)

### Sonar
res_sonar_ri <- test_randomforest(X_sonar, y_sonar, F_=1, test_size=0.1, iteration=100, otherF=floor(log2(ncol(X_sonar))+1), to_combine=1, fixed_seed = SEED)
print(res_sonar_ri)

#####   Large datasets   #####

### Satellite
res_sat_ri <- test_randomforest_large(sat_train, sat_test, sat_train$V37 ~ ., sat_test$V37, 0)
print(res_sat_ri)

### Letter
res_letter_ri <- test_randomforest_large(letter_train, letter_test, letter_train$V1 ~ ., letter_test$V1, 0)
print(res_letter_ri)


########          FOREST RC           ##########

#####   Small datasets   #####

### Breast Cancer
res_breast_rc <- test_randomforest(X_breast, y_breast, F_=2, test_size=0.1, iteration=100, otherF=8, to_combine=3, fixed_seed = SEED)
print(res_breast_rc)

### Glass
res_glass_rc <- test_randomforest(X_glass, y_glass, F_=2, test_size=0.1, iteration=100, otherF=8, to_combine=3, fixed_seed = SEED)
print(res_glass_rc)

### Sonar
res_sonar_rc <- test_randomforest(X_sonar, y_sonar, F_=2, test_size=0.1, iteration=100, otherF=8, to_combine=3,  fixed_seed = SEED)
print(res_sonar_rc)

#####   Large datasets   #####

### Satellite
res_sat_rc <- test_randomforest_large(sat_train, sat_test[ , 1:(ncol(sat_test)-1)], sat_train$V37 ~ ., sat_test$V37, 1)
print(res_sat_rc)

### Letter
res_letter_rc <- test_randomforest_large(letter_train, letter_test[ , 2:ncol(letter_train)], letter_train$V1 ~ ., letter_test$V1, 1)
print(res_letter_rc)
