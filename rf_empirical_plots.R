#
# Implementation of Chapter 6.
#
# For each of the three datasets, this script apply the function forest_test() defined in rf_empirical_results.R.
# Two plots are drawn for each dataset.
#

library(ODRF)
library(randomForest)

setwd('data')
source("../rf_empirical_results.R")
source("../rf_errors.R")

############SONAR DATA##################
X_sonar <- read.csv("sonar/sonar.all-data", header = FALSE)
y_sonar <- as.factor(X_sonar$V61)
X_sonar <- X_sonar[ , 1:(ncol(X_sonar)-1)]
levels(y_sonar) <- 1:length(levels(y_sonar))

sonar_results = forest_test(X_sonar, y_sonar, maxF=50, to_combine=1, test_size=0.1, iteration=80, fixed_seed = 8, verbose=0)

###Plot FIGURA 1
#Grafico 1
plot(1:50, sonar_results$strength_list, type="b", pch=16, col="blue", xlab="number of features", ylab="Y Variables", main="CORRELATION AND STRENGTH", cex.lab=1.4, cex.axis=1.2, ylim=c(0, 0.6))
lines(1:50, sonar_results$correlation, type="b", pch=15, col="red") 
legend("bottomright", legend=c("strength", "correlation"), col=c("blue", "red"), pch=c(16, 15), cex=0.8)

#Grafico 2
plot(1:50, sonar_results$err_list, type="b", pch=16, col="blue", xlab="number of features", ylab="percent", main="TEST SET AND OB ERROR", cex.lab=1.4, cex.axis=1.2, ylim=c(0, 25))
lines(1:50, sonar_results$oob_err_list, type="b", pch=15, col="red") 
legend("bottomright", legend=c("test set error", "OB error"), col=c("blue", "red"), pch=c(16, 15), cex=0.8)



#############BREAST DATA##################
X_breast <- read.csv("breast_cancer/breast-cancer-wisconsin.data", header = FALSE)
X_breast$V7 <- as.integer(X_breast$V7) #trasformo la riga 11 da stringhe a interi
y_breast <- as.factor(X_breast$V11)
X_breast$V7[is.na(X_breast$V7)] <- round(mean(X_breast$V7[!(is.na(X_breast$V7))])) # sostituisco i missing values con la media di quella colonna
X_breast <- X_breast[ ,2:(ncol(X_breast)-1)] #rimuovo la colonna delle classi dal training set
levels(y_breast) <- 1:length(levels(y_breast))

# with high number of "iterations", execution can take several hours
breast_results = forest_test(X_breast, y_breast, maxF=25, to_combine=3, test_size=0.1, iteration=80, fixed_seed = 8, verbose=1)

###Plot FIGURA 2
#Grafico 1
plot(1:25, breast_results$strength_list, type="b", pch=16, col="blue", xlab="number of features", ylab="Y Variables", main="CORRELATION AND STRENGTH", cex.lab=1.4, cex.axis=1.2, ylim=c(0, 1.2))
lines(1:25, breast_results$correlation, type="b", pch=15, col="red") 
legend("bottomright", legend=c("strength", "correlation"), col=c("blue", "red"), pch=c(16, 15), cex=0.8)

#Grafico 2
plot(1:25, breast_results$err_list, type="b", pch=16, col="blue", xlab="number of features", ylab="percent", main="TEST SET AND OB ERROR", cex.lab=1.4, cex.axis=1.2, ylim=c(0, 5))
lines(1:25, breast_results$oob_err_list, type="b", pch=15, col="red") 
legend("bottomright", legend=c("test set error", "OB error"), col=c("blue", "red"), pch=c(16, 15), cex=0.8)



############SATELLITE DATA##################
sat_X_train <- read.csv("satellite/sat.trn", sep=' ', header = FALSE)
sat_y_train <- as.factor(sat_X_train$V37)
sat_X_train <- sat_X_train[ , 1:(ncol(sat_X_train)-1)]
levels(sat_y_train) <- 1:length(levels(sat_y_train))

# with high number of "iterations", execution can take several hours
sat_results = forest_test(sat_X_train, sat_y_train, maxF=25, to_combine=2, test_size=0.1, iteration=30, fixed_seed = 8, verbose=1)

###Plot FIGURA 3
#Grafico 1
plot(1:25, sat_results$strength_list, type="b", pch=16, col="blue", xlab="number of features", ylab="Y Variables", main="CORRELATION AND STRENGTH", cex.lab=1.4, cex.axis=1.2, ylim=c(0, 1))
lines(1:25, sat_results$correlation, type="b", pch=15, col="red") 
legend("bottomright", legend=c("strength", "correlation"), col=c("blue", "red"), pch=c(16, 15), cex=0.8)

#Grafico 2
plot(1:25, sat_results$err_list, type="b", pch=16, col="blue", xlab="number of features", ylab="percent", main="TEST SET AND OB ERROR", cex.lab=1.4, cex.axis=1.2, ylim=c(0, 15))
lines(1:25, sat_results$oob_err_list, type="b", pch=15, col="red") 
legend("bottomright", legend=c("test set error", "OB error"), col=c("blue", "red"), pch=c(16, 15), cex=0.8)
